#### CHARLOTTETOWN IMPORT ######################################################

### Load libraries #############################################################

library(tidyverse)
library(upgo)
library(strr)
library(future)
library(sf)
library(osmdata)


### Set global variables #######################################################

plan(multiprocess)

end_date <- as.Date("2019-11-30")

exchange_rate <- 
  map_dbl(0:11, ~{ 
    ex_table <- 
      fixerapi::fixer_historical(
        date = (end_date %m-% months(.x)), symbols = c("CAD", "USD"))
    ex_table[1,]$value / ex_table[2,]$value
  }) %>% mean()

### Build geometries ###########################################################

## Charlottetown CSD

city <-
  cancensus::get_census(dataset = "CA16", regions = list(CSD = "1102075"),
                        level = "CSD", geo_format = "sf") %>% 
  st_transform(32620) %>% 
  st_set_agr("constant")

## Charlottetown streets

streets <- 
  (getbb("Charlottetown Prince Edward Island") * c(1.01, 0.99, 0.99, 1.01)) %>% 
  opq(timeout = 200) %>% 
  add_osm_feature(key = "highway") %>% 
  osmdata_sf()

streets <-
  rbind(
    streets$osm_polygons %>% st_set_agr("constant") %>% st_cast("LINESTRING"), 
    streets$osm_lines) %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  st_transform(32620) %>%
  st_set_agr("constant") %>%
  st_intersection(city) %>% 
  select(osm_id, name, geometry)


### Census import ##############################################################

DAs_PEI <-
  cancensus::get_census(
    dataset = "CA16", regions = list(PR = "11"), level = "DA",
    vectors = c("v_CA16_2398", "v_CA16_5078", "v_CA16_4888", "v_CA16_6695",
                "v_CA16_4837", "v_CA16_4838", "v_CA16_512", 
                "v_CA16_3393", "v_CA16_3996"),
    geo_format = "sf") %>% 
  st_transform(32620) %>% 
  select(GeoUID, CSD_UID, Population, Dwellings, contains("v_CA")) %>% 
  set_names(c("Geo_UID", "CSD_UID", "population", "dwellings", "med_income", 
              "university_education", "housing_need", "non_mover",
              "owner_occupier", "rental", "official_language", "citizen",
              "white", "geometry")) %>% 
  st_set_agr("constant") %>% 
  mutate_at(
    .vars = c("university_education", "non_mover", 
              "official_language", "citizen", "white"),
    .funs = list(`pct_pop` = ~{. / population})) %>% 
  mutate_at(
    .vars = c("housing_need", "owner_occupier", "rental"),
    .funs = list(`pct_dwellings` = ~{. / dwellings}))

DAs <-
  cancensus::get_census(
    dataset = "CA16", regions = list(CSD = "1102075"), level = "DA",
    vectors = c("v_CA16_2398", "v_CA16_5078", "v_CA16_4888", "v_CA16_6695",
                "v_CA16_4837", "v_CA16_4838", "v_CA16_512", 
                "v_CA16_3393", "v_CA16_3996"),
    geo_format = "sf") %>% 
  st_transform(32620) %>% 
  select(GeoUID, CSD_UID, Population, Dwellings, contains("v_CA")) %>% 
  set_names(c("GeoUID", "CMA_UID", "population", "dwellings", "med_income",
    "university_education", "housing_need", "non_mover", "owner_occupier", 
    "rental", "official_language", "citizen", "white", "geometry")) %>% 
  st_set_agr("constant") %>% 
  mutate_at(
    .vars = c("university_education", "non_mover", 
              "official_language", "citizen", "white"),
    .funs = list(`pct_pop` = ~{. / population})) %>% 
  mutate_at(
    .vars = c("housing_need", "owner_occupier", "rental"),
    .funs = list(`pct_dwellings` = ~{. / dwellings}))


### Import STR data ############################################################

upgo_connect()

property <- 
  property_all %>% 
  filter(country == "Canada", city == "Charlottetown") %>% 
  collect() %>% 
  filter(!is.na(listing_type)) %>% 
  select(property_ID:longitude, ab_property:ha_host, bedrooms) %>% 
  strr_as_sf(32620)

daily <- 
  daily_all %>% 
  filter(property_ID %in% !! property$property_ID) %>% 
  collect() %>% 
  strr_expand() %>%
  filter(date >= created, date - 30 <= scraped, status != "U")

ML_property <- 
  property_all %>% 
  filter(host_ID %in% !! property$host_ID) %>% 
  collect()

ML_daily <- 
  daily_all %>% 
  filter(property_ID %in% !! ML_property$property_ID) %>% 
  collect() %>% 
  strr_expand() %>% 
  filter(date >= created, date - 30 <= scraped, status != "U")

upgo_disconnect()

### Process the property file ##################################################

## Run the raffle to assign a DA to each listing

property <- 
  property %>% 
  strr_raffle(DAs, GeoUID, dwellings) 


## Add last twelve months revenue

property <-
  daily %>% 
  filter(date > (end_date - lubridate::years(1)), status == "R") %>% 
  group_by(property_ID) %>% 
  summarize(revenue_LTM = sum(price) * exchange_rate) %>% 
  select(property_ID, revenue_LTM) %>% 
  left_join(property, .) %>% 
  select(-geometry, everything(), geometry)


## Create last twelve months property file

LTM_property <- 
  property %>% 
  filter(created <= end_date, scraped > (end_date - lubridate::years(1)))


### Process multilistings ######################################################

EH_ML <- 
  ML_daily %>% 
  filter(listing_type == "Entire home/apt") %>% 
  group_by(listing_type, host_ID, date) %>% 
  count() %>% 
  ungroup() %>% 
  filter(n >= 2) %>% 
  mutate(ML = TRUE)

PR_ML <- 
  ML_daily %>% 
  filter(listing_type == "Private room") %>% 
  group_by(listing_type, host_ID, date) %>% 
  count() %>% 
  ungroup() %>% 
  filter(n >= 3) %>% 
  mutate(PR_ML = TRUE)

daily <- 
  EH_ML %>% 
  select(-n) %>% 
  left_join(daily, .)

daily <- 
  PR_ML %>% 
  select(-n) %>% 
  left_join(daily, .) %>% 
  mutate(ML = if_else(is.na(ML), PR_ML, ML)) %>% 
  mutate(ML = if_else(is.na(ML), FALSE, ML)) %>% 
  select(-PR_ML)

rm(EH_ML, PR_ML)


### Calculate FREH and GH listings #############################################

FREH <- 
  daily %>% 
  strr_FREH("2017-01-01", end_date) %>%
  filter(FREH == TRUE) %>% 
  select(-FREH)

GH <- 
  property %>% 
  strr_ghost(start_date = "2017-01-01", end_date = end_date)


### Add principal residence fields #############################################

## Experimental principal residence function

strr_principal_residence <- 
  function(property, daily, FREH, GH, start_date, end_date, 
           field_name = principal_residence) {
  
  start_date <- as.Date(start_date, origin = "1970-01-01")
  end_date <- as.Date(end_date, origin = "1970-01-01")
  
  pr_table <- tibble(property_ID = property$property_ID,
                     listing_type = property$listing_type,
                     host_ID = property$host_ID,
                     housing = property$housing)
  
  pr_ML <- 
    daily %>% 
    group_by(property_ID) %>% 
    summarize(ML = if_else(
      sum(ML * (date >= start_date)) + sum(ML * (date <= end_date)) > 0, 
      TRUE, FALSE))
  
  pr_n <-
    daily %>%  
    filter(status %in% c("R", "A"), date >= start_date, date <= end_date) %>% 
    count(property_ID, status) %>% 
    group_by(property_ID) %>% 
    summarize(n_available = sum(n),
              n_reserved = sum(n[status == "R"]))
  
  pr_table <- 
    pr_table %>% 
    left_join(pr_ML, by = "property_ID") %>% 
    mutate(ML = if_else(is.na(ML), FALSE, ML)) %>% 
    left_join(pr_n, by = "property_ID") %>% 
    group_by(host_ID, listing_type) %>% 
    mutate(LFRML = case_when(
      listing_type != "Entire home/apt" ~ FALSE,
      ML == FALSE                       ~ FALSE,
      n_available == min(n_available)   ~ TRUE,
      TRUE                              ~ FALSE)) %>% 
    ungroup()
  
  pr_table <- 
    pr_table %>%
    filter(LFRML == TRUE) %>%
    group_by(host_ID) %>%
    mutate(prob = sample(0:10000, n(), replace = TRUE),
           LFRML = if_else(
             sum(LFRML) > 1 & prob != max(prob), FALSE, LFRML)) %>%
    ungroup() %>% 
    select(property_ID, LFRML2 = LFRML) %>% 
    left_join(pr_table, ., by = "property_ID") %>% 
    mutate(LFRML = if_else(!is.na(LFRML2), LFRML2, LFRML)) %>% 
    select(-LFRML2)
  
  GH_list <-
    GH %>% 
    filter(date >= start_date, date <= end_date) %>% 
    pull(property_IDs) %>%
    unlist() %>%
    unique()
  
  pr_table <-
    pr_table %>% 
    mutate(GH = if_else(property_ID %in% GH_list, TRUE, FALSE))
  
  pr_table <-
    FREH %>% 
    filter(date >= start_date, date <= end_date) %>% 
    group_by(property_ID) %>% 
    summarize(FREH = TRUE) %>% 
    left_join(pr_table, ., by = "property_ID") %>% 
    mutate(FREH = if_else(is.na(FREH), FALSE, FREH))
  
  # Add principal_res field
  
  pr_table <- 
    pr_table %>% 
    mutate({{ field_name }} := case_when(
      housing == FALSE               ~ FALSE,
      GH == TRUE                     ~ FALSE,
      listing_type == "Shared room"  ~ TRUE,
      listing_type == "Private room" ~ TRUE,
      FREH == TRUE                   ~ FALSE,
      LFRML == TRUE                  ~ TRUE,
      ML == TRUE                     ~ FALSE,
      TRUE                           ~ TRUE)) %>% 
    select(property_ID, {{ field_name }})
  
  left_join(property, pr_table, by = "property_ID")

}

property <- 
  property %>% 
  strr_principal_residence(daily, FREH, GH, "2019-04-01", "2019-09-30", 
                           principal_res_2019) %>% 
  strr_principal_residence(daily, FREH, GH, "2018-04-01", "2018-09-30", 
                           principal_res_2018) %>% 
  strr_principal_residence(daily, FREH, GH, "2017-04-01", "2017-09-30", 
                           principal_res_2017)


### Add seasonal fields ########################################################

season_start <- as.Date("2019-04-01")
season_end <- as.Date("2019-09-30")

property <-
  daily %>%
  filter(listing_type == "Entire home/apt", status %in% c("R", "A")) %>% 
  group_by(property_ID) %>%
  summarize(
    n_ar_2019 = sum(between(date, season_start, season_end)),
    n_r_2019  = sum(between(date[status == "R"], season_start, season_end)),
    n_ar_2018 = sum(between(date, 
                            season_start - years(1), 
                            season_end - years(1))),
    n_r_2018  = sum(between(date[status == "R"], 
                            season_start - years(1), 
                            season_end - years(1))),
    n_ar_2017 = sum(between(date, 
                            season_start - years(2),
                            season_end - years(2))),
    n_r_2017  = sum(between(date[status == "R"],
                            season_start - years(2),
                            season_end - years(2))),
    seasonal_2019 = if_else(n_ar_2019 >= 120, n_r_2019 >= 90, TRUE, FALSE),
    seasonal_2018 = if_else(n_ar_2018 >= 120, n_r_2018 >= 90, TRUE, FALSE),
    seasonal_2017 = if_else(n_ar_2017 >= 120, n_r_2017 >= 90, TRUE, FALSE)
    ) %>% 
    select(property_ID, seasonal_2019:seasonal_2017) %>% 
    left_join(property, ., by = "property_ID")
  
property <-
  property %>% 
  select(-geometry, everything(), geometry)


### Save files #################################################################

save(city, daily, DAs, DAs_PEI, FREH, GH, LTM_property, ML_daily, ML_property,
     property, streets, end_date, exchange_rate, 
     file = "data/charlottetown.Rdata")