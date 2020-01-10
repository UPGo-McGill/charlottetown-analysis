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


### Save files #################################################################

save(city, daily, DAs, DAs_PEI, file = "data/charlottetown.Rdata")