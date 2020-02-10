#### CHARLOTTETOWN IMPORT ######################################################

### Load libraries #############################################################

library(tidyverse)
library(upgo)
library(strr)
library(future)
library(sf)
library(osmdata)
library(lubridate)
library(readxl)


### Set global variables #######################################################

plan(multiprocess)

end_date <- as.Date("2019-12-31")

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

DAs <-
  cancensus::get_census(
    dataset = "CA16", regions = list(CSD = "1102075"), level = "DA",
    geo_format = "sf") %>% 
  st_transform(32620) %>% 
  select(GeoUID, CSD_UID, Population, Dwellings) %>% 
  set_names(c("GeoUID", "CMA_UID", "population", "dwellings")) %>% 
  st_set_agr("constant")


### Import data from city ######################################################

## Import wards, zoning and parcels

wards <-
  read_sf("data/shapefiles/Charlottetown_Wards_2018.shp") %>% 
  select(ward = WARDNAME) %>% 
  st_set_crs(2954) %>% 
  st_transform(32620)

# Add population data from the census
wards <-
  st_interpolate_aw(select(DAs, dwellings), wards, extensive = TRUE) %>% 
  pull(dwellings) %>% 
  mutate(wards, dwellings = .) %>% 
  select(ward, dwellings, geometry)

zones <- 
  read_sf("data/shapefiles/Zoning.shp") %>% 
  st_set_crs(2954) %>% 
  st_transform(32620)

parcels <- 
  read_sf("data/shapefiles/Property.shp") %>% 
  st_set_crs(2954) %>% 
  st_transform(32620) %>% 
  filter(!is.na(st_is_valid(geometry)))


## Import host data

host <-
  read_xlsx("data/host.xlsx") %>% 
  set_names(c("parcel_number", "compliance_status", "advertised", "STR",
              "identification_status", "first_activity", "first_identified",
              "last_posted", "mailability_status", "address",
              "unit_number", "owner_name", "listings", "last_stay",
              "registration_numbers", "max_sleeping_capacity",
              "max_bedrooms", "property_types", "rental_unit_record",
              "most_recent_comment")) %>% 
  select(parcel_number, STR, listings, address, unit_number, compliance_status,
         identification_status, advertised, registration_numbers, 
         property_types) %>% 
  mutate(advertised = if_else(advertised == "Yes", TRUE, FALSE),
         STR = if_else(STR == "Yes", TRUE, FALSE),
  ) %>% 
  mutate(listings = 
           str_split(listings, '"') %>% 
           map(str_extract_all, "(?<=www.).*?(?=<)") %>% 
           map(unlist)) %>% 
  unnest(listings) %>% 
  mutate(property_ID = case_when(
    str_detect(listings, "airbnb") ~ 
      paste0("ab-", str_extract(listings, "(?<=rooms/).*")),
    str_detect(listings, "homeaway") ~ 
      paste0("ha-", str_extract(listings, "(?<=rental/p).*")),
    str_detect(listings, "vrbo") ~ 
      paste0("ha-", str_extract(listings, "(?<=vrbo.com/).*"), "vb"),
    TRUE ~ NA_character_
  )) %>% 
  select(property_ID, everything()) %>% 
  select(-listings) %>% 
  distinct() %>% 
  mutate(
    street_number = str_extract(address, "[:digit:]"),
    street_name = str_extract(address, "(?<=[:digit:][:blank:]).*(?=, Charlottetown)"),
    street_type = str_extract(street_name, "(\\w+)$"),
    street_name = str_extract(street_name, ".*(?=\\s)"),
    street_type = case_when(
      street_type == "Rd"   ~ "Road",
      street_type == "St"   ~ "Street",
      street_type == "Dr"   ~ "Drive",
      street_type == "Ave"  ~ "Avenue",
      street_type == "Ln"   ~ "Lane",
      street_type == "Ct"   ~ "Crescent",
      street_type == "Cir"  ~ "Circle",
      street_type == "Pkwy" ~ "Parkway",
      street_type == "Pl"   ~ "Place",
      TRUE                  ~ street_type
    )
  ) %>% 
  ggmap::mutate_geocode(address)


### Import STR data ############################################################

upgo_connect()

property <- 
  property_all %>% 
  filter(country == "Canada", city == "Charlottetown") %>% 
  collect() %>% 
  filter(!is.na(listing_type)) %>% 
  select(property_ID:longitude, ab_property:ha_host, bedrooms) %>% 
  # Overwrite lat/long where exact coordinates are known
  left_join(select(host, property_ID, parcel_number, lon, lat)) %>% 
  mutate(longitude = if_else(!is.na(lon), lon, longitude),
         latitude = if_else(!is.na(lat), lat, latitude),
         exact_address = if_else(!is.na(lon), TRUE, FALSE)
  ) %>% 
  select(-lon, -lat) %>% 
  strr_as_sf(32620)

daily <- 
  daily_all %>% 
  filter(property_ID %in% !! property$property_ID) %>% 
  collect() %>% 
  strr_expand() %>%
  filter(date >= created, date <= scraped, status != "U")

ML_property <- 
  property_all %>% 
  filter(host_ID %in% !! property$host_ID) %>% 
  collect()

ML_daily <- 
  daily_all %>% 
  filter(property_ID %in% !! ML_property$property_ID) %>% 
  collect() %>% 
  strr_expand() %>% 
  filter(date >= created, date <= scraped, status != "U")

upgo_disconnect()


### Process the property file ##################################################

## Use parcel ID & exact lat/long to assign listings to DAs/zones/parcels/wards

property_exact <- 
  property %>% 
  filter(exact_address)

property_parcel <-
  parcels %>% 
  st_drop_geometry() %>% 
  group_by(PID) %>% 
  select(parcel_number = PID, parcel_units = TOT_FAMILY) %>% 
  summarise_all(first) %>% 
  inner_join(property_exact, .) %>% 
  select(-geometry, everything(), geometry)

property_join <- 
  parcels %>% 
  select(parcel_units = TOT_FAMILY) %>% 
  st_intersection(
    filter(property_exact, !property_ID %in% property_parcel$property_ID), .)

property_ptype <- 
  property %>% 
  filter(!property_ID %in% property_parcel$property_ID,
         !property_ID %in% property_join$property_ID) %>% 
  mutate(parcel_units = case_when(
    property_type %in% c("Condo", "Condominium", "Loft") ~ 1000,
    property_type %in% c("Bungalow", "Cottage", "Entire house",
                         "Entire vacation home", "Farm stay", "Guesthouse",
                         "House", "Private room in house",
                         "Private room in villa", "Villa", "Townhouse",
                         "Townhome", "Entire townhouse", 
                         "Private room in townhouse") ~ 1
  )) %>% 
  filter(!is.na(parcel_units)) %>% 
  select(-geometry, everything(), geometry)
  
property_remain <- 
  property %>% 
  filter(!property_ID %in% property_parcel$property_ID,
         !property_ID %in% property_join$property_ID,
         !property_ID %in% property_ptype$property_ID) %>% 
  strr_raffle(mutate(parcels, TOT_FAMILY2 = as.integer(TOT_FAMILY)), TOT_FAMILY, 
              TOT_FAMILY2, seed = 1) %>% 
  mutate(parcel_units = as.integer(TOT_FAMILY)) %>% 
  select(-geometry, everything(), geometry, -TOT_FAMILY)

property <-
  data.table::rbindlist(list(
    property_parcel, property_join, property_ptype, property_remain)) %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  arrange(property_ID)

property <- 
  property %>% 
  mutate(apartment = if_else(parcel_units > 2, TRUE, FALSE)) %>% 
  mutate(apartment = if_else(is.na(apartment), FALSE, apartment)) %>% 
  mutate(apartment = if_else(property_type %in% c("Condo", "Condominium"), TRUE,
                             apartment))

rm(property_exact, property_parcel, property_join, property_ptype, 
   property_remain)


## Add ward, commercial and DMUN fields

property <- 
  property %>% 
  st_join(select(wards, ward))

property <- 
  property %>% 
  st_join(zones)

hotel_zones <- 
  c("C2", "C3", "MUVC", "DMU", "DMS", "DC", "M3", "WF", "MUC", "A", "PZ")

property <- 
  property %>% 
  mutate(commercial = if_else(ZONING %in% hotel_zones, TRUE, FALSE, FALSE),
         DMUN = if_else(ZONING == "DMUN", TRUE, FALSE, FALSE))

property <- 
  property %>% 
  select(-geometry, everything()) %>% 
  rename(zone = ZONING)

rm(hotel_zones)


## Spatial join or run the raffle to assign a DA to each listing

property_exact <- 
  property %>% 
  filter(exact_address == TRUE) %>% 
  st_intersection(select(DAs, GeoUID))

property_raffle <-
  property %>% 
  filter(!property_ID %in% property_exact$property_ID) %>% 
  strr_raffle(DAs, GeoUID, dwellings, seed = 1) 

property <-
  data.table::rbindlist(list(
    property_exact, property_raffle)) %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  arrange(property_ID)

rm(property_exact, property_raffle)


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
           field_name = principal_residence, sensitivity = 0.1) {
  
  start_date <- as.Date(start_date, origin = "1970-01-01")
  end_date <- as.Date(end_date, origin = "1970-01-01")
  
  sens_n <- 
    round(sensitivity * as.integer((end_date - start_date + 1)))
  
  pr_table <- tibble(property_ID = property$property_ID,
                     listing_type = property$listing_type,
                     host_ID = property$host_ID,
                     zone = property$zone,
                     apartment = property$apartment,
                     housing = property$housing)
  
  pr_ML <- 
    daily %>% 
    group_by(property_ID) %>% 
    summarize(ML = if_else(
      sum(ML * (date >= start_date)) + sum(ML * (date <= end_date)) >= sens_n, 
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
    tibble(property_ID = .) %>% 
    group_by(property_ID) %>% 
    filter(n() >= sens_n) %>% 
    ungroup() %>% 
    pull(property_ID) %>% 
    unique()
  
  pr_table <-
    pr_table %>% 
    mutate(GH = if_else(property_ID %in% GH_list, TRUE, FALSE))
  
  pr_table <-
    FREH %>% 
    filter(date >= start_date, date <= end_date) %>% 
    group_by(property_ID) %>% 
    summarize(FREH = if_else(n() >= sens_n, TRUE, FALSE)) %>% 
    left_join(pr_table, ., by = "property_ID") %>% 
    mutate(FREH = if_else(is.na(FREH), FALSE, FREH))
  
  # Add principal_res field
  
  pr_table <- 
    pr_table %>% 
    mutate({{ field_name }} := case_when(
      housing == FALSE               ~ FALSE,
      !ML & zone %in% c("R1L", "R1S") & !apartment ~ TRUE,
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
  strr_principal_residence(daily, FREH, GH, "2019-05-01", "2019-09-30", 
                           principal_res_2019, 0.5) %>% 
  strr_principal_residence(daily, FREH, GH, "2018-05-01", "2018-09-30", 
                           principal_res_2018, 0.5) %>% 
  strr_principal_residence(daily, FREH, GH, "2017-05-01", "2017-09-30", 
                           principal_res_2017, 0.5)


### Add seasonal fields ########################################################

season_start <- as.Date("2019-05-01")
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
    seasonal_2019 = if_else(n_ar_2019 >= 120 & n_r_2019 >= 60, TRUE, FALSE),
    seasonal_2018 = if_else(n_ar_2018 >= 120 & n_r_2018 >= 60, TRUE, FALSE),
    seasonal_2017 = if_else(n_ar_2017 >= 120 & n_r_2017 >= 60, TRUE, FALSE)
    ) %>% 
    select(property_ID, seasonal_2019:seasonal_2017) %>% 
    left_join(property, ., by = "property_ID")
  
property <-
  property %>% 
  select(-geometry, everything(), geometry)


### Calculate scenarios ########################################################

property <-
  property %>% 
  mutate(
    scenario_1 = if_else(principal_res_2019 == TRUE & apartment == FALSE, TRUE, 
                         FALSE),
    scenario_2 = if_else(principal_res_2019 == TRUE, TRUE, FALSE),
    scenario_3 = if_else((principal_res_2019 == TRUE & apartment == FALSE) | 
                           commercial == TRUE, TRUE, FALSE),
    scenario_4 = if_else(principal_res_2019 == TRUE | commercial == TRUE, TRUE, 
                         FALSE),
    scenario_5 = if_else(principal_res_2019 == TRUE | commercial == TRUE | 
                           DMUN == TRUE, TRUE, FALSE)
    ) %>% 
  select(-geometry, everything(), geometry)


### Save files #################################################################

save(city, daily, DAs, FREH, GH, host, ML_daily, ML_property, parcels,
     property, registration, streets, wards, zones, end_date, exchange_rate, 
     season_end, season_start, file = "data/charlottetown.Rdata")
