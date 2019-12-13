#### HALIFAX IMPORT ############################################################

source("R/01_helper_functions.R")

#regions <- list_census_regions('CA16')

### Build geometries ###########################################################

## For Charlottetown only (not PEI-wide)

charlottetown <-
  get_census(dataset = "CA16", regions = list(CSD = "1102075"), level = "CSD",
             geo_format = "sf") %>% 
  st_transform(32620)

streets <- 
  (getbb("Charlottetown Prince Edward Island") * c(1.01, 0.99, 0.99, 1.01)) %>% 
  opq(timeout = 200) %>% 
  add_osm_feature(key = "highway") %>% 
  osmdata_sf()

charlottetown_streets <- 
  rbind(streets$osm_polygons %>% st_cast("LINESTRING"), streets$osm_lines) %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  st_transform(32620) %>%
  st_intersection(charlottetown) %>% 
  select(osm_id, name, geometry)

### Census import ##############################################################

DAs_charlottetown <-
  get_census(
    dataset = "CA16", regions = list(CSD = "1102075"), level = "DA",
    vectors = c("v_CA16_2398", "v_CA16_5078", "v_CA16_4888", "v_CA16_6695",
                "v_CA16_4837", "v_CA16_4838", "v_CA16_512", 
                "v_CA16_3393", "v_CA16_3996"),
    geo_format = "sf") %>% 
  st_transform(32620) %>% 
  select(GeoUID, CSD_UID, Population, Dwellings, contains("v_CA")) %>% 
  set_names(c("Geo_UID", "CMA_UID", "population", "dwellings", "med_income",
    "university_education", "housing_need", "non_mover", "owner_occupier", 
    "rental", "official_language", "citizen", "white", "geometry")) %>% 
  mutate_at(
    .vars = c("university_education", "non_mover", 
              "official_language", "citizen", "white"),
    .funs = list(`pct_pop` = ~{. / population})) %>% 
  mutate_at(
    .vars = c("housing_need", "owner_occupier", "rental"),
    .funs = list(`pct_dwellings` = ~{. / dwellings}))

### Import CMHC neighbourhoods #################################################

## need to get shp CMHC <- read_sf("data/CMHC/CMHC_NBHD_2016-mercWGS84.shp")

# CMHC <-
#   CMHC %>% 
#   filter(METCODE %in% c("3300", "7690")) %>%
#   select(-OBJECTID, -NBHDNAME_F, -NBHDNAME_1, -NBHDNAME_L, -NBHDNAME_E,
#          -NAME_FR, -GEO_LAYER_, -SHAPE_Leng, -SHAPE_Area) %>% 
#   mutate(zone = c("Charlottetown", "Peripheral Charlottetown",
#                   "Summerside")) %>% 
#   select(zone, NBHDCODE, geometry) %>%
#   st_transform(32620)

con <- RPostgres::dbConnect(
  RPostgres::Postgres(),
  user = "rbbinder",
  password = "csq4PvpZksy4tZdKW8fvsMoa",
  host = "025wpgs.campus.mcgill.ca",
  dbname = "airdna")

property_all <- tbl(con, "property")

daily_all <- tbl(con, "daily")

property <- 
  property_all %>% 
  filter(country == "Canada", city == "Charlottetown") %>% 
  collect()

property <-  property %>% 
  filter(!is.na(listing_type)) %>% 
  select(property_ID:longitude, ab_property:ha_host, bedrooms) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(32620)

property <- 
  property %>% 
  select(-zone) %>% 
  st_intersection(CMHC) %>% 
  st_drop_geometry() %>% 
  select(property_ID, zone) %>% 
  left_join(select(property, -zone), .) %>% 
  mutate(zone = if_else(is.na(zone), "Rest of PEI", zone))

daily_compressed <- 
  daily_all %>% 
  filter(property_ID %in% !! property$property_ID) %>% 
  collect()

# Set up ML file at this point as some hosts may have properties in other cities

ML_property <- 
  property_all %>% 
  filter(host_ID %in% !! property$host_ID) %>% 
  collect()

ML_daily <- 
  daily_all %>% 
  filter(property_ID %in% !! ML_property$property_ID) %>% 
  collect()

# # Atlantic Canada 
# property_AC <- 
#   property_all %>% filter(country == "Canada", 
#                           region %in% c("Nova Scotia", "New Brunswick", 
#                                         "Prince Edward Island", 
#                                         "Newfoundland and Labrador")) %>% 
#   collect()
# 
# property_AC <-  property_AC %>% 
#   filter(!is.na(listing_type)) %>% 
#   select(property_ID:longitude, ab_property:ha_host, bedrooms, city, region) %>% 
#   st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
#   st_transform(32617)
# 
# daily_AC <- 
#   daily_all %>% 
#   filter(property_ID %in% !! property_AC$property_ID) %>% 
#   collect()
# 
# rm(con, daily_all, property_all)

start_date <- "2018-10-31"
end_date <- "2019-10-30"

daily <- 
  strr_expand(daily_compressed, cores = 2) #changed from 4 cores

daily <- 
  daily %>% 
  filter(date >= created, date - 30 <= scraped, status != "U")

daily <- 
  property %>% 
  st_drop_geometry() %>% 
  select(property_ID, zone) %>% 
  left_join(select(daily, -zone), .)


# daily_AC <- 
#   strr_expand(daily_AC, cores = 2) #changed from 4 cores
# 
# daily_AC <- 
#   daily_AC %>% 
#   filter(date >= created, date - 30 <= scraped, status != "U")


## Run the raffle to assign a neighbourhood and a census tract #################

property <- 
  property %>% 
  strr_raffle(DAs_charlottetown, Geo_UID, dwellings) %>% 
  mutate(DA_GeoUID = winner) %>% 
  select(-winner)

## Add last twelve months revenue

exchange_rate <- mean(1.3037, 1.3010, 1.3200,
                      1.3432, 1.3301, 1.3206, 
                      1.3368, 1.3378, 1.3438,
                      1.3188, 1.3046, 1.3316)

property <- 
  daily %>% 
  filter(date >= start_date, status == "R") %>% 
  group_by(property_ID) %>% 
  summarize(revenue = sum(price) * exchange_rate) %>% 
  select(property_ID, revenue) %>% 
  left_join(property, .)

# property_AC <- 
#   daily_AC %>% 
#   filter(date >= start_date, status == "R") %>% 
#   group_by(property_ID) %>% 
#   summarize(revenue = sum(price) * exchange_rate) %>% 
#   select(property_ID, revenue) %>% 
#   left_join(property_AC, .)


## Create last twelve months property file

LTM_property <- 
  property %>% 
  filter(created <= end_date, scraped >= start_date, housing == TRUE)


### Process multilistings ######################################################

## Prepare ML_daily

ML_daily <- 
  strr_expand(ML_daily, cores = 2) #changed from 4 cores

ML_daily <- 
  ML_daily %>% 
  filter(date >= created, date <= scraped + 30, status != "U")

## Do ML calculations

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


### Calculate FREH and GH listings #############################################

FREH <- 
  daily %>% 
  strr_FREH("2015-09-30", end_date, cores = 2) %>% as_tibble() %>%   
  filter(FREH == TRUE) %>% 
  select(-FREH)

GH <- 
  property %>% 
  filter(housing == TRUE) %>% 
  strr_ghost(start_date = "2014-10-01", end_date = end_date)


### Save files #################################################################

save(charlottetown, file = "data/charlottetown.Rdata")
save(charlottetown_streets, file = "data/charlottetown_streets.Rdata")
save(property, file = "data/charlottetown_property.Rdata")
save(LTM_property, file = "data/charlottetown_LTM_property.Rdata")
save(GH, file = "data/charlottetown_GH.Rdata")
save(FREH, file = "data/charlottetown_FREH.Rdata")
save(daily, file = "data/charlottetown_daily.Rdata")
save(daily_compressed, file = "data/charlottetown_daily_compressed.Rdata")
save(DAs_charlottetown, file = "data/DAs_charlottetown.Rdata")
#save(neighbourhoods, file = "data/HRM_neighbourhoods.Rdata")
#save(daily_AC, file = "data/daily_AC.Rdata")
#save(property_AC, file = "data/property_AC.Rdata")


