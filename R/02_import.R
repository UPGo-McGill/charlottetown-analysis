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
  select(GeoUID, CSD_UID, Population, Households, contains("v_CA")) %>% 
  set_names(c("Geo_UID", "CMA_UID", "population", "households", "med_income",
    "university_education", "housing_need", "non_mover", "owner_occupier", 
    "rental", "official_language", "citizen", "white", "geometry")) %>% 
  mutate_at(
    .vars = c("university_education", "non_mover", 
              "official_language", "citizen", "white"),
    .funs = list(`pct_pop` = ~{. / population})) %>% 
  mutate_at(
    .vars = c("housing_need", "owner_occupier", "rental"),
    .funs = list(`pct_household` = ~{. / households}))

### [ OMITTED ] Import CMHC neighbourhoods #################################################

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


