#### HALIFAX IMPORT ############################################################

source("R/01_helper_functions.R")

#regions <- list_census_regions('CA16')

### Build geometries ###########################################################

CRM <-
  get_census(dataset = "CA16", regions = list(CSD = "1102075"), level = "CSD",
             geo_format = "sf") %>% 
  st_transform(32617)

streets <- 
  (getbb("Charlottetown Prince Edward Island") * c(1.01, 0.99, 0.99, 1.01)) %>% 
  opq(timeout = 200) %>% 
  add_osm_feature(key = "highway") %>% 
  osmdata_sf()

CRM_streets <- 
  rbind(streets$osm_polygons %>% st_cast("LINESTRING"), streets$osm_lines) %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  st_transform(32617) %>%
  st_intersection(CRM) %>% 
  select(osm_id, name, geometry)

neighbourhoods <-
  read_sf(dsn = "data", layer = "charlottetown") %>%
  st_transform(32617) %>% 
  select(id = OBJECTID, neighbourhood = OLD_DIST, geometry) %>% 
  group_by(neighbourhood) %>% 
  st_buffer(0) %>% 
  summarize(count = n())

names <- read_csv("data/names.csv")

neighbourhoods <- neighbourhoods %>% 
  left_join(names, by = "neighbourhood")


### Census import ##############################################################

CTs_charlottetown <-
  get_census(
    dataset = "CA16", regions = list(PR = "11"), level = "DA",
    vectors = c("v_CA16_2398", "v_CA16_5078", "v_CA16_4888", "v_CA16_6695",
                "v_CA16_4837", "v_CA16_4838", "v_CA16_512", 
                "v_CA16_3393", "v_CA16_3996"),
    geo_format = "sf") %>% 
  st_transform(32617) %>% 
  filter(Type == "DA") %>% 
  select(GeoUID, PR_UID, CMA_UID, Population, Households, contains("v_CA"))

