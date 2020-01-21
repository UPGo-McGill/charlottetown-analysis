### Host Compliance data #######################################################

library(tidyverse)
library(strr)
library(sf)
library(readxl)

?readxl::read_xlsx

host <-
  read_xlsx("data/host.xlsx") %>% 
  set_names(c("parcel_number", "compliance_status", "advertised", "STR",
                 "identification_status", "first_activity", "first_identified",
                 "last_posted", "mailability_status", "address",
                 "unit_number", "owner_name", "listings", "last_stay",
                 "registration_numbers", "max_sleeping_capacity",
                 "max_bedrooms", "property_types", "rental_unit_record",
                 "most_recent_comment"))

host <-
  host %>% 
  select(parcel_number, STR, listings, address, unit_number, compliance_status,
         identification_status, advertised, registration_numbers, 
         property_types) %>% 
  mutate(advertised = if_else(advertised == "Yes", TRUE, FALSE),
         STR = if_else(STR == "Yes", TRUE, FALSE),
         )


host <- 
  host %>% 
  mutate(listings = 
           str_split(listings, '"') %>% 
           map(str_extract_all, "(?<=www.).*?(?=<)") %>% 
           map(unlist)) %>% 
  unnest(listings)

host$listings[1:20]

host %>% 
  mutate(property_ID = case_when(
    str_detect(listings, "airbnb") ~ paste0("ab-", str_extract(listings, "(?<=rooms/).*")),
    str_detect(listings, "homeaway") ~ paste0("ha-", str_extract(listings, "(?<=rental/p).*")),
    TRUE ~ NA_character_
  )) %>% 
  select(property_ID, everything(), -listings)