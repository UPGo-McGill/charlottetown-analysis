### Host Compliance data #######################################################

library(tidyverse)
library(strr)
library(sf)
library(readxl)


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
    )

registration <- 
  read_xls("data/registration.xls") %>%  
  set_names(c("property", "address", "owner")) %>%
  mutate(address = str_replace(address, "(\\.|,)$", "")) %>% 
  mutate(address = case_when(
    address == "60&64 Nicholas Lane Ch" ~ "	60&64 Nicholas Lane",
    address == "140 Mount Edward" ~ "140 Mount Edward Road",
    address == "227 Richmond" ~ "227 Richmond Street",
    TRUE ~ address
  )) %>% 
  mutate(
    street_number = str_extract(address, ".*[:digit:]"),
    street_name = str_replace(address, ".*[:digit:] ", ""),
    street_type = str_extract(street_name, "(\\w+)$\\.?"),
    street_name = str_replace(street_name, "\\s+\\S*$", ""),
    street_type = case_when(
      street_type == "Crt"  ~ "Court",
      street_type == "Rd"   ~ "Road",
      street_type %in% c("St", "S") ~ "Street",
      street_type == "Dr"   ~ "Drive",
      street_type == "Ave"  ~ "Avenue",
      street_type == "Ln"   ~ "Lane",
      street_type %in% c("Ct", "Cr", "Cres") ~ "Crescent",
      street_type == "Cir"  ~ "Circle",
      street_type == "Pkwy" ~ "Parkway",
      street_type == "Pl"   ~ "Place",
      TRUE                  ~ street_type
    )
    )

assessment <- 
  read_excel("data/assessment.xlsx")

assessment %>% view()


library(ggmap)

host <- 
  host %>% 
  mutate_geocode(address)

view(host)


host %>% 
  filter(is.na(lat)) %>% 
  pull(address)



