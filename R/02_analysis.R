#### Charlottetown ANALYSIS ##########################################################

# Load packages
library(tidyverse)
library(upgo)
library(strr)
library(future)
library(sf)
library(osmdata)
library(lubridate)
library(cancensus)
library(scales)
library(zoo)


# Load data file

load("data/charlottetown.Rdata")

# Set up dates

end_date <- as.Date("2019-11-30")
key_date <- as.Date("2019-07-01")

# Exchange rate (average over last twelve months)

# exchange_rate <- mean(1.3037, 1.3010, 1.3200,
#                       1.3432, 1.3301, 1.3206, 
#                       1.3368, 1.3378, 1.3438,
#                       1.3188, 1.3046, 1.3316)


### Region comparison ##########################################################

# revenue <-  property_AC %>% 
#     filter(housing == TRUE) %>%
#     group_by(region) %>% 
#     summarise(region_revenue = sum(revenue, na.rm = TRUE))
         
# cities <-
#   property_AC %>% 
#   filter(housing == TRUE) %>% 
#   count(city) 
# 
# region <- property_AC %>% 
#   filter(housing == TRUE) %>% 
#   count(region) 

canada_population <- 
  get_census("CA16", regions = list(PR = c("10", "11", "12", "13")), 
             level = "CSD")


### Active daily listings ######################################################

## Active listings from property file
# All listings
nrow(filter(property, housing == TRUE, created <= key_date, scraped >= key_date))

# Housing listings over the last twelve months
nrow(LTM_property)

# Listing type breakdown
nrow(filter(property, listing_type == "Entire home/apt")) /
  nrow(filter(property))

nrow(filter(LTM_property, listing_type == "Shared room"))/
  nrow(LTM_property)

# Number of hosts over last twelve months
length(unique(LTM_property$host_ID))

# Hosts by listing type
LTM_property %>% 
  filter(listing_type == "Entire home/apt") %>% 
  select(host_ID) %>% 
  st_drop_geometry() %>% 
  unique() %>% 
  nrow()/
  length(unique(LTM_property$host_ID))

# LTM revenue

sum(LTM_property$revenue_LTM, na.rm = TRUE)

# LTM revenue by property type
filter(LTM_property, listing_type == "Entire home/apt") %>% 
  select(revenue_LTM) %>% 
  st_drop_geometry() %>% 
  sum(na.rm = TRUE) /
  sum(LTM_property$revenue_LTM, na.rm = TRUE)

# YOY growth rate
nrow(filter(property, housing == TRUE)) / 
  nrow(filter(property, created <= end_date - years(1), scraped >= end_date - years(1),
              housing == TRUE))


### Charlottetown mapping prep ###################################################


### Which STR platforms are used in Halifax? ###################################

# Airbnb and not Homeaway
nrow(filter(LTM_property, !is.na(ab_property), is.na(ha_property)))

# Homeaway and not Airbnb
nrow(filter(LTM_property, !is.na(ha_property), is.na(ab_property)))

# Both Airbnb and Homeaway
nrow(filter(LTM_property, !is.na(ha_property), !is.na(ab_property)))

nrow(LTM_property)


### Listing type prevalence ####################################################


  property %>% 
  filter(housing == TRUE, created <= key_date, scraped >= key_date) %>% 
  rename(`Listing type` = listing_type) %>% 
  st_drop_geometry() %>% 
  group_by(`Listing type`) %>% 
  summarize(`Number of listings` = n(),
            `Annual revenue` = sum(revenue_LTM, na.rm = TRUE),
            `Rev. per listing` = `Annual revenue` / n()) %>% 
  mutate(
    `% of all listings` = round(`Number of listings` /
                                  sum(`Number of listings`), 3),
    `% of all listings` = paste0(100 * `% of all listings`, "%"),
    `% of annual revenue` = `Annual revenue` / sum(`Annual revenue`)) %>% 
  mutate(
    `Annual revenue` = round(`Annual revenue`),
    `Annual revenue` = paste0("$", str_sub(`Annual revenue`, 1, -7), ".",
                              str_sub(`Annual revenue`, -6, -6), " million"),
    `% of annual revenue` = round(`% of annual revenue`, 3),
    `% of annual revenue` = paste0(100 * `% of annual revenue`, "%"),
    `Rev. per listing` = round(`Rev. per listing`),
    `Rev. per listing` = paste0("$", str_sub(`Rev. per listing`, 1, -4),
                                ",", str_sub(`Rev. per listing`, -3, -1))
  ) %>% view() %>% 
  write.table("output/tables/table_listing_types.txt")


### Bedroom breakdown ##########################################################

property %>% 
  filter(housing == TRUE,
         listing_type == "Entire home/apt") %>% 
  count(bedrooms) %>% 
  mutate(percentage = n / sum(n)) %>% 
  group_by(bedrooms) %>% 
  summarize(perc_bedrooms = sum(percentage)) 

# property_bedrooms <- 
#   property %>% 
#   filter(
#          housing == TRUE,
#          listing_type == "Entire home/apt") %>% 
#   count(bedrooms) %>% 
#   mutate(percentage = n / sum(n))
# 
# sum(property_bedrooms$percentage)
# sum(property_bedrooms$n)



### Revenue distribution and commercial operators ##############################

## Host revenue percentiles

table_host_revenue <- 
daily %>%
  filter(housing == TRUE, date > end_date - years(1), status == "R") %>%
  group_by(host_ID) %>%
  summarize(rev = sum(price)*exchange_rate) %>%
  filter(rev > 0) %>%
  summarize(
    `Top 1%`  = sum(rev[rev > quantile(rev, c(0.99))] / sum(rev)),
    `Top 5%`  = sum(rev[rev > quantile(rev, c(0.95))] / sum(rev)),
    `Top 10%` = sum(rev[rev > quantile(rev, c(0.90))] / sum(rev)),
    `Top 20%` = sum(rev[rev > quantile(rev, c(0.80))] / sum(rev))) 


## Median host income

LTM_property %>% 
  filter(revenue_LTM > 0) %>% 
  pull(revenue_LTM) %>% 
  quantile() %>% 
  as.list() %>% 
  as_tibble() %>% 
  select(-`0%`) %>% 
  set_names(c("25th percentile", "Median", "75th percentile", 
              "100th percentile")) %>% 
  mutate_all(round, -2)

## Top earning host(s)

LTM_property %>% 
  group_by(host_ID) %>% 
  summarise(host_rev = sum(revenue_LTM)) %>% 
  filter(host_rev>0) %>% 
  arrange(-host_rev)

## Multilistings

ML_table <- 
  daily %>% 
  group_by(date) %>% 
  summarize(Listings = mean(ML),
            Revenue = sum(price * (status == "R") * ML * 
                            exchange_rate, na.rm = TRUE) / 
              sum(price * (status == "R") * exchange_rate, na.rm = TRUE)) %>% 
  gather(Listings, Revenue, key = `Multilisting percentage`, value = Value)

ML_table %>% 
  filter(date == end_date)

# Entire home multilistings

daily %>% 
 filter(listing_type == "Entire home/apt") %>% 
  group_by(date) %>% 
  summarize(Listings = sum(ML)) %>% 
  filter(date == end_date)


### Housing loss ###############################################################

FREH %>% 
  filter(date == end_date) %>% 
  count()

FREH %>% 
  count(date) %>% 
  ggplot() +
  geom_line(aes(date, n), colour = "black", size = 1) +
  theme_minimal() +
  scale_y_continuous(name = NULL, label = scales::comma) +
  ggtitle("FREH listings in Charlottetown")

GH %>% 
  st_drop_geometry() %>% 
  group_by(date) %>% 
  summarize(GH_units = sum(housing_units)) %>%
  ggplot() +
  geom_line(aes(date, GH_units), colour = "black", size = 1) +
  theme_minimal() +
  scale_y_continuous(name = NULL, label = scales::comma) +
  ggtitle("Units converted to ghost hostels in Charlottetown")

GH_total <- 
  GH %>% 
  st_drop_geometry() %>% 
  group_by(date) %>% 
  summarize(GH_units = sum(housing_units)) %>% 
  pull(GH_units) %>% 
  rollmean(30, align = "right") # TKTK zoo() 

#TKTK error 
GH_total <- GH_total[(length(GH_total) + 1 - 
                        n_groups(FREH %>% group_by(date))):length(GH_total)]

# TKTK error from missing GH_total
housing_loss <- 
  FREH %>% 
  group_by(date) %>% 
  summarize(`Entire home/apt` = n()) %>% 
  mutate(`Private room` = as.integer(GH_total)) %>% 
  gather(`Entire home/apt`, `Private room`, key = `Listing type`,
         value = `Housing units`)

#TKTK error: need to fix housing loss

# Current housing loss figure
sum(filter(housing_loss, date == end_date)$`Housing units`)

# YOY increase
sum(filter(housing_loss, date == end_date)$`Housing units`) /
  sum(filter(housing_loss, date == end_date - years(1))$`Housing units`)


## Relate housing loss to rental vacancy rate

# vacancy_rate <- 1.016 
# 
# housing <- 
#   get_census("CA16", regions = list(CSD = "1209034"), level = "CSD", 
#              vectors = c("v_CA16_4897", "v_CA16_405"))
# 
# housing %>% 
#   select(
#     `v_CA16_405: Private dwellings occupied by usual residents`,
# `v_CA16_4897: Total - Tenant households in non-farm, non-reserve private dwellings - 25% sample data`
#          ) %>% 
#   set_names(c("Dwellings", "Tenants")) %>% 
#   pull(Tenants) %>% 
#   {. * vacancy_rate * (vacancy_rate - 1)}

## Yearly principal_res vs Reserved status comparisons #########################

reserved_vs_pr <- 
  function(property, daily, start_date, end_date, 
           group_var = principal_res_2019, field_name) {
    
    start_date <- as.Date(start_date, origin = "1970-01-01")
    end_date <- as.Date(end_date, origin = "1970-01-01")
    
    reserved_pr <- 
      daily %>% 
      filter(date >= start_date, date <= end_date, status == "R") %>% 
      count({{ field_name }} := property_ID %in%  
              filter(property, {{ group_var }} == TRUE)$property_ID)
    
  }

r_2019 <- reserved_vs_pr(property, daily, "2019-04-01", "2019-09-30", principal_res_2019, reserved_pr_2019)
r_2018 <- reserved_vs_pr(property, daily, "2018-04-01", "2018-09-30", principal_res_2018, reserved_pr_2018)
r_2017 <- reserved_vs_pr(property, daily, "2017-04-01", "2017-09-30", principal_res_2017, reserved_pr_2017)


## Save files #####################################
save(active_listings_filtered, file = "data/active_listings_filtered.Rdata")
save(property, file = "data/charlottetown_property.Rdata")
#save(housing_loss, file = "data/housing_loss.Rdata")
#save(airbnb_neighbourhoods, file = "data/airbnb_neighbourhoods.Rdata")
save(principal_res, file = "data/principal_res.Rdata")
#save(urban_rural, file = "data/urban_rural.Rdata")
#save(neighbourhoods, file = "data/neighbourhoods.Rdata")




