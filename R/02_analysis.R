#### Charlottetown ANALYSIS ####################################################

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
library(data.table)


### Load data file #############################################################

load("data/charlottetown.Rdata")


### Active daily listings ######################################################

## Active listings from property file

# Listings active on Sep 1
property %>% 
  filter(housing, created <= key_date, scraped >= key_date) %>% 
  nrow()

# Hosts active on Sep 1
property %>% 
  filter(housing, created <= key_date, scraped >= key_date) %>% 
  pull(host_ID) %>% 
  unique() %>% 
  length()

# Housing listings over the last twelve months
LTM_property <-
  property %>%
  filter(housing, created <= end_date, scraped > end_date - years(1))

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
  #st_drop_geometry() %>% 
  unique() %>% 
  nrow()/
  length(unique(LTM_property$host_ID))

# LTM revenue

LTM_property <-
  daily %>%
  filter(housing,
         date <= end_date, date > end_date - years(1),
         status == "R") %>%
  group_by(property_ID) %>%
  summarize(revenue_LTM = sum(price) * exchange_rate) %>%
  left_join(LTM_property, .)

sum(LTM_property$revenue_LTM, na.rm = TRUE)

# Principal residence revenue

property %>%
  st_drop_geometry() %>%
  filter(housing, created <= key_date, scraped >= key_date, principal_res_2019) 

property %>%
  st_drop_geometry() %>%
  filter(housing, created <= end_date, scraped > end_date - years(1)) %>% 
  filter(principal_res_2019)

sep1_pr_propertyID <- 
  property %>%
  st_drop_geometry() %>%
  filter(housing, created <= key_date, scraped >= key_date, 
         principal_res_2019 == FALSE) %>% 
  pull(property_ID)
  
LTM_pr <-
  daily %>%
  filter(housing,
         date <= end_date, date > end_date - years(1),
         status == "R") %>%
  group_by(property_ID) %>%
  summarize(revenue_PR = sum(price) * exchange_rate) %>%
  left_join(., property) #%>% 
  #filter(principal_res_2019 == TRUE)

sum(LTM_pr$revenue_PR, na.rm = TRUE)

LTM_pr_revenue <- 
  LTM_pr %>% 
  filter(property_ID %in% sep1_pr_propertyID) 

sum(LTM_pr_revenue$revenue_PR, na.rm = TRUE)

length(unique(LTM_pr$host_ID))


# LTM revenue by property type
filter(LTM_revenue, listing_type == "Entire home/apt") %>% 
  select(revenue_LTM) %>% 
  #st_drop_geometry() %>% 
  sum(na.rm = TRUE) /
  sum(LTM_revenue$revenue_LTM, na.rm = TRUE)

# YOY growth rate
#2019
nrow(filter(property, housing == TRUE, created <= end_date, scraped >= end_date)) / 
  nrow(filter(property, created <= end_date - years(1), scraped >= end_date - years(1),
              housing == TRUE))

property %>%
  st_drop_geometry() %>%
  summarize(length(property_ID[housing == TRUE & created <= end_date & scraped >= end_date])) 

property %>%
  st_drop_geometry() %>%
  summarize(length(property_ID[housing == TRUE & created <= end_date - years(1) &
                                 scraped >= end_date - years(1)])) 

property %>%  
length(property_ID[housing == TRUE & created <= end_date & scraped > end_date]) / 
  length(property_ID[housing == TRUE & created <= end_date - years(1) &
                       scraped >= end_date - years(1)])


#2018
nrow(filter(property, housing == TRUE)) / 
  nrow(filter(property, created <= end_date - years(1), scraped >= end_date - years(1),
              housing == TRUE))

#2017
nrow(filter(property, housing == TRUE)) / 
  nrow(filter(property, created <= end_date - years(2), scraped >= end_date - years(2),
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

# Listings by ward

## messy but works
LTM_property %>%
  rename(`Listing type` = listing_type) %>%
  st_drop_geometry() %>%
  group_by(ward) %>%
  summarize(`Number of listings` = n(),
            `Active listings on 2019/09/01` = 
              as.numeric(length(property_ID[created <= key_date & scraped >= key_date])),
            `Annual revenue` = sum(revenue_LTM, na.rm = TRUE),
            `Rev. per listing` = `Annual revenue` / n(),
            `Annual growth ratio` = 
              as.numeric(
                length(
                  property_ID[housing & created <= end_date & scraped >= end_date]) /
                  length(property_ID[housing & created <= end_date - years(1) &
                                       scraped >= end_date - years(1)]))) %>%
  mutate(
    `% of all listings` = `Number of listings` / sum(`Number of listings`),
    `% of annual revenue` = `Annual revenue` / sum(`Annual revenue`),
    `% annual growth` = `Annual growth ratio` - 1
  ) %>%
  drop_na() %>%
  select(-"Annual growth ratio", -"Number of listings") %>%
  mutate(
    `% reservations from May-September` = c(0.7379253, 0.6388024, 0.7693436,
                                            0.8985275, 0.6794479, 0.7949283,
                                            0.6406896, 0.8268593, 0.7344935,
                                            0.8138371)) %>% 
  view()

## Clean but rounding errors

# LTM_property %>% 
#   filter(housing == TRUE) %>% 
#   rename(`Listing type` = listing_type) %>% 
#   #st_drop_geometry() %>% 
#   group_by(ward) %>% 
#   summarize(`Number of listings` = n(),
#             `Active listings on 2019/09/01` = 
# as.numeric(length(property_ID[created <= key_date & scraped >= key_date])),
#             `Annual revenue` = sum(revenue_LTM, na.rm = TRUE),
#             `Rev. per listing` = `Annual revenue` / n(),
#             `Annual growth ratio` = as.numeric(length(property_ID[housing == TRUE &
#                               created <= end_date & scraped >= end_date]) / 
#                               length(property_ID[housing == TRUE & 
#                               created <= end_date - years(1) &
#                               scraped >= end_date - years(1)]))) %>% 
#   mutate(
#     `% of all listings` = round(`Number of listings` /
#                                   sum(`Number of listings`), 3),
#     `% of all listings` = paste0(100 * `% of all listings`, "%"),
#     `% of annual revenue` = `Annual revenue` / sum(`Annual revenue`),
#     `% annual growth` = paste0(round(100 * (`Annual growth ratio` - 1), 1), "%")
#     ) %>% 
#   mutate(
#     `Annual revenue` = round(`Annual revenue`),
#     `Annual revenue` = paste0("$", str_sub(`Annual revenue`, 1, -7), ".",
#                               str_sub(`Annual revenue`, -6, -6), " million"),
#     `% of annual revenue` = round(`% of annual revenue`, 3),
#     `% of annual revenue` = paste0(100 * `% of annual revenue`, "%"),
#     `Rev. per listing` = round(`Rev. per listing`),
#     `Rev. per listing` = paste0("$", str_sub(`Rev. per listing`, 1, -4),
#                                 ",", str_sub(`Rev. per listing`, -3, -1))#,
#     # `% annual growth` = round(`% annual growth`)
#   ) %>%
#   drop_na() %>% 
#   select(-"Annual growth ratio", -"Number of listings") %>% 
#   mutate(
#     `% reservations from May-September` = c(0.7379253, 0.6388024, 0.7693436, 
#                                             0.8985275, 0.6794479, 0.7949283, 
#                                             0.6406896, 0.8268593, 0.7344935,
#                                             0.8138371)) %>% 
#   view() %>% 
#   write.table("output/tables/table_listings_wards.txt")

# Listing types for city

LTM_revenue %>% 
  filter(housing, created <= end_date, scraped > end_date - years(1)) %>% 
  rename(`Listing type` = listing_type) %>% 
  #st_drop_geometry() %>% 
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

LTM_revenue %>% 
  filter(revenue_LTM > 0, !is.na(host_ID)) %>% 
  group_by(host_ID) %>% 
  summarize("host_rev" = sum(revenue_LTM)*exchange_rate) %>% 
  pull(host_rev) %>%
  quantile() %>% 
  as.list() %>% 
  as_tibble() %>% 
  select(-`0%`) %>% 
  set_names(c("25th percentile", "Median", "75th percentile", 
              "100th percentile")) %>% 
  mutate_all(round, -2) %>% 
  drop_na()

## Top earning host(s)

# TKTK 
LTM_revenue %>% 
  group_by(host_ID) %>% 
  summarize(host_rev = sum(revenue_LTM)*exchange_rate) %>% 
  filter(host_rev>0) %>% 
  arrange(-host_rev) %>% 
  drop_na()

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
  filter(date == key_date)

property %>% 
  filter(housing, created <= key_date, scraped >= key_date) %>% 
  summarize(mean(principal_res_2019))

# Entire home multilistings

daily %>% 
 filter(listing_type == "Entire home/apt") %>% 
  group_by(date) %>% 
  summarize(Listings = sum(ML)) %>% 
  filter(date == end_date)

# Principal residences

property %>% 
  filter(housing, created <= key_date, scraped >= key_date, 
         principal_res_2019 == FALSE) %>% 
  nrow()


### Housing loss ###############################################################

FREH %>% 
  filter(date == end_date) %>% 
  count()

FREH %>% 
  count(date) %>% 
  ggplot() +
  geom_line(aes(date, n), colour = "black", size = 1) +
  theme_minimal() +
  scale_y_continuous(name = NULL, label = comma) +
  ggtitle("FREH listings in Charlottetown")

GH %>% 
  st_drop_geometry() %>% 
  group_by(date) %>% 
  summarize(GH_units = sum(housing_units)) %>%
  ggplot() +
  geom_line(aes(date, GH_units), colour = "black", size = 1) +
  theme_minimal() +
  scale_y_continuous(name = NULL, label = comma) +
  ggtitle("Units converted to ghost hostels in Charlottetown")

GH_total <-
  GH %>%
  st_drop_geometry() %>%
  group_by(date) %>%
  summarize(GH_units = sum(housing_units)) %>%
  mutate(GH_average = frollmean(GH_units, 30, align = "right")) %>%
  mutate(GH_average = if_else(is.na(GH_average), 8, GH_average)) %>%
  select(-GH_units)

# Seasonal housing loss

# 2019
FREH_season_2019 <- 
  FREH %>% 
  filter(date >= season_start & date <= season_end)

seasonal_loss_2019 <- 
  nrow(filter(property, seasonal_2019 == TRUE)) - 
  property %>% 
  filter(seasonal_2019, property_ID %in% FREH_season_2019$property_ID) %>% 
  nrow()

#2018
FREH_season_2018 <-
  FREH %>% 
  filter(date >= season_start - years(1) & date <= season_end - years(1))

seasonal_loss_2018 <-
  nrow(filter(property, seasonal_2018 == TRUE)) - 
  property %>% 
  filter(seasonal_2018, property_ID %in% FREH_season_2018$property_ID) %>% 
  nrow()

#2017
FREH_season_2017 <- 
  FREH %>% 
  filter(date >= season_start - years(2) & date <= season_end - years(2))

seasonal_loss_2017 <-
  nrow(filter(property, seasonal_2017 == TRUE)) - 
  property %>% 
  filter(seasonal_2017, property_ID %in% FREH_season_2017$property_ID) %>% 
  nrow()

# Housing loss numbers

housing_loss <-
  FREH %>%
  group_by(date) %>%
  summarize(`Entire home/apt` = n()) %>%
  left_join(GH_total, by = "date") %>%
  rename(`Private room` = GH_average) %>%
  mutate(`Summer listings` = case_when(
    date >= season_start & date <= season_end ~ seasonal_loss_2019,
    date >= season_start - years(1) & date <= season_end - years(1) ~ seasonal_loss_2018,
    date >= season_start - years(2) & date <= season_end - years(2) ~ seasonal_loss_2017,
    TRUE ~ 0L)) %>% 
  gather(`Entire home/apt`, `Private room`, `Summer listings`, 
         key = `Listing type`, value = `Housing units`) 

# Current housing loss figure
sum(filter(housing_loss, date == end_date)$`Housing units`)

# Scenario Housing loss

## TKTK GH number needs to be checked
#scenario1
  (FREH %>% 
                filter(property_ID %in% filter(property, housing == TRUE)$property_ID, 
                       date > end_date - years(1), date <= end_date, 
                       property_ID %in%   (filter(property, scenario_1)$property_ID)) %>% 
                nrow()/365) + #sum(GH_total$GH_2019) #+ as.numeric(seasonal_loss_2019)

# scenario2
  (FREH %>% 
                          filter(property_ID %in% filter(property, housing == TRUE)$property_ID, 
                                 date > end_date - years(1), date <= end_date, 
                                 property_ID %in%   (filter(property, scenario_2)$property_ID)) %>% 
                          nrow()/365) #+ sum(GH_total$GH_2019) #+ as.numeric(seasonal_loss_2019)

# scenario3
  (FREH %>% 
                          filter(property_ID %in% filter(property, housing == TRUE)$property_ID, 
                                 date > end_date - years(1), date <= end_date, 
                                 property_ID %in%   (filter(property, scenario_3)$property_ID)) %>% 
                          nrow()/365) #+ sum(GH_total$GH_2019) #+ as.numeric(seasonal_loss_2019)

# scenario4
  (FREH %>% 
                          filter(property_ID %in% filter(property, housing == TRUE)$property_ID, 
                                 date > end_date - years(1), date <= end_date, 
                                 property_ID %in%   (filter(property, scenario_4)$property_ID)) %>% 
                          nrow()/365) #+ sum(GH_total$GH_2019) #+ as.numeric(seasonal_loss_2019)

# scenario5
  (FREH %>% 
                          filter(property_ID %in% filter(property, housing == TRUE)$property_ID, 
                                 date > end_date - years(1), date <= end_date, 
                                 property_ID %in%   (filter(property, scenario_5)$property_ID)) %>% 
                          nrow()/365) #+ sum(GH_total$GH_2019) #+ as.numeric(seasonal_loss_2019)


# Seasonal scenarios

# scenario 1
property %>%
  filter(seasonal_2019,
         !property_ID %in% FREH_season_2019$property_ID,
         scenario_1)

# scenario 2
property %>%
  filter(seasonal_2019,
         !property_ID %in% FREH_season_2019$property_ID,
         scenario_2)

# scenario 3
property %>%
  filter(seasonal_2019,
         !property_ID %in% FREH_season_2019$property_ID,
         scenario_3)

# scenario 4
property %>%
  filter(seasonal_2019,
         !property_ID %in% FREH_season_2019$property_ID,
         scenario_4)

# scenario 5
property %>%
  filter(seasonal_2019,
         !property_ID %in% FREH_season_2019$property_ID,
         scenario_5)

## Daily graphs:

GH %>% 
  filter(date == key_date) %>% 
  count()

# Total housing loss numbers 

# Housing loss 2019
  
loss_2019 <- (FREH %>% 
  filter(property_ID %in% filter(property, housing == TRUE)$property_ID, 
         date > end_date - years(1), date <= end_date) %>% 
  nrow()/365) + sum(GH_total$GH_2019) + as.numeric(seasonal_loss_2019)
  

# Housing loss 2018
  
loss_2018 <- (FREH %>% 
  filter(property_ID %in% filter(property, housing == TRUE)$property_ID,
         date > end_date - years(2), date <= end_date - years(1)) %>% 
  nrow()/365) + sum(GH_total$GH_2018) + (seasonal_loss_2018)

# Housing loss 2017

loss_2017 <- (FREH %>% 
    filter(property_ID %in% filter(property, housing == TRUE)$property_ID,
           date > end_date - years(3), date <= end_date - years(2)) %>% 
    nrow()/365) + sum(GH_total$GH_2017) + seasonal_loss_2017

# YOY increase
# sum(filter(housing_loss, date == end_date)$`Housing units`) /
#   sum(filter(housing_loss, date == end_date - years(1))$`Housing units`)


## SCENARIO ANALYSIS ###########################################################

# Re-do principal_res for all of 2019
# property <-
#   property %>%
#   select(-(principal_res_2019:principal_res_2017))
# 
# 
# property <-
#   property %>%
#   strr_principal_residence(daily, FREH, GH, "2019-01-01", "2019-12-31",
#                            principal_res_2019, 0.5) %>%
#   strr_principal_residence(daily, FREH, GH, "2018-01-01", "2018-12-31",
#                            principal_res_2018, 0.5) %>%
#   strr_principal_residence(daily, FREH, GH, "2017-01-01", "2017-12-31",
#                            principal_res_2017, 0.5)

# Total nights "R" in 2019
daily %>%
  filter(housing, date > end_date - years(1), status == "R") %>%
  count(property_ID) %>% 
  summarize(sum(n))


daily %>% 
  filter(housing, date > end_date - years(1), status == "R") %>% 
  count(property_ID) %>% 
  left_join(property %>% 
              filter(housing, created <= end_date, scraped > end_date - years(1)) %>% 
              filter(scenario_1),
            .) %>% 
  pull(n) %>% 
  sum(na.rm = TRUE)

# Remaining listings (scenario_x = TRUE)

sc1_prop <- 
  property %>% 
  filter(housing, created <= end_date, scraped > end_date - years(1)) %>% 
  filter(scenario_1)

sc2_prop <- 
  property %>% 
  filter(housing, created <= end_date, scraped > end_date - years(1)) %>% 
  filter(scenario_2)

sc3_prop <- 
  property %>% 
  filter(housing, created <= end_date, scraped > end_date - years(1)) %>% 
  filter(scenario_2)

sc4_prop <- 
  property %>% 
  filter(housing, created <= end_date, scraped > end_date - years(1)) %>% 
  filter(scenario_2)

sc5_prop <- 
  property %>% 
  filter(housing, created <= end_date, scraped > end_date - years(1)) %>% 
  filter(scenario_2)

# Scenario 1
daily %>% 
  filter(housing, date > end_date - years(1), status == "R") %>% 
  filter(!property_ID %in% sc1_prop$property_ID)

# Scenario 2
daily %>% 
  filter(housing, date > end_date - years(1), status == "R") %>% 
  filter(!property_ID %in% sc2_prop$property_ID)

# Scenario 3
daily %>% 
  filter(housing, date > end_date - years(1), status == "R") %>% 
  filter(!property_ID %in% sc3_prop$property_ID)

# Scenario 4
daily %>% 
  filter(housing, date > end_date - years(1), status == "R") %>% 
  filter(!property_ID %in% sc4_prop$property_ID)

# Scenario 5
daily %>% 
  filter(housing, date > end_date - years(1), status == "R") %>% 
  filter(!property_ID %in% sc5_prop$property_ID)


# Table calculations (# missing nights divided by remaining listings)
(56664 - 36444) / 438
(56664 - 33488) / 497
(56664 - 33500) / 469
(56664 - 31200) / 519
(56664 - 28400) / 542




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

## Function to get comparisons
reserved_vs_pr <-
  function(property, daily, start_date, end_date, group_var, field_name) {
    start_date <- as.Date(start_date, origin = "1970-01-01")
    end_date <- as.Date(end_date, origin = "1970-01-01")
    daily %>%
      filter(date >= start_date, date <= end_date, status == "R") %>%
      count(!! field_name := property_ID %in%
              filter(property, !! group_var == TRUE)$property_ID)
  }

sd_vec <- c("2019" = "2019-04-01", "2018" = "2018-04-01", "2017" = "2017-04-01")
ed_vec <- c("2019-09-30", "2018-09-30", "2017-09-30")
gv_vec <- vars(principal_res_2019, principal_res_2018, principal_res_2017)

## Use the function on all years to make a df
# reserved_vs_pr_all <-
pmap_dfr(list(sd_vec, ed_vec, gv_vec), ~{
  reserved_vs_pr(property, daily, ..1, ..2, ..3, "principal_res")
}, .id = "year")









## Save files #####################################
save(active_listings_filtered, file = "data/active_listings_filtered.Rdata")
save(property, file = "data/charlottetown_property.Rdata")
#save(housing_loss, file = "data/housing_loss.Rdata")
#save(airbnb_neighbourhoods, file = "data/airbnb_neighbourhoods.Rdata")
save(principal_res, file = "data/principal_res.Rdata")
#save(urban_rural, file = "data/urban_rural.Rdata")
#save(neighbourhoods, file = "data/neighbourhoods.Rdata")




