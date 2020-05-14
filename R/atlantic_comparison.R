library(upgo)

upgo_connect()

atlantic_properties <- 
  property_all %>% 
  filter(country == "Canada",
         city %in% c("Halifax Regional Municipality", "St. John's", "Charlottetown", "Moncton", "Lunenberg")) %>% 
  collect()

atlantic_properties <- 
  atlantic_properties %>% 
  filter(scraped >= "2019-01-01")

atlantic_daily <- 
  daily_all %>% 
  filter(property_ID %in% !! atlantic_properties$property_ID, 
         start_date >= "2019-01-01") %>% 
  collect()

atlantic_daily <- 
  atlantic_daily %>% 
  strr_expand()


atlantic_properties %>% 
  filter(city == "St. John's") %>% 
  count(region)


atlantic_all <- 
  property_all %>% 
  filter(country == "Canada",
         region %in% c("Nova Scotia", "New Brunswick", "Newfoundland and Labrador", "Prince Edward Island")) %>% 
  collect()

lb <- 
  atlantic_all %>% 
  filter(str_detect(city, "Lunenburg"))
  
lb_daily <- 
  daily_all %>% 
  filter(property_ID %in% !! lb$property_ID, start_date >= "2019-01-01") %>% 
  collect()

lb_daily <- 
  lb_daily %>% strr_expand()


atlantic_properties %>% 
  bind_rows(lb) %>% 
  filter(created <= "2020-01-01", scraped >= "2019-01-01", housing) %>% 
  group_by(city) %>% 
  count()

atlantic_daily %>% 
  bind_rows(lb_daily) %>% 
  filter(status == "R", housing == TRUE, date >= created, date <= scraped) %>% 
  group_by(city) %>% 
  summarize(revenue = sum(price) * exchange_rate)


property_all %>% 
  filter(country == "Canada", city == "St. John's") %>% 
  collect() %>% 
  count(region)



