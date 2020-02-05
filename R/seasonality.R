#### Seasonality analysis ######################################################

# Set up data

seasonality_maker <- function(daily, end_date) { 
  daily %>% 
    filter(date <= end_date, 
           date > ymd(end_date) - years(2)) %>% 
    mutate(yearmonth = zoo::as.yearmon(date)) %>% 
    group_by(yearmonth) %>% 
    summarize(
      bookings = sum(status == "R"),
      rev = sum(price[status == "R"])
      ) %>% 
    mutate(
      month = lubridate::month(yearmonth, label = TRUE), 
      season_book = as.vector(decompose(ts(bookings, frequency = 12),
                                        "multiplicative")$seasonal) / 12,
      season_rev  = as.vector(decompose(ts(rev,      frequency = 12),
                                        "multiplicative")$seasonal) / 12
      ) %>% 
    group_by(month, season_book, season_rev) %>% 
    summarise() %>% 
    ungroup() %>% 
    mutate(month = factor(month, levels=unique(month)))
}

seasonality <- 
  seasonality_maker(daily, end_date)


# Plot
# figure_seasonality <-
  ggplot(seasonality) +
  geom_line(aes(x = month, y = season_book), lwd = 1.8, alpha = 0.8, group = 1) +
  geom_line(aes(x = month, y = season_rev), lwd = 1.8, alpha = 0.8, group = 1,
            colour = "red") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  xlab("Month")+
  ylab("Percentage of revenue")+



### Cross-Canada comparison ####################################################

upgo_connect()

property_Canada <- 
  property_all %>% 
  filter(country == "Canada") %>% 
  collect()

daily_Canada <-
  daily_all %>% 
  filter(property_ID %in% !! property_Canada$property_ID) %>% 
  collect()

upgo_disconnect()

library(future)
plan(multiprocess, workers = 4)

daily_Canada <- 
  daily_Canada %>% 
  strr_expand()

daily_Canada <- 
  daily_Canada %>% 
  filter(date >= created, date <= scraped, status != "U")

save(daily_Canada, file = "daily_Canada.Rdata")


##
daily_Canada %>% filter(city == "Ottawa") %>% seasonality_maker(end_date)


seasonality_Canada <- 
  daily_Canada %>% 
  filter(city %in% c(
    "Calgary", "Edmonton", "Red Deer", "Vancouver", "Surrey", "Burnaby",
    "Richmond", "Abbotsford", "Coquitlam", "Kelowna", "Delta", "Winnipeg",
    "Saint John", "Fredericton", "Moncton", "St. John's", "Toronto", #"Ottawa",
    "Mississauga", "Brampton", "Hamilton", "London", "Markham", "Vaughn",
    "Kitchener", "Windsor", "Richmond Hill", "Burlington", "Greater Sudbury",
    "Oshawa", "Barrie", "St. Catherines", "Guelph", "Cambridge", "Kingston",
    "Thunder Bay", "Waterloo", "Charlottetown", "Montreal", "Quebec", "Laval",
    "Gatineau", "Longueuil", "Sherbrooke", "Saguenay", "Levis",
    "Trois-Rivieres", "Terrebonne", "Saskatoon", "Regina", "Whitehorse")) %>% 
  group_split(region, city) %>% 
  map_dfr(seasonality_maker, end_date, .id = "city")

seasonality_Canada %>% 
  filter(city != 15) %>% 
ggplot() +
  geom_smooth(aes(month, season_book, group = city), lwd = 1.5, alpha = 0.2,
              colour = alpha("grey10", 0.2), se = FALSE) +
  geom_smooth(aes(month, season_book, group = 1), data = seasonality, lwd = 1.5,
            colour = "blue", alpha = 0.8, se = FALSE)


install.packages("gghighlight")

seasonality_Canada %>% 
  filter(month == "Jan") %>% 
  filter(season_book == min(season_book, na.rm = TRUE))

daily_Canada %>% 
  filter(city == "Moncton") %>% filter(status == "R") %>% 
  count(date) %>% 
  ggplot(aes(date, n)) +
  geom_line()


dates <- 
daily_Canada %>% 
  filter(city %in% c(
    "Calgary", "Edmonton", "Red Deer", "Vancouver", "Surrey", "Burnaby",
    "Richmond", "Abbotsford", "Coquitlam", "Kelowna", "Delta", "Winnipeg",
    "Saint John", "Fredericton", "Moncton", "St. John's", "Toronto", #"Ottawa",
    "Mississauga", "Brampton", "Hamilton", "London", "Markham", "Vaughn",
    "Kitchener", "Windsor", "Richmond Hill", "Burlington", "Greater Sudbury",
    "Oshawa", "Barrie", "St. Catherines", "Guelph", "Cambridge", "Kingston",
    "Thunder Bay", "Waterloo", "Charlottetown", "Montreal", "Quebec", "Laval",
    "Gatineau", "Longueuil", "Sherbrooke", "Saguenay", "Levis",
    "Trois-Rivieres", "Terrebonne", "Saskatoon", "Regina", "Whitehorse")) %>% 
  count(region, city, date)

dates %>% 
  filter(date == "2019-07-01")

dates %>% 
  filter(date == "2019-01-01")
