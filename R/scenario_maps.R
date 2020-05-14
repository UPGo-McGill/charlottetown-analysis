### Scenario maps ########

library(patchwork)


### Scenario 1 #################################################################

sc1 <-
  zones %>% 
  mutate(
    `STRs allowed` = factor(
      "Principal residence only, no apartments", 
      levels = c("Principal residence only, no apartments",
                 "Principal residence only, apartments allowed",
                 "Commercial operations allowed"
                 ))) %>% 
  ggplot() +
  geom_sf(aes(fill = `STRs allowed`), lwd = 0, colour = "transparent") +
  geom_sf(data = filter(property, housing, created <= key_date, scraped >= key_date),
          size = 0.5, alpha = 0.5) +
  scale_fill_manual(values = c("orange", "gold", "green"), drop = FALSE) +
  ggtitle("Scenario 1") +
  theme_void() +
  theme(legend.position = "right")
  


### Scenario 2 #################################################################

sc2 <- 
  zones %>% 
  mutate(
    `STRs allowed` = factor(
      "Principal residence only, apartments allowed",
      levels = c("Principal residence only, no apartments",
                 "Principal residence only, apartments allowed",
                 "Commercial operations allowed"
      ))) %>% 
  ggplot() +
  geom_sf(aes(fill = `STRs allowed`), lwd = 0, colour = "transparent") +
  geom_sf(data = filter(property, housing, created <= key_date, scraped >= key_date),
          size = 0.5, alpha = 0.5) +
  scale_fill_manual(values = c("orange", "gold", "green"), drop = FALSE) +
  ggtitle("Scenario 2") +
  theme_void() +
  theme(legend.position = "right")


### Scenario 3 #################################################################

sc3_zones <- 
  zones %>% 
  mutate(`STRs allowed` = case_when(
    ZONING %in% hotel_zones ~ factor(
      "Commercial operations allowed",
      levels = c("Principal residence only, no apartments",
                 "Principal residence only, apartments allowed",
                 "Commercial operations allowed"
                 )),
    TRUE ~ factor(
      "Principal residence only, no apartments",
      levels = c("Principal residence only, no apartments",
                 "Principal residence only, apartments allowed",
                 "Commercial operations allowed"
      ))))

sc3_zones %>% 
  ggplot() +
  geom_sf(aes(fill = `STRs allowed`), lwd = 0, colour = "transparent") +
  geom_sf(data = filter(property, housing, created <= key_date, scraped >= key_date),
          size = 0.5, alpha = 0.5) +
  scale_fill_manual(values = c("orange", "gold", "green"), drop = FALSE) +
  ggtitle("Scenario 3") +
  theme_void() +
  theme(legend.position = "right")


### Scenario 4 #################################################################

sc4 <- 
  zones %>% 
  mutate(`STRs allowed` = case_when(
    ZONING %in% hotel_zones ~ factor(
      "Commercial operations allowed",
      levels = c("Principal residence only, no apartments",
                 "Principal residence only, apartments allowed",
                 "Commercial operations allowed"
      )),
    TRUE ~ factor(
      "Principal residence only, apartments allowed",
      levels = c("Principal residence only, no apartments",
                 "Principal residence only, apartments allowed",
                 "Commercial operations allowed"
      )))) %>% 
  ggplot() +
  geom_sf(aes(fill = `STRs allowed`), lwd = 0, colour = "transparent") +
  geom_sf(data = filter(property, housing, created <= key_date, scraped >= key_date),
          size = 0.5, alpha = 0.5) +
  scale_fill_manual(values = c("orange", "gold", "green"), drop = FALSE) +
  ggtitle("Scenario 4") +
  theme_void() +
  theme(legend.position = "right")


### Scenario 5 #################################################################

sc5_zones <-
  zones %>% 
  mutate(`STRs allowed` = case_when(
    ZONING %in% hotel_zones ~ factor(
      "Commercial operations allowed",
      levels = c("Principal residence only, no apartments",
                 "Principal residence only, apartments allowed",
                 "Commercial operations allowed"
      )),
    ZONING == "DMUN" ~ factor(
      "Commercial operations allowed",
      levels = c("Principal residence only, no apartments",
                 "Principal residence only, apartments allowed",
                 "Commercial operations allowed"
      )),
    TRUE ~ factor(
      "Principal residence only, apartments allowed",
      levels = c("Principal residence only, no apartments",
                 "Principal residence only, apartments allowed",
                 "Commercial operations allowed"
      ))))

sc5_zones %>% 
  ggplot() +
  geom_sf(aes(fill = `STRs allowed`), lwd = 0, colour = "transparent") +
  geom_sf(data = filter(property, housing, created <= key_date, scraped >= key_date),
          size = 0.5, alpha = 0.5) +
  scale_fill_manual(values = c("orange", "gold", "green"), drop = FALSE) +
  ggtitle("Scenario 5") +
  theme_void() +
  theme(legend.position = "right")



sc1 + sc2 + sc3 + sc4 + sc5 + guide_area() + plot_layout(guides = "collect")

  
property %>% 
  st_drop_geometry() %>% 
  filter(housing, created <= key_date, scraped >= key_date) %>% 
  summarize_at(vars(scenario_1:scenario_5), mean) %>% 
  bind_rows(
    daily %>% 
      filter(housing, date > end_date - years(1), status == "R") %>%
      count(property_ID) %>% 
      left_join(property, .) %>% 
      st_drop_geometry() %>% 
      filter(housing, created <= key_date, scraped >= key_date) %>% 
      summarize_at(vars(scenario_1:scenario_5), ~{
        sum(n * ., na.rm = TRUE) / sum(n, na.rm = TRUE)
      })
  )

daily %>% 
  filter(housing, date > end_date - years(1), status == "R") %>%
  count(property_ID) %>% 
  left_join(property, .) %>% 
  st_drop_geometry() %>% 
  filter(housing, created <= key_date, scraped >= key_date) %>% 
  summarize_at(vars(scenario_1:scenario_5), ~{
    sum(n * ., na.rm = TRUE) / sum(n, na.rm = TRUE)
  })
  





property %>% 
  st_filter(city) %>% 
  filter(housing, created <= end_date, scraped > end_date - years(1)) %>% 
  ggplot() +
  # geom_sf(data = wards, fill = "white", colour = "transparent", lwd = 0) +
  geom_sf(data = streets, colour = "grey50", lwd = 0.2) +
  geom_sf(aes(colour = principal_res_2019), size = 1, alpha = 0.8) +
  scale_colour_manual(name = "Principal residence", 
                      values = col_palette[c(1,3)]) +
  guides(colour = 
           guide_legend(override.aes = list(fill = col_palette[c(1,3)],
                                            alpha = 1))
         ) +
  theme_void() +
  theme(
    legend.position = "bottom")




GH %>% 
  filter(date > end_date - years(1)) %>% 
  st_filter(filter(sc3_zones, `STRs allowed` == "Commercial operations allowed")) %>% 
  st_drop_geometry() %>% 
  summarize(GH_housing_loss = sum(housing_units) / 365)


GH %>% 
  filter(date > end_date - years(1)) %>% 
  st_filter(filter(sc5_zones, `STRs allowed` == "Commercial operations allowed")) %>% 
  st_drop_geometry() %>% 
  summarize(GH_housing_loss = sum(housing_units) / 365)




