#### CHARLOTTETOWN FIGURES #####################################################

library(patchwork)


### Colour palette #############################################################

col_palette <- 
  c("#601F5F", "#89BD9E", "#46B1C9", "#8797B2", "#7E8D85")



### FIGURE 1 - active listings #################################################

active_listings_type <- 
  daily %>% 
  filter(housing, status != "U") %>% 
  count(date, listing_type) %>% 
  mutate(n = if_else(date <= "2017-05-31" & listing_type == "Entire home/apt", 
                     # Adjusted to account for addition of HA on 2017-06-01
                     n + 40, as.numeric(n)))

active_listings_graph <-
  daily %>% 
  filter(housing, status != "U") %>% 
  count(date) %>% 
  # Numbers adjusted to account for addition of Homeaway on 2017-06-01
  mutate(n = if_else(date <= "2017-05-31", n + 48, as.numeric(n)),
         n = data.table::frollmean(n, 7)) %>% 
  ggplot() +
  geom_line(aes(date, n), colour = "black", size = 1.5) +
  geom_line(data = active_listings_type, aes(date, n, colour = listing_type),
            size = 0.75) +
  scale_y_continuous(name = NULL, label = scales::comma) +
  scale_x_date(name = NULL, limits = c(as.Date("2017-01-01"), NA)) +
  scale_colour_manual(name = "", values = col_palette[1:3]) +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(family = "Futura"),
        )

ggsave("output/figure_1.pdf", plot = active_listings_graph, width = 8, 
       height = 5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figure_1.pdf")



### FIGURE 2 - Charlottetown maps ##############################################

wards_map <-
  property %>%
  filter(housing, created <= key_date, scraped >= key_date) %>%
  st_drop_geometry() %>%
  count(ward) %>%
  left_join(wards, .) %>%
  ggplot() +
  geom_sf(
    aes(fill = n / dwellings), 
    lwd = 1, 
    colour = "white") +
  geom_sf_label(
    aes(label = ward), 
    size = 1.8, 
    family = "Futura",
    #alpha = 0.5,
    fill = alpha("white", 0.6)) +
  scale_fill_gradientn(
    colors = col_palette[c(2, 1)],
    na.value = "grey80",
    limits = c(0, 0.1),
    oob = scales::squish,
    labels = scales::percent) +
  guides(fill = guide_colorbar(title = "STRs/dwelling")) +
  theme_void() +
  theme(legend.position = "right",
        text = element_text(family = "Futura", face = "plain"),
        legend.title = element_text(family = "Futura", face = "bold", 
                                    size = 10),
        legend.text = element_text(family = "Futura", size = 10)
        )

DA_map <- 
  property %>%
  filter(housing, created <= key_date, scraped >= key_date) %>%
  st_drop_geometry() %>%
  count(GeoUID) %>%
  left_join(DAs, .) %>%
  st_intersection(ward_geometries) %>% 
  ggplot() +
  geom_sf(aes(fill = n / dwellings), lwd = 0, colour = "white") +
  scale_fill_gradientn(colors = col_palette[c(2, 1)],
                       na.value = "grey80",
                       limits = c(0, 0.1),
                       oob = scales::squish,
                       labels = scales::percent) +
  guides(fill = guide_colorbar(title = "STRs/dwelling")) +
  theme_void() +
  theme(legend.position = "right",
        text = element_text(family = "Futura", face = "plain"),
        legend.title = element_text(family = "Futura", face = "bold", 
                                    size = 10),
        legend.text = element_text(family = "Futura", size = 10)
  )

listings_map <- 
  wards_map + DA_map + plot_layout(guides = "collect")

ggsave("output/figure_2.pdf", plot = listings_map, width = 8,
       height = 4, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figure_2.pdf")


### FIGURE 3 - seasonality graph ###############################################

seasonality_graph <- 
  seasonality_Canada %>% 
  filter(city != 15) %>%
  ggplot() +
  geom_line(
    aes(month, season_book, group = city), 
    lwd = 1.5, 
    colour = alpha("grey10", 0.2)) +
  geom_line(
    aes(month, season_book, group = 1), 
    data = seasonality, 
    lwd = 2,
    colour = col_palette[3]) +
  scale_y_continuous(name = "", labels = scales::percent, limits = c(0, NA)) +
  scale_x_discrete(name = "") +
  theme_minimal() +
  theme(text = element_text(family = "Futura"))

ggsave("output/figure_3.pdf", plot = seasonality_graph, width = 8, 
       height = 5.5, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figure_3.pdf")


### FIGURE 4 - host revenue percentiles graph ##################################

revenue_graph <-
  daily %>%
  filter(housing == TRUE, date > end_date - years(1), status == "R") %>%
  group_by(host_ID) %>%
  summarize(rev = sum(price)*exchange_rate) %>%
  filter(rev > 0) %>%
  summarize(
    `Top 1%`  = sum(rev[rev > quantile(rev, c(0.99))] / sum(rev)),
    `Top 5%`  = sum(rev[rev > quantile(rev, c(0.95))] / sum(rev)),
    `Top 10%` = sum(rev[rev > quantile(rev, c(0.90))] / sum(rev)),
    `Top 20%` = sum(rev[rev > quantile(rev, c(0.80))] / sum(rev))) %>% 
  gather(`Top 1%`, `Top 5%`, `Top 10%`, `Top 20%`, key = "percentile", 
         value = "value") %>% 
  mutate(percentile = factor(percentile, 
                             levels = c('Top 1%', 'Top 5%', 'Top 10%', 
                                        'Top 20%'))
  ) %>% 
  ggplot() +
  geom_bar(aes(percentile, value, fill = percentile), stat = "identity") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = col_palette[1:4]) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        text = element_text(family = "Futura", face = "plain"),
        legend.title = element_text(family = "Futura", face = "bold",
                                    size = 10),
        legend.text = element_text(family = "Futura", size = 10),
        legend.position = "none")

ggsave("output/figure_4.pdf", plot = revenue_graph, width = 8, height = 5, 
       units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figure_4.pdf")


### FIGURE 5 - housing loss ####################################################

housing_graph <-
  housing_loss %>% 
  mutate(`Listing type` = factor(
    `Listing type`, 
    levels = c("Summer listings", "Private room", "Entire home/apt"))) %>% 
  ggplot(aes(`Listing type`)) +
  geom_col(aes(date, `Housing units`, fill = `Listing type`),
           lwd = 0) +
  theme_minimal() +
  scale_y_continuous(name = NULL, label = scales::comma, limits = c(0, 200)) +
  scale_x_date(name = NULL, limits = c(as.Date("2016-10-01"), NA)) +
  scale_fill_manual(values = col_palette[3:1]) +
  theme(legend.position = "bottom", 
        text = element_text(family = "Futura", face = "plain"),
        legend.title = element_text(family = "Futura", face = "bold",
                                    size = 10),
        legend.text = element_text(family = "Futura", size = 10)
  )

ggsave("output/figure_5.pdf", plot = housing_graph, width = 8, height = 7, 
       units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figure_5.pdf")


### FIGURE 6 - vacancy rate change #############################################

vacancy_rate <- 
  tibble(Year = c(2017, 2018, 2019, 2019, 2020, 2021, 2022,
                  2017, 2018, 2019, 2020, 2021, 2022),
         `Vacancy rate` = c(rep("Vacancy rate", 3),
                            rep("Vacancy rate (projected)", 4),
                            rep("Vacancy rate with no dedicated STRs", 6)
                            ),
          Value = c(0.010, 0.002, 0.012, 0.012, 0.006, 0.011, 0.020,
                    0.017, 0.018, 0.029, 0.023, 0.031, 0.041))

vacancy_graph <- 
  vacancy_rate %>% 
  ggplot(aes(Year, Value)) +
  geom_line(aes(colour = `Vacancy rate`, linetype = `Vacancy rate`),
            lwd = 2) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "", limits = c(0, 0.045), 
                     labels = scales::percent) +
  scale_colour_manual(values = col_palette[c(1, 1, 3)]) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        text = element_text(family = "Futura", face = "plain"),
        legend.text = element_text(family = "Futura", size = 10))

ggsave("output/figure_6.pdf", plot = vacancy_graph, width = 8, height = 4, 
       units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figure_6.pdf")


### FIGURE 7 - multilistings graph #############################################

ML_summary <- 
  daily %>% 
  group_by(date) %>% 
  summarize(Listings = mean(ML),
            Revenue = sum(price * (status == "R") * ML * exchange_rate, 
                          na.rm = TRUE) / 
              sum(price * (status == "R") * exchange_rate, na.rm = TRUE))

ML_graph <-
  ML_summary %>% 
  gather(Listings, Revenue, key = `Multilisting percentage`, value = Value) %>% 
  ggplot() +
  geom_line(aes(date, Value, colour = `Multilisting percentage`), alpha = 0.2) +
  geom_smooth(aes(date, Value, colour = `Multilisting percentage`), se = FALSE,
              method = "loess", span = 0.25) +
  theme_minimal() +
  scale_y_continuous(name = NULL, label = scales::percent) +
  scale_x_date(name = NULL, limits = c(as.Date("2016-06-01"), NA)) +
  scale_colour_manual(values = col_palette[c(1, 3)]) +
  theme(legend.position = "bottom", 
        text = element_text(family = "Futura", face = "plain"),
        legend.title = element_text(family = "Futura", face = "bold",
                                    size = 10),
        legend.text = element_text(family = "Futura", size = 10)
        )

ggsave("output/figure_7.pdf", plot = ML_graph, width = 8, height = 6, 
       units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figure_7.pdf")

  
### FIGURE 8 - principal residence map #########################################

pr_map <- 
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
    legend.position = "bottom",
    text = element_text(family = "Futura", face = "plain"),
    legend.title = element_text(family = "Futura", face = "bold", size = 10),
    legend.text = element_text(family = "Futura", size = 10)
    )

ggsave("output/figure_8.pdf", plot = pr_map, width = 8, height = 8, 
       units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figure_8.pdf")


### FIGURE 9 - scenario maps ###################################################

library(patchwork)

hotel_zones <- 
  c("C2", "C3", "MUVC", "DMU", "DMS", "DC", "M3", "WF", "MUC", "A", "PZ")

map_theme <- 
  theme_void() +
  theme(legend.position = "right",
        text = element_text(family = "Futura", face = "plain"),
        legend.title = element_text(family = "Futura", face = "bold", 
                                    size = 10),
        legend.text = element_text(family = "Futura", size = 9),
        plot.title = element_text(family = "Futura", face = "bold"),
        legend.key.height = unit(1, "cm")
        )

sc1 <-
  zones %>% 
  mutate(
    `STRs allowed` = factor(
      "Principal residence only, \nno apartments", 
      levels = c("Principal residence only, \nno apartments",
                 "Principal residence only, \napartments allowed",
                 "Commercial operations allowed"
      ))) %>% 
  ggplot() +
  geom_sf(aes(fill = `STRs allowed`), lwd = 0, colour = "transparent") +
  geom_sf(data = filter(property, housing, created <= key_date, 
                        scraped >= key_date),
          size = 0.7, alpha = 0.7, stroke = 0) +
  scale_fill_manual(values = col_palette[2:4], drop = FALSE) +
  ggtitle("Scenario 1") +
  map_theme

sc2 <- 
  zones %>% 
  mutate(
    `STRs allowed` = factor(
      "Principal residence only, \napartments allowed",
      levels = c("Principal residence only, \nno apartments",
                 "Principal residence only, \napartments allowed",
                 "Commercial operations allowed"
      ))) %>% 
  ggplot() +
  geom_sf(aes(fill = `STRs allowed`), lwd = 0, colour = "transparent") +
  geom_sf(data = filter(property, housing, created <= key_date, 
                        scraped >= key_date),
          size = 0.7, alpha = 0.7, stroke = 0) +
  scale_fill_manual(values = col_palette[2:4], drop = FALSE) +
  ggtitle("Scenario 2") +
  map_theme

sc3_zones <- 
  zones %>% 
  mutate(`STRs allowed` = case_when(
    ZONING %in% hotel_zones ~ factor(
      "Commercial operations allowed",
      levels = c("Principal residence only, \nno apartments",
                 "Principal residence only, \napartments allowed",
                 "Commercial operations allowed"
      )),
    TRUE ~ factor(
      "Principal residence only, \nno apartments",
      levels = c("Principal residence only, \nno apartments",
                 "Principal residence only, \napartments allowed",
                 "Commercial operations allowed"
      ))))

sc3 <- 
  sc3_zones %>% 
  ggplot() +
  geom_sf(aes(fill = `STRs allowed`), lwd = 0, colour = "transparent") +
  geom_sf(data = filter(property, housing, created <= key_date, 
                        scraped >= key_date),
          size = 0.7, alpha = 0.7, stroke = 0) +
  scale_fill_manual(values = col_palette[2:4], drop = FALSE) +
  ggtitle("Scenario 3") +
  map_theme

sc4 <- 
  zones %>% 
  mutate(`STRs allowed` = case_when(
    ZONING %in% hotel_zones ~ factor(
      "Commercial operations allowed",
      levels = c("Principal residence only, \nno apartments",
                 "Principal residence only, \napartments allowed",
                 "Commercial operations allowed"
      )),
    TRUE ~ factor(
      "Principal residence only, \napartments allowed",
      levels = c("Principal residence only, \nno apartments",
                 "Principal residence only, \napartments allowed",
                 "Commercial operations allowed"
      )))) %>% 
  ggplot() +
  geom_sf(aes(fill = `STRs allowed`), lwd = 0, colour = "transparent") +
  geom_sf(data = filter(property, housing, created <= key_date, 
                        scraped >= key_date),
          size = 0.7, alpha = 0.7, stroke = 0) +
  scale_fill_manual(values = col_palette[2:4], drop = FALSE) +
  ggtitle("Scenario 4") +
  map_theme


sc5_zones <-
  zones %>% 
  mutate(`STRs allowed` = case_when(
    ZONING %in% hotel_zones ~ factor(
      "Commercial operations allowed",
      levels = c("Principal residence only, \nno apartments",
                 "Principal residence only, \napartments allowed",
                 "Commercial operations allowed"
      )),
    ZONING == "DMUN" ~ factor(
      "Commercial operations allowed",
      levels = c("Principal residence only, \nno apartments",
                 "Principal residence only, \napartments allowed",
                 "Commercial operations allowed"
      )),
    TRUE ~ factor(
      "Principal residence only, \napartments allowed",
      levels = c("Principal residence only, \nno apartments",
                 "Principal residence only, \napartments allowed",
                 "Commercial operations allowed"
      ))))

sc5 <- 
  sc5_zones %>% 
  ggplot() +
  geom_sf(aes(fill = `STRs allowed`), lwd = 0, colour = "transparent") +
  geom_sf(data = filter(property, housing, created <= key_date,
                        scraped >= key_date),
          size = 0.7, alpha = 0.7, stroke = 0) +
  scale_fill_manual(values = col_palette[2:4], drop = FALSE) +
  ggtitle("Scenario 5") +
  map_theme

scenario_map <- 
  sc1 + sc2 + sc3 + sc4 + sc5 + guide_area() + plot_layout(guides = "collect")

ggsave("output/figure_9.pdf", plot = scenario_map, width = 8, height = 6, 
       units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figure_9.pdf")


### FIGURE 10 - ward commercial maps ###########################################

patchwork_layout <- "
ABCDE
FGHIJ
KKKKK
"

housing_loss_listings <-
  FREH %>% 
  filter(date > end_date - years(1)) %>% 
  pull(property_ID) %>% 
  {filter(property, housing, property_ID %in% .)} %>% 
  select(property_ID, geometry) %>% 
  rbind(GH %>% 
          filter(date > end_date - years(1)) %>% 
          group_by(ghost_ID) %>% 
          summarize(geometry = st_centroid(st_union(geometry))) %>% 
          transmute(property_ID = as.character(ghost_ID))
  )

sc5_reallocation <- 
  wards %>% 
  mutate(area = st_area(geometry)) %>% 
  st_intersection(
    summarize(group_by(lwgeom::st_make_valid(sc5_zones), `STRs allowed`))
  ) %>% 
  mutate(dwellings = dwellings * st_area(geometry) / area)

ward_map <- 
  housing_loss_listings %>% 
  st_join(sc5_reallocation, ., left = TRUE) %>% 
  group_by(ward, STRs.allowed) %>% 
  summarize(listing_density = n() / sum(dwellings)) %>% 
  group_split() %>% 
  map(~{
    .x %>% 
      ggplot() +
      geom_sf(aes(fill = units::drop_units(listing_density)), lwd = 0, 
              colour = "transparent") +
      scale_fill_gradientn(
        name = "STR housing loss",
        colors = col_palette[c(2, 3, 1)],
        limits = c(0., 0.02),
        oob = scales::squish,
        labels = scales::percent) +
      ggtitle(.x$ward[1]) +
      theme_void() +
      theme(legend.position = "bottom",
            legend.key.width = unit(1.5, "cm"),
            text = element_text(family = "Futura", face = "plain"),
            legend.title = element_text(family = "Futura", face = "bold", 
                                        size = 10),
            legend.text = element_text(family = "Futura", size = 9),
            plot.title = element_text(family = "Futura", size = 9,
                                      face = "bold"))
  }) %>% 
  wrap_plots() +
  guide_area() +
  plot_layout(design = patchwork_layout, guides = "collect",
              heights = c(3, 3, 1))

ggsave("output/figure_10.pdf", plot = ward_map, width = 8, height = 8, 
       units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figure_10.pdf")


### FIGURE 11 - vacancy rate scenarios #########################################

vacancy_rate_scenarios <- 
  tibble(Year = c(2017, 2018, 2019, 
                  rep(c(2019, 2020, 2021, 2022), 6)),
         `Vacancy rate` = c(rep("Vacancy rate", 3),
                            rep("Vacancy rate (projected)", 4),
                            rep("Scenario 1", 4),
                            rep("Scenario 2", 4),
                            rep("Scenario 3", 4),
                            rep("Scenario 4", 4),
                            rep("Scenario 5", 4)
         ),
         Value = c(0.010, 0.002, 0.012, 
                   0.012, 0.006, 0.011, 0.020,
                   0.012, 0.022, 0.029, 0.039,
                   0.012, 0.021, 0.028, 0.038,
                   0.012, 0.017, 0.024, 0.033,
                   0.012, 0.017, 0.023, 0.033,
                   0.012, 0.014, 0.021, 0.030)) %>% 
  mutate(`Vacancy rate` = factor(`Vacancy rate`, levels = c(
    "Vacancy rate", "Vacancy rate (projected)", "Scenario 1", "Scenario 2",
    "Scenario 3", "Scenario 4", "Scenario 5")))

vacancy_scenario_graph <- 
  vacancy_rate_scenarios %>% 
  ggplot(aes(Year, Value)) +
  geom_line(aes(colour = `Vacancy rate`, linetype = `Vacancy rate`),
            lwd = 1.5) +
  scale_x_continuous(name = "") +
  scale_y_continuous(name = "", limits = c(0, 0.04), 
                     labels = scales::percent) +
  scale_colour_manual(values = col_palette[c(1, 1, 2, 3, 4, 5, 2)]) +
  scale_linetype_manual(values = c(1, rep(5, 6))) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        text = element_text(family = "Futura", face = "plain"),
        legend.text = element_text(family = "Futura", size = 10))

ggsave("output/figure_11.pdf", plot = vacancy_scenario_graph, width = 8, 
       height = 4, units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figure_11.pdf")


### FIGURE 12 - supply modelling ###############################################

supply_table <- 
  tibble(
    scenario = c("All scenarios", "Scenario 1", "Scenario 2", "Scenario 3",
                 "Scenario 4", "Scenario 5"),
    listings = c(0, 438, 497, 469, 519, 542),
    avg_nights = c(0, 46.2, 46.6, 49.4, 49.1, 52.1)
  )

supply_funs_100 <- 
  pmap(supply_table, ~{
  function(x) (56664 - ..2 * ..3 - ..3 * x) / (..2 + x)
}) %>% 
  set_names(supply_table$scenario)

full_list <- 834
full_avg_n <- 56664/834

supply_funs_75 <- 
  pmap(supply_table, ~{
    
    m <- ..2 + (full_list - ..2) * 1/4
    n <- ..3 + (full_avg_n - ..3) * 1/4
    
    function(x) {
      (56664 - (m * n) - (n * x)) / (m + x)
      }}) %>% 
  set_names(supply_table$scenario)

supply_funs_50 <- 
  pmap(supply_table, ~{
    
    m <- ..2 + (full_list - ..2) * 1/2
    n <- ..3 + (full_avg_n - ..3) * 1/2
    
    function(x) {
      (56664 - (m * n) - (n * x)) / (m + x)
    }}) %>% 
  set_names(supply_table$scenario)

supply_df <- 
  pmap_dfr(list(supply_funs_50, supply_funs_75, supply_funs_100), 
           function(a, b, c) {
             tibble(x = 0:800, y50 = a(x), y75 = b(x), y100 = c(x)) %>% 
      mutate(y50 = if_else(y50 >= 0, y50, 0),
             y100 = if_else(y100 >= 0, y100, 0),
             y50_line = if_else(y50 > 0, y50, NA_real_),
             y75_line = if_else(y75 > 0, y75, NA_real_),
             y100_line = if_else(y100 > 0, y100, NA_real_))
  }, .id = "scenario")

supply_main <- 
  ggplot(supply_df) +
  geom_ribbon(aes(x, ymin = y50, ymax = y100, fill = scenario), alpha = 0.1) +
  geom_line(aes(x, y50_line, colour = scenario), linetype = 2) +
  geom_line(aes(x, y75_line, colour = scenario), linetype = 3) +
  geom_line(aes(x, y100_line, colour = scenario)) +
  scale_y_continuous(name = "Additional nights booked per listing") +
  scale_x_continuous(name = "Additional listings") +
  scale_colour_manual(name = "", values = c("transparent", col_palette)) +
  scale_fill_manual(name = "", values = c("transparent", col_palette)) +
  coord_cartesian(ylim = c(0, 100)) +
  facet_wrap(~scenario) +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(family = "Futura", face = "plain"),
        legend.text = element_text(family = "Futura", size = 10),
        strip.text = element_text(family = "Futura", face = "bold", size = 10))

overview <-
  supply_df %>% 
  filter(scenario != "All scenarios") %>% 
  ggplot() +
  geom_line(aes(x, y50_line + 1, colour = scenario), linetype = 2) +
  geom_line(aes(x, y100_line + 1, colour = scenario)) + 
  scale_y_continuous(name = "", limits = c(0, 100)) +
  scale_x_continuous(name = "") +
  scale_colour_manual(values = col_palette) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.line = element_line(colour = "transparent"),
        panel.grid = element_line(colour = "transparent")
        )

supply_layout <- c(
  area(1, 1, 2, 3),
  area(1, 1, 1, 1)
)

supply_graph <- supply_main + overview + plot_layout(design = supply_layout)

ggsave("output/figure_12.pdf", plot = supply_graph, width = 8, height = 5, 
       units = "in", useDingbats = FALSE)

extrafont::embed_fonts("output/figure_12.pdf")


### Calculate minimum night/listings combinations ##############################

supply_funs_100_adjusted <-
  supply_table %>% 
  filter(scenario != "All scenarios") %>% 
  pmap(~{
      
      m <- ..2 / 10
      n <- ..3
      
    function(x) {
      x^2 + ((56664/10 - (m * n) - (n * x)) / (m + x))^2
      }}) %>% 
  set_names(supply_table$scenario[2:6])

supply_funs_75_adjusted <- 
  supply_table %>% 
  filter(scenario != "All scenarios") %>% 
  pmap(~{
    m <- (..2 + (full_list - ..2) * 1/4) / 10
    n <- ..3 + (full_avg_n - ..3) * 1/4
    
    function(x) {
      x^2 + ((56664/10 - (m * n) - (n * x)) / (m + x))^2
      }}) %>% 
  set_names(supply_table$scenario[2:6])

supply_funs_50_adjusted <- 
  supply_table %>% 
  filter(scenario != "All scenarios") %>% 
  pmap(~{
    m <- (..2 + (full_list - ..2) * 1/2) / 10
    n <- ..3 + (full_avg_n - ..3) * 1/2
    
    function(x) {
      x^2 + ((56664/10 - (m * n) - (n * x)) / (m + x))^2
    }}) %>% 
  set_names(supply_table$scenario[2:6])

supply_derivs_100 <- 
  supply_funs_100_adjusted %>% 
  map(Deriv::Deriv)

supply_derivs_75 <- 
  supply_funs_75_adjusted %>% 
  map(Deriv::Deriv)

supply_derivs_50 <- 
  supply_funs_50_adjusted %>% 
  map(Deriv::Deriv)

supply_derivs_100[[5]](21.8)
supply_derivs_75[[5]](16.1)
supply_derivs_50[[5]](10.5)

minima <- 
  tibble(
    scenario = c("Scenario 1", "Scenario 2", "Scenario 3",
                 "Scenario 4", "Scenario 5"),
    listing_100_min = c(306, 267, 275, 244, 219),
    nights_100_min = map2_dbl(listing_100_min, supply_funs_100[2:6], ~.y(.x)),
    listing_75_min = c(224, 196, 200, 179, 160),
    nights_75_min = map2_dbl(listing_75_min, supply_funs_75[2:6], ~.y(.x)),
    listing_50_min = c(144, 128, 129, 117, 104),
    nights_50_min = map2_dbl(listing_50_min, supply_funs_50[2:6], ~.y(.x))
  )


