## Ward reallocations

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


sc3_reallocation <- 
  wards %>% 
  mutate(area = st_area(geometry)) %>% 
  st_intersection(
    summarize(group_by(lwgeom::st_make_valid(sc3_zones), `STRs allowed`))
    ) %>% 
  mutate(dwellings = dwellings * st_area(geometry) / area)


sc5_reallocation <- 
  wards %>% 
  mutate(area = st_area(geometry)) %>% 
  st_intersection(
    summarize(group_by(lwgeom::st_make_valid(sc5_zones), `STRs allowed`))
  ) %>% 
  mutate(dwellings = dwellings * st_area(geometry) / area)

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
      scale_fill_viridis_c(name = "STR housing loss",
                           limits = c(0., 0.02),
                           oob = scales::squish,
                           labels = scales::percent) +
      ggtitle(.x$ward[1]) +
      theme_void() +
      theme(legend.position = "right")
  }) %>% 
  wrap_plots() +
  guide_area() +
  plot_layout(guides = "collect")



excess_capacity_sc3 <- 
  housing_loss_listings %>% 
  st_join(sc3_reallocation, ., left = TRUE) %>% 
  group_by(ward, STRs.allowed) %>% 
  summarize(housing_loss = n(),
            dwellings = units::drop_units(mean(dwellings)),
            density = n() / sum(dwellings)) %>% 
  st_drop_geometry() %>% 
  summarize(
    listings_to_reassign = housing_loss[STRs.allowed == "Principal residence only, no apartments"],
    capacity = ceiling(density[STRs.allowed == "Commercial operations allowed"] * 0.5 * 
                         dwellings[STRs.allowed == "Commercial operations allowed"])
  )



excess_capacity_sc5 <- 
  housing_loss_listings %>% 
  st_join(sc5_reallocation, ., left = TRUE) %>% 
  group_by(ward, STRs.allowed) %>% 
  summarize(housing_loss = n(),
            dwellings = units::drop_units(mean(dwellings)),
            density = n() / sum(dwellings)) %>% 
  st_drop_geometry() %>% 
  summarize(
    listings_to_reassign = housing_loss[STRs.allowed == "Principal residence only, apartments allowed"],
    capacity = ceiling(density[STRs.allowed == "Commercial operations allowed"] * 0.5 * 
      dwellings[STRs.allowed == "Commercial operations allowed"])
  )

sum(excess_capacity_sc3$capacity)
sum(excess_capacity_sc5$capacity)
