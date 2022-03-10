# random sampling of survey points in Metropolitan perth, weighted by population
mb <- st_read("data/raw/MB_2016_WA.shp")

counts <- read_csv(
  "data/raw/2016\ census\ mesh\ block\ counts.csv",
  col_types = cols(
    MB_CODE_2016 = col_character(),
    MB_CATEGORY_NAME_2016 = col_character(),
    AREA_ALBERS_SQKM = col_double(),
    Dwelling = col_double(),
    Person = col_double(),
    State = col_double()
  )
)

# only keep inner metro SA3s
inner_metro_sa3s <- c(
  "Bayswater - Bassendean",
  "Perth City",
  "Belmont - Victoria Park", 
  "Cottesloe - Claremont",
  "Stirling",
  "Canning",
  "Melville",
  "Fremantle",
  "South Perth" 
)

wa <- mb %>%
  left_join(
    counts,
    by = c("MB_CODE16" = "MB_CODE_2016")
  )

greater_perth <- wa %>%
  # subset to inner metropolitan Perth, excluding Wadjemup/Rottnest
  filter(
    GCC_NAME16 == "Greater Perth",
  )

perth_inner_metro <- greater_perth %>%
  # subset to inner metropolitan Perth, excluding Wadjemup/Rottnest
  filter(
    SA3_NAME16 %in% inner_metro_sa3s,
    SA1_MAIN16 != "50702116525"
  )

# plot the areas included
perth_inner_metro %>%
  rename(
    `Level 3 Statistical Area` = SA3_NAME16
  ) %>%
  ggplot() +
  geom_sf(
    fill = grey(0.9),
    data = greater_perth,
    lwd = 0.1,
    colour = "white"
  ) +
  geom_sf(
    aes(
      fill = `Level 3 Statistical Area`
    ),
    lwd = 0
  ) +
  theme_minimal()

ggsave("figures/study_region.png",
       width = 8,
       height = 10,
       bg = "white")


# SA3 populations
perth_inner_metro %>%
  group_by(SA3_NAME16) %>%
  summarise(
    Person = sum(Person)
  )

# total population
perth_inner_metro %>%
  st_drop_geometry() %>%
  summarise(
    Person = sum(Person)
  )


# we are aiming for 250 households, but can sample 3-4 households per location,
# so divide by that and round up
n_clusters <- ceiling(250 / 3.5)

# set the RNG seed for reproducibility (file not commited to ensure security)
.Random.seed <- readRDS("rng_seed.RDS")

# these meshblocks
cluster_meshblocks <- sample(
  perth_inner_metro$MB_CODE16,
  size = n_clusters,
  prob = perth_inner_metro$Person * perth_inner_metro$Dwelling > 0
)

# sample one point at random within each meshblocks
cluster_points <- perth_inner_metro %>%
  filter(MB_CODE16 %in% cluster_meshblocks) %>%
  st_sample(
    size = rep(1, nrow(.))
  )

# check the numbers
n_distinct(cluster_meshblocks)
n_distinct(cluster_points)
n_clusters

# plot locations of sampled meshblocks
perth_inner_metro %>% mutate(
  sampled = MB_CODE16 %in% cluster_meshblocks
) %>%
  ggplot(
    aes(
      fill = sampled,
    ),
  ) +
  geom_sf(
    lwd = 0
  ) +
  geom_sf(
    fill = "blue",
    alpha = 0.5,
    data = cluster_points
  ) +
  scale_fill_manual(
    values = c(grey(0.9), "black")
  ) +
  theme_minimal()

cluster_points %>%
  st_as_sf() %>%
  mutate(
    wave_number = 1,
    cluster_number = row_number(),
    cluster_id = sprintf(
      "%02d-%03d",
      wave_number,
      cluster_number
    ),
    .before = everything()
  ) %>%
  dplyr::select(
    Name = cluster_id
  ) %>%
  st_write(
    "outputs/pcis_points_wave_1.kml",
    driver = "kml",
    append = FALSE
  )
  
