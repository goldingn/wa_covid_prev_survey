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


perth_inner_metro <- wa %>%
  # subset to inner metropolitan Perth, excluding Wadjemup/Rottnest
  filter(
    GCC_NAME16 == "Greater Perth",
    str_starts(SA4_NAME16, "Perth"),
    SA3_NAME16 %in% inner_metro_sa3s,
    SA1_MAIN16 != "50702116525"
  )


# plot the areas included
perth_inner_metro %>%
  ggplot(
    aes(
      fill = SA3_NAME16
    ),
  ) +
  geom_sf(
    lwd = 0
  ) +
  theme_minimal()

perth_inner_metro %>%
  group_by(SA3_NAME16) %>%
  summarise(
    Person = sum(Person)
  )

set.seed(2022-02-16)
meshblocks_250 <- sample(
  perth_inner_metro$MB_CODE16,
  size = 250,
  prob = perth_inner_metro$Person * perth_inner_metro$Dwelling > 0
)

n_distinct(meshblocks_250)

# plot locations of sampled meshblocks
perth_inner_metro %>% mutate(
  sampled = MB_CODE16 %in% meshblocks_250
) %>%
  ggplot(
    aes(
      fill = sampled,
    ),
  ) +
  geom_sf(
    lwd = 0
  ) +
  scale_fill_manual(
    values = c(grey(0.9), "black")
  ) +
  theme_minimal()

# pick a random point within each meshblock


