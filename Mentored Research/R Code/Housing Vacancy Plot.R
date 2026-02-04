

# -------------------------------
# 0. Load libraries
# -------------------------------

library(tidyverse)
library(sf)
library(tidycensus)
library(ggplot2)
library(ggspatial)

# -------------------------------
# 1. Census API Key
# -------------------------------
census_api_key(Sys.getenv("f96ec8f8b2bf818ad9f07ad65c17dafb73f457"), install = FALSE)

# -------------------------------
# 2. Load tract-level Airbnb data
# -------------------------------
acs_vars <- c(
  total_units = "B25002_001",
  vacant_units = "B25002_003",
  population = "B01003_001"
)

dallas_tracts <- get_acs(
  geography = "tract",
  state = "TX",
  county = "Dallas",
  variables = acs_vars,
  year = 2023,
  survey = "acs5",
  geometry = TRUE
) %>%
  select(GEOID, variable, estimate, geometry) %>%
  pivot_wider(
    names_from = variable,
    values_from = estimate
  ) %>%
  mutate(
    vacancy_rate = vacant_units / total_units,
    units_per_capita = total_units / population
  )
# -------------------------------
# Create the ggplot object
# -------------------------------
housing_map <- ggplot(dallas_tracts) +
  geom_sf(
    aes(fill = vacancy_rate),
    color = "#6b5a1e",   # dark gold outline
    size = 0.5
  ) +
  scale_fill_gradient(
    low  = "#fff4cc",   # pale yellow
    high = "#c9a227",   # strong Drover gold
    name = "Vacancy Rate",
    labels = scales::percent_format(accuracy = 1)
  ) +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = north_arrow_fancy_orienteering
  ) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Housing Availability meassured through Vacancy Rates in Dallas",
    subtitle = "Vacancy Rates was calculated by dividing the available rental appartments by the total appartments",
    caption = "Data: ACS 2019–2023 5-year estimates"
  ) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    legend.position = "right"
  )

# -------------------------------
# Save as PDF
# -------------------------------
ggsave(
  filename = "/Users/luca/Desktop/Mentored Research/TAbles/Housingvacancy_dallas_map.pdf",
  plot = housing_map,   
  width = 10,
  height = 8
)

# -------------------------------
# Print the map in RStudio
# -------------------------------
print(housing_map)
