
library(tidyverse)
library(sf)
library(tidycensus)
library(janitor)

# Census API key
census_api_key(Sys.getenv("f96ec8f8b2bf818ad9f07ad65c17dafb73f457"), install = FALSE)

# Load Airbnb data
airbnb <- read_csv("/Users/luca/Downloads/listings (1).csv.gz") %>%
  clean_names() %>%
  filter(!is.na(latitude), !is.na(longitude))

# Keep only regression-relevant variables
vars_keep <- c(
  "price",
  "latitude",
  "longitude",
  "accommodates",
  "bedrooms",
  "beds",
  "bathrooms",
  "room_type",
  "property_type",
  "neighbourhood_cleansed",
  "neighbourhood_group_cleansed",
  "number_of_reviews",
  "reviews_per_month",
  "review_scores_rating",
  "minimum_nights",
  "instant_bookable",
  "has_availability"
)

airbnb_data <- airbnb %>%
  select(any_of(vars_keep))

# Convert to sf
airbnb_sf <- st_as_sf(
  airbnb_data,
  coords = c("longitude", "latitude"),
  crs = 4326,
  agr = "constant"
)

# Load Dallas census tracts (latest available)
dallas_tracts <- get_acs( #Remember this is 5 year estimate, and we use it because we lag 2 years behind 
  geography = "tract",
  state = "TX",
  county = "Dallas",
  variables = "B01003_001",
  geometry = TRUE,
  year = 2023,
  survey = "acs5"
)

# Match CRS
airbnb_sf <- st_transform(airbnb_sf, st_crs(dallas_tracts))

# Spatial join
airbnb_in_dallas <- st_join(
  airbnb_sf,
  dallas_tracts,
  join = st_intersects
) %>%
  filter(!is.na(estimate))  # keep only Dallas listings

airbnb_in_dallas <- airbnb_in_dallas %>% #spreadsheet of dallas airbnbs with price and characteristics of surrounding neighboorhood
  mutate(
    price = as.numeric(gsub("[$,]", "", price)),
    log_price = log(price),
    log_population = log(estimate)
  ) %>%
  st_drop_geometry()

write_csv(airbnb_in_dallas, "/Users/luca/Downloads/airbnb_dallas_cleaned.csv")

