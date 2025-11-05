##***************************
##  Assignment 1
##  Lusia Lee
##***************************

## package used ----
library(tidyverse)
library(sf)
library(broom)
library(dplyr)
library(jtools)
library(ggplot2)
#

## Data Cleaning and Preparation ----
vespidae_raw <- read_tsv("C:/Users/95joo/OneDrive/Documents/BINF-6210/Data/result.tsv")

# minimun value set up
min_bin <-10

Min_area <- 0.001 # (This is 0.001 km^2)

# filtering the data
vespidae_clean <- vespidae_raw %>%
  # extract coordination to latitude and longitude
  mutate(coord = as.character(coord)) %>%
  tidyr::extract(
    col = coord,
    into = c("lat", "lon"),
    regex = "\\[(-?[0-9.]+),\\s*(-?[0-9.]+)\\]",
    convert = TRUE
  ) %>%
  # select relevant columns only
  select(bin_uri, species, lat, lon, country_iso) %>%
  # filter out NA value rows
  filter(
    !is.na(bin_uri),
    !is.na(lat),
    !is.na(lon)
  ) %>%
  # filter only north american country
  filter(country_iso %in% c("CA", "US", "MX")) %>%
  # number of records per bin and filter based on the minimum value
  group_by(bin_uri) %>%
  add_count(name = "records_per_bin") %>%
  filter(records_per_bin >= min_bin) %>%
  # Ungroup to be safe for next steps
  ungroup()

# calculate the range
vespidae_range <- function(vespidae_clean) {
  tryCatch({
    area_m2 <- vespidae_clean %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
      st_union() %>%                                  
      st_convex_hull() %>%                          
      st_transform(crs = 102008) %>%                    
      st_area()                                 
    # Convert from m^2 to km^2
    as.numeric(area_m2 / 1000000)
  }, error = function(e) {
    NA_real_
  })
}

## Calculate Variables for Hypothesis Testing ----
bin_summary_1 <- vespidae_clean %>%
  group_by(bin_uri) %>%
  summarise(
    # Response variable: Latitudinal Range
    latitudinal_range = max(lat) - min(lat),
    # Predictor variable: Median Latitude
    median_latitude = median(lat),
    n_records = n()
  )

bin_summary_2 <- vespidae_clean %>%
  group_by(bin_uri) %>%
  # 1. First, remove groups with < 3 unique points
  filter(n_distinct(paste(lat, lon)) >= 3) %>%
  mutate(median_latitude = median(lat)) %>%
  nest(points = c(lat, lon)) %>%
  mutate(range_area_km2 = map_dbl(points, vespidae_range)) %>%
  # 2. Remove any NAs (from other errors)
  filter(!is.na(range_area_km2)) %>%
  # 3. NEW: Remove all co-linear "zero" areas
  filter(range_area_km2 > Min_area) %>%
  select(bin_uri, median_latitude, range_area_km2) %>%
  ungroup()

## Run Statistical Test ----
rapoport_model_1 <- lm(latitudinal_range ~ median_latitude, data = bin_summary_1)
summary(rapoport_model_1)

rapoport_model_2<- lm(log(range_area_km2) ~ median_latitude, data = bin_summary_2)
summary(rapoport_model_2)

## Visualize the Result ----
summ(rapoport_model_1)
summ(rapoport_model_2)

rapoport_plot_1 <- ggplot(bin_summary_1, aes(x = median_latitude, y = latitudinal_range)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "Testing Rapoport's Rule in North American Vespidae",
    subtitle = "Each point represents one BIN (n >= 10 records)",
    x = "Median Latitude of BIN (°N)",
    y = "Latitudinal Range of BIN (max - min latitude)"
  ) +
  theme_minimal()

print(rapoport_plot_1)

rapoport_plot_2 <- ggplot(bin_summary_2, aes(x = median_latitude, y = log(range_area_km2))) +
  geom_point(alpha = 0.6, color = "purple") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "Testing Rapoport's Rule in North American Vespidae",
    subtitle = "Each point represents one BIN (n >= 10 records)",
    x = "Median Latitude of BIN (°N)",
    y = "log(Minimum Convex hull of BIN) (km^2)"
  ) +
  theme_minimal()

print(rapoport_plot_2)
