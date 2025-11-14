##***************************
## 6210 ASSIGNMENT 1
##
## Gia Ly
##
## 2024-10-13
##
##***************************

## Edit by Lusia

## _ Packages used -------
library(stats)
library(tidyverse)
conflicted::conflict_prefer("filter", "dplyr")
library(viridis)
library(vegan)
library(ggplot2)
library(dplyr)
library(maps) #Added for map plot
theme_set(theme_light())

#Start-up ends here

#The original URL load is commented out
Columbiformes <- read_tsv("https://portal.boldsystems.org/api/documents/eAErSaywyi9KSS2ycs7PKc1NykzLL8pNLbbOyczNLElNAQDD8QxY/download?format=tsv")

#Columbiformes <- read_tsv("../data/result.tsv")

#Checking data
class(Columbiformes) 
str(Columbiformes)
summary(Columbiformes)
names(Columbiformes)

#Renaming to country for easier reading
dfColumbi <- rename(Columbiformes, country = `country/ocean`)

##QUESTION: Are species of columbiformes more diverse in North America or South America?

#Data Cleaning and Preparation ----

# Define continent vectors
north_america <- c("United States", "Canada", "Mexico")
south_america <- c("Argentina", "Peru", "Brazil", "Guyana", "Venezuela")

# Clean data ONCE to create a base data frame for all plots
dfColumbi_clean <- dfColumbi %>%
  mutate(coord = as.character(coord)) %>%
  tidyr::extract(
    col = coord,
    into = c("lat", "lon"),
    regex = "\\[(-?[0-9.]+),\\s*(-?[0-9.]+)\\]",
    convert = TRUE
  ) %>% # for plot 4
  filter(!is.na(country), !is.na(bin_uri)) %>%
  mutate(continent = case_when( # Grouping your samples
    country %in% north_america ~ "North America",
    country %in% south_america ~ "South America",
    TRUE ~ "Other" # Keep others
  ))

##Plot 1: Accumulation Curve ----

# create data frame of BINs per country
dfBINs_by_country <- dfColumbi_clean %>% #use cleaned data
  group_by(country, bin_uri) %>%
  count(bin_uri)

#Spreading data, converting NAs all at once
dfBINs_spread_by_country <- pivot_wider(data = dfBINs_by_country, names_from = bin_uri, values_from = n, values_fill=0 #convert NAs to 0
                                        ) %>%
  tibble::column_to_rownames(var="country")

#Accum curve analysis
AccumCurve <- specaccum(dfBINs_spread_by_country)

#Accucurve plot
plot(AccumCurve, xlab = "Countries Sampled", ylab = "BIN Richness")

##Plot 2: Figuring out total amount of recorded BINs in each continent with a bar plot ----

## dfBINs_spread_by_region was not used, so I removed

# Filter the data
dfsortedcountry <- dfColumbi_clean %>%
  filter(continent %in% c("North America", "South America")) %>%
  count(continent, sort = TRUE)

# Plot
bar_plot <- dfsortedcountry %>%
  ggplot(aes(x = reorder(continent, n), y = n, fill = continent)) +
  geom_bar(stat = "identity") +
  labs(title = "Columbiformes Records by Continent",
       x = "Continent", y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") # Removed redundant legend

print(bar_plot)

# Statistical test ----

#Sorting unique BIN counts
dfunique_BINs_by_country <- dfColumbi_clean %>%
  filter(continent %in% c("North America", "South America")) %>%
  distinct(country, bin_uri, .keep_all = TRUE) %>%
  group_by(country, continent) %>%
  summarize(unique_bin_count = n(), .groups = "drop")

t.test(unique_bin_count ~ continent, data = dfunique_BINs_by_country)
#There is no statistically significant difference in Columbiformes species diversity (measured by unique BIN counts) between North America and South America in the dataset.

##Plot 3: Box plot with error bars
box_plot <- ggplot(dfunique_BINs_by_country, aes(x = continent, y = unique_bin_count)) +
  stat_boxplot(aes(x = continent, y = unique_bin_count),
    geom = "errorbar", linetype = 1, width = 0.5
  ) +
  geom_boxplot(aes(x = continent, y = unique_bin_count), outlier.shape = 0.5) +
  labs(title = "Columbiformes BIN Diversity Based on Continent", x = "Continent", y = "Unique BIN Count") +
  stat_summary(fun = mean, geom = "point", size = 2)

print(box_plot)

## Plot 4: Geographic Distribution of Samples ----
#exploratory visualization of the data

# Get world map data
world_map <- map_data("world")

# Group data by coordinates and continent
df_coords <- dfColumbi_clean %>%
  group_by(lat, lon, continent) %>%
  summarise(record_count = n(), .groups = "drop")

# Create the map plot
map_plot <- ggplot() +
  # Draw the world map
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), 
               fill = "gray90", color = "gray50", linewidth = 0.3) +
  # Add sample points
  geom_point(data = df_coords, aes(x = lon, y = lat, size = record_count, color = continent), 
             alpha = 0.6) +
  # Use viridis color scale
  scale_color_viridis_d(option = "C") +
  # Set map limits to focus on the Americas
  coord_quickmap(xlim = c(-170, -30), ylim = c(-60, 85)) +
  labs(title = "Geographic Distribution of Columbiformes Samples in BOLD",
       x = "Longitude",
       y = "Latitude",
       color = "Continent",
       size = "Record Count") +
  theme_minimal()

print(map_plot)

#Reference ----
# https://r-graph-gallery.com/map.html
# https://cran.r-project.org/web/packages/maps/index.html
# https://ggplot2.tidyverse.org/