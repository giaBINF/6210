##***************************
## 6210 ASSIGNMENT 1
##
## Gia Ly
##
## 2024-10-13
##
##***************************

## _ Packages used -------
library(stats)
library(tidyverse)
conflicted::conflict_prefer("filter", "dplyr")
library(viridis)
library(vegan)
library(ggplot2)
library(dplyr)
theme_set(theme_light())

#Start-up ends here

Columbiformes <- read_tsv("https://portal.boldsystems.org/api/documents/eAErSaywyi9KSS2ycs7PKc1NykzLL8pNLbbOyczNLElNAQDD8QxY/download?format=tsv")

Columbiformes <- read_tsv("../data/result.tsv")

#Checking data
class(Columbiformes) 
str(Columbiformes)
summary(Columbiformes)
names(Columbiformes)

#Renaming to country for easier reading
dfColumbi <- rename(Columbiformes, country = `country/ocean`)

##QUESTION: Are species of columbiformes more diverse in North America or South America?

##Plot 1: Accumulation Curve
dfBINs_by_country <- dfColumbi %>%
  group_by(country, bin_uri) %>%
  count(bin_uri)

#Remove NAs
dfBINs_by_country_na_rm <- dfBINs_by_country %>%
  filter(!is.na(country)) %>%
  filter(!is.na(bin_uri))

#Spreading data
dfBINs_spread_by_country <- pivot_wider(data = dfBINs_by_country_na_rm, names_from = bin_uri, values_from = n)

#Converting NAs to 0
dfBINs_spread_by_country[is.na(dfBINs_spread_by_country)] <- 0

#Setting country outside of data frame so that x is numeric
dfBINs_spread_by_country <- dfBINs_spread_by_country %>%
  remove_rownames %>%
  column_to_rownames(var = "country")

#Accum curve analysis
AccumCurve <- specaccum(dfBINs_spread_by_country)

#Accucurve plot
plot(AccumCurve, xlab = "Countries Sampled", ylab = "BIN Richness")

#Sorting data for bar plot
north_america <- c("United States", "Canada", "Mexico")
south_america <- c("Argentina", "Peru", "Brazil", "Guyana", "Venezuela")

#Create "continent" column 
dfsortedcountry <- dfColumbi %>%
  count(country, sort = TRUE) %>%
  filter(!is.na(country),
         country %in% c(north_america, south_america)) %>%
  mutate(Continent = case_when(
    country %in% north_america ~ "North America",
    country %in% south_america ~ "South America"
  ))

##Plot 2: Figuring out total amount of recorded BINs in each continent with a bar plot
dfBINs_spread_by_region <- pivot_wider(data = dfsortedcountry, names_from = country, values_from = n)

#Turning NAs into 0 
dfBINs_spread_by_region[is.na(dfBINs_spread_by_region)] <- 0

#Puts "Continent" outside of data frame
dfBINs_spread_by_region %>%
  remove_rownames %>%
  column_to_rownames(var = "Continent")

bar_plot <- dfsortedcountry %>%
  ggplot(aes(x = reorder(Continent, n), y = n, fill = Continent)) +
  geom_bar(stat = "identity") +
  labs(title = "Columbiformes Records by Continent",
       x = "Continent", y = "Frequency") +
  theme_minimal()
print(bar_plot)

#Creating "continent" column
df_unique_BINs <- dfColumbi %>%
  mutate(continent = case_when(
    country %in% north_america ~ "North America",
    country %in% south_america ~ "South America",
    TRUE ~ NA_character_
  ))

#Sorting unique BIN counts
dfunique_BINs_by_country <- df_unique_BINs %>%
  filter(!is.na(bin_uri), !is.na(continent)) %>%
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