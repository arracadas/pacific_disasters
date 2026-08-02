##########################################
## Pacific Data Viz Challenge 2026      ##
## Disasters in the Pacific Region      ##
## June 2026 - Exploratory Data Analysis ##
##########################################

# Set working directory
# setwd("~/workbench/robin/data viz/pacific disasters")

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(janitor)

#################################
## 1. LOAD ALL THREE DATASETS  ##
#################################
# (DF_SDG_11) Sustainable Development Goal 11 - Sustainable Cities and Communities
# 11.5.1 Number of directly affected persons attributed to disasters
affected_persons <- read.csv('data/pacific_disaster_affected_persons.csv', 
                            stringsAsFactors = FALSE,
                            na.strings = c("", " ", "NA", "#VALUE!"))

# 11.5.2 Direct disaster economic loss, average annual loss
economic_loss <- read.csv('data/pacific_disaster_economic_loss.csv',
                         stringsAsFactors = FALSE,
                         na.strings = c("", " ", "NA", "#VALUE!"))

# Climate Change Indicators: Sea Surface Temperature anomalies, Celsius
sea_temp_anomalies <- read.csv('data/pacific_sea_surface_temperature_anomalies.csv',
                          stringsAsFactors = FALSE,
                          na.strings = c("", " ", "NA", "#VALUE!"))

# UN Population Statistics
# Source: undata
population_stats <- read.csv('data/undata_country_population_stats.csv',
                           stringsAsFactors = FALSE,
                           na.strings = c("", " ", "NA", "#VALUE!"),
                           skip = 1)

# World Bank Population Stats
# Source: World Bank
population_wb_stats <- read.csv('data/world_bank_population_stats.csv',
                                stringsAsFactors = FALSE,
                                na.strings = c("", " ", "NA", "#VALUE!"),
                                header = FALSE,
                                skip = 4,
                                col.names = c("Country Name","Country Code","Indicator Name","Indicator Code","1960","1961","1962","1963","1964","1965","1966","1967","1968","1969","1970","1971","1972","1973","1974","1975","1976","1977","1978","1979","1980",
                                              "1981","1982","1983","1984","1985","1986","1987","1988","1989","1990","1991","1992","1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010",
                                              "2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023","2024","2025","1111")
)



###################################
## World Bank Population Stats   ##
###################################
# Convert wide to long format
population_wb_long <- population_wb_stats %>%
  # Filter out the header row if it exists (e.g., "Country Name", "Indicator Name", etc.)
  filter(!Country.Name %in% c("Country Name", "Indicator Name")) %>%
  # Select only the relevant columns: Country.Code, Country.Name, and year columns
  select(Country.Code, Country.Name, starts_with("X")) %>%
  # Pivot the year columns to long format
  pivot_longer(
    cols = starts_with("X"),
    names_to = "Year",
    values_to = "Population"
  ) %>%
  # Clean the Year column by removing the "X" prefix
  mutate(Year = as.numeric(sub("^X", "", Year))) %>%
  # Reorder columns to match the target dataframe
  select(Country.Code, Country.Name, Year, Population)


# Find pacific islands with different name
# Micronesia (Federated States of), Cook Islands, Niue, Tokelau, Wallis and Futuna
filter(population_wb_long, grepl("Micronesia", Country.Name) & Year == 2015)      # Micronesia, Fed. Sts.
filter(population_wb_long, grepl("Cook", Country.Name))    # not found
filter(population_wb_long, grepl("Niue", Country.Name))    # not found
filter(population_wb_long, grepl("Tokelau", Country.Name)) # not found
filter(population_wb_long, grepl("Wallis", Country.Name))  # not found




##############################
## 2. CHECK MISSING VALUES  ##
##############################
# Function to perform initial data checks
missing_data <- function(data) {
  # Check rows and columns
  cat("Dimensions (rows, cols):", dim(data), "\n\n")
  
  # Print column names
  cat("Column names:", paste(names(data), collapse = ", "), "\n\n")
  
  # Print missing values per column
  cat("Missing values per column:\n")
  missing_counts <- colSums(is.na(data))
  if (sum(missing_counts) > 0) {
    print(missing_counts[missing_counts > 0])
  } else {
    cat("No missing values found.\n")
  }
}

# Run basic checks on all datasets
missing_data(affected_persons)
missing_data(economic_loss)
missing_data(sea_temp_anomalies)
missing_data(population_stats)


# Show data sources
distinct(affected_persons, Data.source, DATA_SOURCE)
distinct(economic_loss, Data.source, DATA_SOURCE)
distinct(population_stats, Source)


# Check years for each Data Source in affected_persons
affected_persons %>%
  filter(DATA_SOURCE == 'United Nations Office for Disaster Risk Reduction (2023)') %>%
  distinct(TIME_PERIOD) %>%
  arrange(TIME_PERIOD)


# Check if duplicate rows for country given the same year and multiple data sources
affected_persons %>%
  filter(TIME_PERIOD == 2015) %>%
  select(DATA_SOURCE, GEO_PICT, Pacific.Island.Countries.and.territories
         ,TIME_PERIOD, OBS_VALUE) %>%
  arrange(GEO_PICT)

# Check distinct values for Series in population
distinct(population_stats, Series)
distinct(population_stats, Region.Country.Area, X)

# Check rows for a given country
population_stats %>%
  filter(X == "Marshall Islands") %>%
  filter(Year == 2025) %>%
  filter(Series == 'Population mid-year estimates (millions)') %>%
  select(Year, X, Value)


##############################
## 3. DATA PREPARATION      ##
##############################
# Clean and simplify the SDG datasets
clean_sdg_data <- function(data) {
  data_clean <- data %>%
    select(
      Pacific_Island = GEO_PICT,
      Country = Pacific.Island.Countries.and.territories,
      Year = TIME_PERIOD,
      Value = OBS_VALUE,
    ) %>%
    mutate(
      Value = as.numeric(gsub(",", "", Value)),
      Year = as.numeric(Year),
      Pacific_Island = as.character(Pacific_Island),
      Country = as.character(Country)
    ) %>%
    filter(!is.na(Value) & !is.na(Year) & !is.na(Country)) %>%
    arrange(Year, Pacific_Island)
  return(data_clean)
}

## Clean data sets
affected_clean <- clean_sdg_data(affected_persons)
economic_clean <- clean_sdg_data(economic_loss)
sea_temp_anomalies_clean <- clean_sdg_data(sea_temp_anomalies)

# Fix Micronesia in Population Stats
population_stats <- population_stats %>%
  mutate(
    X = ifelse(X == "Micronesia (Fed. States of)",
               "Micronesia (Federated States of)",
               X)
  )

# Fix Wallins and Futuna in Population Stats
population_stats <- population_stats %>%
  mutate(
    X = ifelse(X == "Wallis and Futuna Islands",
               "Wallis and Futuna",
               X)
  )

# Prepare unique countries and regions
unique_countries <- distinct(affected_clean, Country) %>% pull(Country)
regions <- c("Melanesia", "Polynesia", "Micronesia")
unique_countries <- c(unique_countries, regions)

# Clean population data for 2025
population_clean <- population_stats %>%
  filter(
    Series == "Population mid-year estimates (millions)",
    Year == "2025",
    X %in% unique_countries,
    !is.na(X)
  ) %>%
  select(
    Country = X,
    Pop_millions = Value
  ) %>%
  mutate(
    Pop_millions = as.numeric(gsub(",", "", Pop_millions))
  )

# Create Pacific Lookup with population stats
# Define region mappings upfront for clarity
region_mapping <- tibble(
  Pacific_Island = c(
    # Melanesia
    "FJ", "NC", "PG", "SB", "VU", "MEL",
    # Micronesia
    "GU", "KI", "MH", "FM", "NR", "MP", "PW", "MIC",
    # Polynesia
    "AS", "CK", "PF", "NU", "WS", "TK", "TO", "TV", "WF", "POL"
  ),
  Region = c(
    rep("Melanesia", 6),
    rep("Micronesia", 8),
    rep("Polynesia", 10)
  )
)

# Build the lookup table
pacific_population <- affected_clean %>%
  select(Pacific_Island, Country) %>%
  distinct() %>%
  # Add region codes for Melanesia, Micronesia, Polynesia
  bind_rows(
    tibble(
      Pacific_Island = c("MEL", "MIC", "POL"),
      Country = c("Melanesia", "Micronesia", "Polynesia")
    )
  ) %>%
  # Join with region mappings
  left_join(region_mapping, by = "Pacific_Island") %>%
  # Add population data
  left_join(population_clean, by = "Country") %>%
  # Select and arrange final columns
  select(Region, Pacific_Island, Country, Pop_millions) %>%
  arrange(Region, Pacific_Island)


# Create final affected persons dataset
pacific_dis_affected <- affected_clean %>%
  rename(Affected_Persons = Value) %>%
  filter(Year >= 2005) %>%
  # Ensure only necessary columns are joined
  left_join(region_mapping, by = "Pacific_Island") %>%
  # Select and arrange final columns
  select(Year, Region, Pacific_Island, Country, Affected_Persons) %>%
  arrange(Year, Region, Pacific_Island)

# Create final pacific disaster economic loss dataset
pacific_dis_economic <- economic_clean %>%
  rename(Economic_Loss = Value) %>%
  filter(Year >= 2005) %>%
  # Join with region mappings
  left_join(region_mapping, by = "Pacific_Island") %>%
  # Select and arrange final columns
  select(Year, Region, Pacific_Island, Country, Economic_Loss) %>%
  arrange(Year, Region, Pacific_Island)


# Create final pacific sea temp anomalies dataset
pacific_sea_temp_anomalies <- sea_temp_anomalies_clean %>%
  rename(Temp = Value) %>%
  filter(Year >= 2005) %>%
  # Join with region mappings
  left_join(region_mapping, by = "Pacific_Island") %>%
  # Select and arrange final columns
  select(Year, Region, Pacific_Island, Country, Temp) %>%
  arrange(Year, Region, Pacific_Island)




######################################
## 4. EXPORT DATAFRAMES             ##
######################################
saveRDS(pacific_population, "./data/pacific_population.rds")
saveRDS(pacific_dis_affected, "./data/pacific_dis_affected.rds")
saveRDS(pacific_dis_economic, "./data/pacific_dis_economic.rds")
saveRDS(pacific_sea_temp_anomalies, "./data/pacific_sea_temp_anomalies.rds")


######################################
## 5. EXPLORATORY DATA ANALYSIS     ##
######################################
# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(janitor)

# Load libraries
pacific_lookup <- readRDS("./data/pacific_lookup.rds")
pacific_dis_affected <- readRDS("./data/pacific_dis_affected.rds")
pacific_dis_economic <- readRDS("./data/pacific_dis_economic.rds")

# Total persons by year by region
# Join pacific_dis_affected with pacific_lookup to get Region
persons_by_year_region <- pacific_dis_affected %>%
  left_join(pacific_lookup, by = "Pacific_Island") %>%
  group_by(Region, Year) %>%
  summarise(Total_Persons = sum(Persons, na.rm = TRUE), .groups = "drop")

# Plot time series for each region
ggplot(persons_by_year_region, aes(x = Year, y = Total_Persons, color = Region)) +
  geom_line(linewidth = 1) +
  geom_point() +
  facet_wrap(~ Region, scales = "free_y", ncol = 1) +
  labs(
    title = "Total Persons Affected by Year and Region",
    x = "Year",
    y = "Total Persons",
    color = "Region"
  ) +
  theme_minimal()


# Total economic loss by year by region
# Join pacific_dis_economic with pacific_lookup to get Region
economic_loss_by_year_region <- pacific_dis_economic %>%
  left_join(pacific_lookup, by = "Pacific_Island") %>%
  group_by(Region, Year) %>%
  summarise(Total_Economic_Loss = sum(Economic_Loss, na.rm = TRUE), .groups = "drop")

# Plot time series for each region
ggplot(economic_loss_by_year_region, aes(x = Year, y = Total_Economic_Loss, color = Region)) +
  geom_line(linewidth = 1) +
  geom_point() +
  facet_wrap(~ Region, scales = "free_y", ncol = 1) +
  labs(
    title = "Total Economic Loss by Year and Region",
    x = "Year",
    y = "Total Economic Loss",
    color = "Region"
  ) +
  theme_minimal()


# Persons vs Economic Loss 2020 - 2025
# Join all dataframes to get Persons, Economic_Loss, and Region
combined_data <- pacific_dis_affected %>%
  left_join(pacific_dis_economic, by = c("Pacific_Island", "Year")) %>%
  left_join(pacific_lookup, by = "Pacific_Island") %>%
  filter(Year > 2019) %>%
  select(Pacific_Island, Year, Persons, Economic_Loss, Region)

# Plot scatter plot for each Pacific_Island
ggplot(combined_data, aes(x = Persons, y = Economic_Loss, color = Pacific_Island)) +
  geom_point(alpha = 0.7) +
  labs(
    title = "Economic Loss vs. Total Persons (Years > 2019)",
    x = "Total Persons",
    y = "Economic Loss",
    color = "Pacific Island"
  ) +
  theme_minimal() +
  theme(legend.position = "right")
