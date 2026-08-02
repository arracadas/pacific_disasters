##########################################
## Pacific Data Viz Challenge 2026      ##
## Disasters in the Pacific Region      ##
## June 2026 - Data Processing          ##
##########################################

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
population_un_stats <- read.csv('data/undata_country_population_stats.csv',
                             stringsAsFactors = FALSE,
                             na.strings = c("", " ", "NA", "#VALUE!"),
                             skip = 1)


##############################
## 2. DATA PREPARATION      ##
## Clean up datasets        ##
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


###################################
## 3. DATA PREPARATION           ##
## Add Region Mapping            ##
###################################
# Create region mapping
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

# Add region to affected persons dataset
pacific_dis_affected <- affected_clean %>%
  rename(Affected_Persons = Value) %>%
  filter(Year >= 2005, Affected_Persons > 9) %>%
  left_join(region_mapping, by = "Pacific_Island") %>%
  # Select and arrange final columns
  select(Year, Region, Pacific_Island, Country, Affected_Persons) %>%
  arrange(Year, Region, Pacific_Island)


# Add region to pacific disaster economic loss dataset
pacific_dis_economic <- economic_clean %>%
  rename(Economic_Loss = Value) %>%
  filter(Year >= 2005, Economic_Loss > 100000) %>%
  # Join with region mappings
  left_join(region_mapping, by = "Pacific_Island") %>%
  # Select and arrange final columns
  select(Year, Region, Pacific_Island, Country, Economic_Loss) %>%
  arrange(Year, Region, Pacific_Island)


# Add region to pacific sea temp anomalies dataset
pacific_sea_temp_anomalies <- sea_temp_anomalies_clean %>%
  rename(Temp = Value) %>%
  filter(Year >= 2005, Temp != 0) %>%
  # Join with region mappings
  left_join(region_mapping, by = "Pacific_Island") %>%
  # Select and arrange final columns
  select(Year, Region, Pacific_Island, Country, Temp) %>%
  arrange(Year, Region, Pacific_Island)


#######################################
## 4. DATA PREPARATION               ##
## Fix Vanuatu's missing statistics  ##
## Cyclone Pam in 2015               ##
#######################################
vrow <- data.frame(
  Year = 2015,
  Region = "Melanesia",
  Pacific_Island = "VU",
  Country = "Vanuatu",
  Affected_Persons = 188000
)

pacific_dis_affected <- rbind(pacific_dis_affected, vrow)

vvrow <- data.frame(
  Year = 2015,
  Region = "Melanesia",
  Pacific_Island = "VU",
  Country = "Vanuatu",
  Economic_Loss = 450000000
)

pacific_dis_economic <- rbind(pacific_dis_economic, vvrow)


###################################
## 5. DATA PREPARATION           ##
## UNData Population Stats       ##
###################################
# Fix Micronesia in Population UN Stats
population_un_stats <- population_un_stats %>%
  mutate(
    X = ifelse(X == "Micronesia (Fed. States of)",
               "Micronesia (Federated States of)",
               X)
  )

# Fix Wallins and Futuna in Population UN Stats
population_un_stats <- population_un_stats %>%
  mutate(
    X = ifelse(X == "Wallis and Futuna Islands",
               "Wallis and Futuna",
               X)
  )

# Filter population_un_stats on Pacific Island countries
unique_countries <- distinct(affected_clean, Country) %>% pull(Country)
regions <- c("Melanesia", "Polynesia", "Micronesia")
unique_countries <- c(unique_countries, regions)

# Clean population data for 2025
population_un_clean <- population_un_stats %>%
  filter(
    Series == "Population mid-year estimates (millions)",
    X %in% unique_countries,
    !is.na(X)
  ) %>%
  select(
    Year,
    Country = X,
    Pop_millions = Value
  ) %>%
  mutate(
    Pop_millions = as.numeric(gsub(",", "", Pop_millions))
  )


# Add affected persons as % of total population
pacific_dis_affected <- pacific_dis_affected %>%
  # Assign each year to the nearest population year
  mutate(
    Pop_Year = case_when(
      Year <= 2010 ~ 2010,
      Year <= 2015 ~ 2015,
      Year <= 2023 ~ 2023,
      Year <= 2025 ~ 2025,
      TRUE ~ NA_real_
    )
  ) %>%
  # Join with population data, filtering out zero population values
  left_join(
    population_un_clean %>%
      filter(Pop_millions > 0),
    by = c("Country", "Pop_Year" = "Year")
  ) %>%
  # Calculate affected persons as % of population
  mutate(
    Affected_Pop_Perc = round(Affected_Persons / (Pop_millions * 1000000), 3)
    ) %>%
  mutate(
    Affected_Pop_Perc = ifelse(Affected_Pop_Perc > 1, 1, Affected_Pop_Perc)
    ) %>%
  # Clean up: remove temporary column
  select(-Pop_Year)



######################################
## 6. EXPORT DATAFRAMES             ##
######################################
saveRDS(population_un_clean, "./data/pcf_pop_clean.rds")
saveRDS(pacific_dis_affected, "./data/pcf_dis_affected_clean.rds")
saveRDS(pacific_dis_economic, "./data/pcf_dis_economic_clean.rds")
saveRDS(pacific_sea_temp_anomalies, "./data/pcf_sea_temp_anomalies_clean.rds")

