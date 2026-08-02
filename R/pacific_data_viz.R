##########################################
## Pacific Data Viz Challenge 2026      ##
## Disasters in the Pacific Region      ##
## June 2026 - Data Viz                 ##
## by jortega                           ##
##########################################

#######################
## Libraries         ##
#######################
library(tidyverse)
library(skimr)
library(ggdist)
library(beeswarm)
library(grid)
library(png)
library(ggtext)
library(showtext)
library(ragg)
library(ggrepel)
library(patchwork)
library(stringr)
library(ggthemes)
library(gghighlight)
library(gt)

## Loading Google fonts (https://fonts.google.com/)
font_add_google("Love Ya Like A Sister", "sister")  # text and title
font_add_google("Montserrat", "montserrat")
font_add_google("Roboto", "roboto")
font_add_google("Gloria Hallelujah", "gloria")
font_add_google("Indie Flower", "indie")
font_add_google("Jost", "jost")
font_add_google("Ubuntu", "ubuntu")

## Automatically use showtext to render text
showtext_auto()

# color canva palettes in ggthemes:
# https://github.com/EmilHvitfeldt/r-color-palettes/blob/main/canva.md

# Load datasets
pcf_population <- readRDS("./data/pcf_pop_clean.rds")
pcf_dis_affected <- readRDS("./data/pcf_dis_affected_clean.rds")
pcf_dis_economic <- readRDS("./data/pcf_dis_economic_clean.rds")
pcf_sea_anomalies <- readRDS("./data/pcf_sea_temp_anomalies_clean.rds")



###############################
## Exploratory Data Analysis ##
###############################
# Check economic loss in 2016
pcf_dis_affected %>%
  filter(Country == "Fiji") 


pcf_dis_economic %>%
  filter(Country == "Fiji")


######################################
## Population in 2023 by Country    ##
######################################
pcf_population %>%
  filter(Year == 2023 & !Country %in% c("Melanesia", "Micronesia", "Polynesia")) %>%
  select(Year, Country, Pop_millions) %>%
  arrange(desc(Pop_millions)) %>%
  mutate(
    Cumulative_Pop = cumsum(Pop_millions),
    Total_Pop = sum(Pop_millions),
    Cumulative_Percent = round((Cumulative_Pop / Total_Pop) * 100, 2)
  ) %>%
  select(Year, Country, Pop_millions, Cumulative_Pop, Cumulative_Percent)




##########################
## Set a common theme   ##
##########################
common_theme <- theme_minimal(
  base_family = "jost"
  ) +
  theme(
    plot.title = element_text(
      face = "bold", size = rel(1.4), color = "#FF6B6B"
    ),
    plot.subtitle = element_text(
      family = "roboto", size = rel(1.1), color = "gray40", 
      lineheight = 1.1, margin = margin(t = 6, b = 20)
    ),
    plot.caption = element_text(
      color = "grey40"
    ),
    axis.title.x = element_text(
      hjust = 0, color = "gray40",
    ),
    axis.title.y = element_text(
      hjust = -0.5, vjust = 3, color = "gray40",
    ),
    axis.text.y = element_text(
      color = "gray40", size = 12,
    ),
    axis.text.x = element_text(
      color = "gray40", size = 12,
    ),
    legend.title = element_text(
      color = "gray40", size = rel(1.1),
      hjust = 0.5  # Center the title
    ),
    legend.text = element_text(
      color = "gray40",
      hjust = 0.5,
      margin = margin(t = 4)  # Add space above the text
    ),
    plot.tag = element_text(
      size = rel(1)
    ),
    plot.margin = margin(0.8, 0.8, 0.8, 0.8, unit = "cm")  # 1 cm for top, right, bottom, left margins
    #panel.grid = element_blank(),
  )


########################
## Affected Persons   ##
########################
ggplot(pcf_dis_affected,
       aes(x = Year, y = Country)) +
  geom_vline(
    xintercept = 2015,
    linewidth = 0.6,
    linetype = "longdash",
    color = "palevioletred1"
    #color = "#FF6B6B"
  ) +
  geom_point(data = filter(pcf_dis_affected, Affected_Pop_Perc < 0.24),
             aes(size = Affected_Persons),
             color = "seashell2",
  ) +
  geom_point(data = filter(pcf_dis_affected, Affected_Pop_Perc >= 0.24),
             aes(size = Affected_Persons),
             color = "#4ECDC4",
             alpha = 0.6
  ) +
  geom_text(
    data = filter(pcf_dis_affected, Affected_Pop_Perc > 0.24),
    aes(label = paste0(round(Affected_Persons / 1000, 0), "k")),
    color = "#0077BE",
    family = "ubuntu",
    fontface = "bold",
    size = 3.5
  ) +
  scale_size(range = c(1, 15),
             labels = function(x) paste0(round(x / 1000, 1), "k")) +
  facet_wrap(~Region, ncol = 1, scales = "free_y") +
  labs(
    title = "Pacific Disasters: A New Era",
    subtitle = "Since 2015, most Pacific countries have experienced at least one disaster\naffecting 25% or more of their population (highlighted).",
    caption = "Source: Pacific Data Hub",
    x = "",
    y = "",
    tag = "Fig.1",
    size = "Affected Persons"
  ) +
  common_theme +
  theme(
    legend.position = "bottom",
    strip.text = element_text(
      family = "jost",
      size = rel (1.2),
      color = "gray50",
    ),
    strip.background = element_rect(
      fill = "snow2",
      color = "white",
      linewidth = 0.5
    ),
  )
  

#########################
## Total Economic Loss ##
#########################
## Subset on Fiji and Vanuatu
pcf_dis_economic_fj <- pcf_dis_economic %>%
  filter(Country == "Fiji")

pcf_dis_economic_vu <- pcf_dis_economic %>%
  filter(Country == "Vanuatu")


## New FJ records
new_records_fj <- data.frame(
  Year = c(2013, 2014, 2015, 2017),
  Region = "Melanesia",
  Pacific_Island = "FJ",
  Country = "Fiji",
  Economic_Loss = 0,
  Pop_millions = 0.92
)

## New VU records
new_records_vu <- data.frame(
  Year = c(2013, 2016, 2017, 2019, 2020),
  Region = "Melanesia",
  Pacific_Island = "VU",
  Country = "Vanuatu",
  Economic_Loss = 0,
  Pop_millions = 0.27
)

## Append new rows
pcf_dis_economic_fj <- bind_rows(pcf_dis_economic_fj, new_records_fj)
pcf_dis_economic_vu <- bind_rows(pcf_dis_economic_vu, new_records_vu)


## Plot Economic Loss for Fiji
ec_p1 <- ggplot(pcf_dis_economic_vu,
       aes(x = Year, y = Economic_Loss)) +
  geom_line(
    linewidth = 1.2,
    color = "#0077BE",
    lineend = "round",
    linejoin = "round"
  ) +
  geom_area(
    fill = "#4ECDC4",
    alpha = 0.5,
    lineend = "round",
    linejoin = "round"
  ) +
  geom_point(
    size = 2,
    color = "#0077BE",
    stroke = 1
  ) +
  annotate(
    geom = "text",
    x = 2015,
    y = 460000000,
    label = "Cyclone Pam:\nEconomic losses reached\n  ~$450m (64% of GDP)",
    hjust = -0.15, vjust = 1,
    fontface = "bold",
    family = "roboto",
    color = "#0077BE",
  ) +
  scale_x_continuous(breaks = unique(pcf_dis_economic_fj$Year), labels = as.integer) +
  scale_y_continuous(
    labels = function(x) paste0("$", round(x / 1000000, 1), "m"),
    expand = expansion(mult = c(0, 0.25))
  ) +
  facet_wrap(~Country, ncol = 1, scales = "free_y") +
  labs(
    title = "Unprecendented Economic Damage",
    subtitle = "The damage to food stocks and critical infrastructure (schools, clinics, homes, power stations)\nhas reached new heights. In March 2015, Category 5 Cyclone Pam left Vanuatu's population\nvulnerable to food shortages. Just a year later, in February 2016, Cyclone Winston - one of the\nfiercest storms ever recorded, left a similar trail of destruction across Fiji, striking homes\nand vital tourism infrastructure.",
    caption = "Source: Pacific Data Hub",
    x = "",
    y = "Economic Loss USD",
    tag = "Fig. 2"
  ) +
  common_theme +
  theme(
    strip.text = element_text(
      family = "jost",
      size = rel (1.2),
      color = "gray50",
    ),
    strip.background = element_rect(
      fill = "snow2",
      color = "white",
      linewidth = 0.5
    ),
    axis.text.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(0.8, 0.8, 0, 0.8, unit = "cm") 
  )


## Plot Economic Loss for Vanuatu
ec_p2 <- ggplot(pcf_dis_economic_fj,
                aes(x = Year, y = Economic_Loss)) +
  geom_line(
    linewidth = 1.2,
    color = "#0077BE",
    lineend = "round",
    linejoin = "round"
  ) +
  geom_area(
    fill = "#4ECDC4",
    alpha = 0.5,
    lineend = "round",
    linejoin = "round"
  ) +
  geom_point(
    size = 2,
    color = "#0077BE",
    stroke = 1
  ) +
  annotate(
    geom = "text",
    x = 2016,
    y = 400000000,
    label = "Cyclone Winston:\nEconomic losses reached\n  ~$400m(10% of GDP)",
    hjust = -0.15, vjust = 1,
    fontface = "bold",
    family = "roboto",
    color = "#0077BE",
  ) +
  scale_x_continuous(breaks = unique(pcf_dis_economic_vu$Year), labels = as.integer) +
  scale_y_continuous(
    labels = function(x) paste0("$", round(x / 1000000, 1), "m"),
    expand = expansion(mult = c(0, 0.25))
  ) +
  facet_wrap(~Country, ncol = 1, scales = "free_y") +
  labs(
    x = "",
    y = ""
  ) +
  theme_minimal() +
  common_theme +
  theme(
    strip.text = element_text(
      family = "jost",
      size = rel (1.2),
      color = "gray50",
    ),
    strip.background = element_rect(
      fill = "snow2",
      color = "white",
      linewidth = 0.5
    ),
    panel.grid.minor = element_blank()
  )

# Stack plots vertically
ec_stacked_plot <- (ec_p1 / ec_p2) +
  plot_layout(heights = c(4, 4))

# Display the stacked plot
print(ec_stacked_plot)




#########################################
## Sea level Anomalies by Year         ##
#########################################
# Calculate max and min Temp
max_temp <- pcf_sea_anomalies %>% pull(Temp) %>% max(na.rm = TRUE)
min_temp <- pcf_sea_anomalies %>% pull(Temp) %>% min(na.rm = TRUE)

# Filter rows where Temp matches max or min
pcf_sea_anomalies %>%
  filter(Temp > 1)
    

# Plot with jittered points and trend lines for each group
ps1 <- ggplot(pcf_sea_anomalies, aes(x = Year, y = Temp)) +
  geom_jitter(aes(color = ifelse(Temp > 1, "#4ECDC4", "#FF6B6B")),
              width = 0.3, height = 0.1, alpha = 0.7, show.legend = FALSE) +
  geom_smooth(method = "lm", formula = "y ~ x", se = TRUE, linewidth = 0.8, color = "gray20") +
  geom_hline(yintercept = 0, linetype = "solid", color = "#0077BE", linewidth = 1, alpha = 0.6) +
  annotate(
    "text",
    x = 2022,
    y = 0,
    label = "1971–2000 climatological baseline",
    vjust = 1.5,
    hjust = 1.1,
    family = "jost", color = "#0077BE", size = 4, fontface = "bold" ) +
  annotate(
    geom = "text",
    x = 2015, y = 1.1, label = "Kiribati",
    hjust = -0.25, vjust = 1,
    fontface = "bold",
    family = "roboto", color = "#FF6B6B"
  ) +
  annotate(
    geom = "text",
    x = 2015, y = -1.1, 
    label = "Each dot represents the average annual temperature anomaly in a Pacific Island Country.",
    hjust = 0.5,
    family = "roboto", color = "gray40", size = 3.5, fontface = "italic"
  ) +
  scale_y_continuous(limits = c(-1.25, 1.25)) +
  labs(
    title = "Hot Water",
    subtitle = "The heat, like the damage, keeps climbing. Sea temperature anomalies in the South Pacific\nhave followed an upward trend for two decades. The first anomaly > 1 °C was recorded\nin 2015 near the island of Kiribati.",
    caption = "Source: Pacific Data Hub",
    x = "",
    y = "Sea temperature anomaly (°C)",
    tag = "Fig. 3"
  ) +
  common_theme +
  theme(
    axis.title.y = element_text(hjust = 0.5),
    plot.margin = margin(0.8, 0.8, 0, 0.8, unit = "cm")
  )


# Filter the dataset for Temp > 1 and select relevant columns
countries_high_temp <- pcf_sea_anomalies %>%
  filter(Temp > 1) %>%
  select(Year, Country, Temp)

# Format the table using gt() with "Jost" font
high_temp_table <- gt(countries_high_temp) %>%
  cols_align(align = "center") %>%
  cols_width(
    Year ~ px(80),
    Country ~ px(150),
    Temp ~ px(80)
  ) %>%
  tab_header(title = "Temp. Anomalies > 1°C") %>%
  tab_style(
    style = cell_text(
      font = "jost",  # Apply "Jost" font to the table
      size = px(12),   # Adjust font size
      color = "gray58",
      weight = "bold"
    ),
    locations = cells_body()  # Apply to all body cells
  ) %>%
  tab_style(
    style = cell_text(
      font = "jost",
      size = px(14),
      color = "gray58",
      weight = "bold"
    ),
    locations = cells_column_labels()  # Apply to column headers
  ) %>%
  tab_style(
    style = cell_text(
      font = "jost",
      size = px(16),
      color = "#FF6B6B",
      weight = "bold"
    ),
    locations = cells_title()  # Apply to the table title
  )


# Print the combined plot
ps1 / wrap_table(high_temp_table, space = "free_x")


