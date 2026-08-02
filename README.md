# Pacific Disasters Data Visualization Project

A comprehensive data analysis and visualization project examining climate risks and disasters in the South Pacific region. This project uses R and Quarto to create an interactive report highlighting the increasing frequency and impact of natural disasters on Pacific Island nations.

## Project Overview

This project analyzes disaster data from the Pacific Data Hub to understand how climate change is affecting the vulnerability of Pacific Island countries to natural disasters. The analysis focuses on:

- **Affected Persons**: Number of people directly affected by disasters
- **Economic Loss**: Direct economic impact of disasters
- **Sea Surface Temperature Anomalies**: Climate indicators related to ocean warming
- **Population Data**: Demographic context for understanding disaster impact

## Key Findings

- **New Era of Disasters**: Since 2015, disasters affecting 25% or more of the population have become more frequent
- **Economic Impact**: Major cyclones like Pam (2015) and Winston (2016) caused unprecedented economic damage
- **Climate Connection**: Sea surface temperature anomalies > 1°C first recorded in 2015 near Kiribati

## Project Structure

```
pacific-disasters/
├── R/
│   ├── pacific_clean.R      # Data cleaning and preprocessing
│   ├── pacific_eda.R        # Exploratory data analysis
│   └── pacific_data_viz.R   # Visualization scripts
├── data/
│   ├── pacific_disaster_affected_persons.csv
│   ├── pacific_disaster_economic_loss.csv
│   ├── pacific_sea_surface_temperature_anomalies.csv
│   ├── undata_country_population_stats.csv
│   ├── world_bank_population_stats.csv
│   └── *.rds                # Processed data files
├── assets/
│   ├── cyclone_pam_disaster.jpeg
│   └── nano_tropical_cyclones_pacific.jpg
├── pacific disasters.Rproj  # RStudio project file
├── .gitignore
└── README.md
```

## Data Sources

- **Pacific Data Hub**: Sustainable Development Goal indicators for Pacific Islands
- **United Nations**: Population statistics and estimates
- **World Bank**: Population and economic data
- **NOAA/Climate Data**: Sea surface temperature anomalies

## Technology Stack

- **R**: Data analysis and statistical computing
- **R Packages**: 
  - `tidyverse` (dplyr, tidyr, ggplot2)
  - `janitor` for data cleaning
  - `patchwork` for multi-plot layouts
  - `gt` for table formatting
  - `showtext` and `ragg` for custom fonts
- **Quarto**: For creating reproducible reports and documents

## Usage

1. **Clone the repository**:
   ```bash
   git clone <repository-url>
   cd pacific-disasters
   ```

2. **Install dependencies**:
   ```r
   install.packages(c("tidyverse", "janitor", "patchwork", "gt", "showtext", "ragg", "ggrepel", "ggthemes", "gghighlight", "ggdist", "beeswarm", "png", "ggtext", "stringr", "grid", "skimr"))
   ```

3. **Run data processing**:
   ```r
   source("R/pacific_clean.R")
   ```

4. **Generate visualizations**:
   ```r
   source("R/pacific_data_viz.R")
   ```

5. **Create Quarto report**:
   ```bash
   quarto render report.qmd
   ```

## Key Visualizations

The project produces several key visualizations:

1. **Pacific Disasters: A New Era** - Shows the increasing frequency of disasters affecting large population percentages
2. **Unprecedented Economic Damage** - Economic loss trends for Fiji and Vanuatu with major cyclone annotations
3. **Hot Water** - Sea temperature anomalies with trend lines and highlights

## Climate Change Connection

The analysis demonstrates how:
- Rising sea surface temperatures correlate with increased cyclone intensity
- Climate change amplifies the impact of natural disasters
- Pacific Islands face disproportionate vulnerability due to their geography and size

## Report Content

The Quarto report includes:
- Executive summary with key statistics
- Data methodology and sources
- Interactive visualizations
- Climate change context
- Policy recommendations

## Contributing

Contributions are welcome! Please:
1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## License

This project is open source and available for academic and research use. Please cite the original data sources when using this analysis.

## Contact

For questions or collaboration opportunities, please contact the project maintainers.

---

*Project created for Pacific Data Viz Challenge 2026*
*Data analysis by: jortega*