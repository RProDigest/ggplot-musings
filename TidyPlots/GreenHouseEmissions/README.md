
# Greenhouse Gas Emissions Analysis
## Overview
his script analyzes **Greenhouse Gas (GHG) emissions** using the tidyplot package. It retrieves data from **Our World in Data**, filters for relevant income groups, and visualizes CO₂ emissions over time.

## Authors

- [@RProDigest](https://github.com/RProDigest/) 
- **Date :** 1st March 2025


## Purpose

The purpose of this script is to:

- Load and process greenhouse gas emissions data from the Our World in Data repository.

- Filter the data based on income categories (low, lower-middle, upper-middle, and high income).

- Generate a stacked bar plot to illustrate CO₂ emissions trends across different income levels over the years.
## Dependencies

- The script requires the following R packages:


``` r

if (!require(pacman)) {
  install.packages("pacman")  # Check if package manager is installed
}

pacman::p_load(tidyplots, tidyverse)

```
## Data Source

The data set is ingested from github.


```r
url <- "https://raw.githubusercontent.com/owid/energy-data/master/owid-energy-data.csv"
energy_data <- read_csv(url)
```

## Data Processing

The script filters for countries classified by income levels and exports the dataset:

```r

energy_per_capita <- energy_data |>
  filter(grepl("income", country, ignore.case = TRUE))


```

## Data Visualisation

The script uses tidyplot to create a stacked bar chart representing GHG emissions:

- Color Scheme:

```r
my_colors <- c("Upper-middle-income countries" = "#4FAE62",
               "Lower-middle-income countries" = "#CCCCCC",
               "Low-income countries" = "darkred",
               "High-income countries" = "#888888")


```

- Plot Construction

```r
energy_per_capita |> 
  tidyplot(x = year, y = greenhouse_gas_emissions, color = country) |> 
  add_barstack_absolute() |>
  add_title("Green House Carbon Gas Emissions (megatonnes of CO₂ equivalents per year )") |> 
  add_caption("Data source: Our World in Data| Plotted by @RProDigest on X") |> 
  theme_minimal_y() |> 
  adjust_size(230, 100) |> 
  adjust_legend_position("top") |> 
  adjust_title(fontsize = 14) |> 
  adjust_colors(my_colors) |>
  remove_legend_title() |> 
  remove_y_axis_title() |>
  adjust_theme_details(legend.key.height = unit(2, "mm")) |> 
  adjust_theme_details(legend.key.width = unit(2, "mm")) 


```
## Output

The visualization presents GHG emissions (megatonnes of CO₂ equivalents per year), categorized by income level, showing emission trends from 2000 onwards.
## License

This script is free to use and modify. Credit to Our World in Data for the dataset.
## Contact

For inquiries or collaborations, reach out via @RProDigest on X.