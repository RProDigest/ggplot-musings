
################################################################################
# Name: RProDigest                                                             #
# Date : 1st March 2025                                                        #
# Purpose: The script uses the tidyplot package understand GH gas Emissions    # 
#                                                                 #
################################################################################

# 1.0 INSTALL THE NECESSARY PACKAGES----

if (!require(pacman)) {
  install.packages("pacman") # Check if package manager is installed 
}

# Manage the installation and loading of relevant packages
pacman::p_load(tidyplots, tidyverse,scales 
)


# 2.0 LOAD THE DATA FROM OUR WORLD IN DATA GITHUB REPOSITORY----

url <- "https://raw.githubusercontent.com/owid/energy-data/master/owid-energy-data.csv"
energy_data <- read_csv(url)

# 3.0 FILTER AND SELECT RELEVANT COLUMNS-------------

energy_per_capita <- energy_data |>
  filter(grepl("income", country, ignore.case = TRUE)) %>% 
  write_csv("continents.csv")


# 4.0 CREATE THE PLOT----------------------------

my_colors <- c("Upper-middle-income countries" = "#4FAE62",
               "Lower-middle-income countries" = "#CCCCCC",
               "Low-income countries" = "darkred",
               "High-income countries" = "#888888")


energy_per_capita |> 
  tidyplot(x = year, y = greenhouse_gas_emissions, color = country) |> 
  add_barstack_absolute() |>
  add_title("Green House Gas Emissions (megatonnes of CO₂ equivalents per year )") |> 
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

