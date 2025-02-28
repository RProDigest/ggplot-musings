################################################################################
# Name: RProDigest                                                             #
# Date : 28th Feb 2025                                                         #
# Purpose: The script uses the tidyplot package to mine insights into life     # 
# expectancy                                                                   #
################################################################################

# 1.0 INSTALL THE NECESSARY PACKAGES----

if (!require(pacman)) {
  install.packages("pacman") # Check if package manager is installed 
}

# Manage the installation and loading of relevant packages
pacman::p_load(tidyplots, tidyverse, gapminder 
               )



# 2.0 PLOT THE HEAT MAP------------

gapminder |> 
  group_by(continent, year) |>
  summarise(mean_lifeExp = mean(lifeExp)) |>
  tidyplot(x = continent, y = year, color = mean_lifeExp) |> 
  add_heatmap() |>
  adjust_size(width = 100, height = 100) |>
  adjust_title("Gap Minder Dataset: Life Expectancy by Continent") |> 
  adjust_colors(new_colors = colors_continuous_inferno) |> 
  adjust_legend_title("Mean Life Expectancy") |>
  theme_minimal_xy(fontsize = 12) |>
  add_caption("Plotted by RProDigest, 28th Feb 2025")
  
