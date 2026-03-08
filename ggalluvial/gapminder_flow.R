
################################################################################
# Name: RProDigest                                                             #
# Date : 8th March  2026                                                       #
################################################################################

# 0.0 DESCRIPTION OF THE SCRIPT----------

# This script showcases the power of the parcats package for creating interactive 
# parallel category plots, using the Gapminder dataset as a case study. 
# We will visualize how life expectancy and income levels have evolved across 
# continents from 1952 to 2007,with a specific focus on Asia ,employing the 
# Okabe-Ito color palette for accessibility. 
# The script includes data preparation, plot construction, and saving the 
# interactive visualization for web use. 

# 1.0 INSTALL AND LOAD THE NECESSARY PACKAGES-----------------------------------

if (!require(pacman)) {
  install.packages("pacman")
}

pacman::p_load(tidyverse,
               gapminder,
               easyalluvial,
               parcats,
               htmlwidgets,
               webshot2)

# 2.0 DATA PREPARATION OPTIMIZED FOR DATA STORYTELLING--------------------------

gap_wide <- gapminder %>%
  filter(year %in% c(1952, 2007)) %>%
  mutate(
    year = factor(year),
    life_band = case_when(
      lifeExp < 45 ~ "<45",
      lifeExp < 60 ~ "45-59",
      lifeExp < 75 ~ "60-74",
      TRUE ~ "75+"
    ) %>% factor(levels = c("<45", "45-59", "60-74", "75+")),
    gdp_band = case_when(
      gdpPercap < 1000 ~ "<1k",
      gdpPercap < 5000 ~ "1k-5k",
      gdpPercap < 15000 ~ "5k-15k",
      TRUE ~ "15k+"
    ) %>% factor(levels = c("<1k", "1k-5k", "5k-15k", "15k+"))
  ) %>%
  select(country, continent, year, life_band, gdp_band) %>%
  pivot_wider(names_from = year, values_from = c(life_band, gdp_band), names_sep = "_") %>%
  drop_na()

# 3.0 PREPARATION FOR THE PLOT--------------------------------------------------

plot_df <- gap_wide %>%
  transmute(
    Continent = as.factor(continent), 
    `Life Expectancy (1952)` = life_band_1952,
    `Income Level (1952)`  = gdp_band_1952,
    `Life Expectancy (2007)` = life_band_2007,
    `Income Level (2007)`  = gdp_band_2007
  )

# 4.0 DEFINITION OF OKABIE-ITO PALETTE (COLOR BLIND FRIENDLY)-------------------

okabe_ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")



# 5.0 CONSTRUCT ALLUVIAL WITH OKABIE-ITO COLORS---------------------------------

p_alluvial <- alluvial_wide(
  data = plot_df,
  fill_by = "first_variable",
  col_vector_flow = okabe_ito,
  col_vector_value = okabe_ito
)

# 6.0 CREATING INTERACTIVE Parcats VISUAL---------------------------------------

p_parcats <- parcats(
  p = p_alluvial,
  marginal_histograms = TRUE,
  data_input = plot_df,
  hoveron = "color",
  hoverinfo = "count+probability",
  arrangement = "perpendicular",
  bundlecolors = TRUE,
  sortpaths = "forward",
  labelfont = list(size = 18, color = "black"), # Larger for X/mobile
  width = 1400, 
  height = 700
)


p_parcats # View in RStudio

# 7.0 SAVE AS HTML USING  WEBSHOT (OPTIONAL)------------------------------------

saveWidget(p_parcats, "gapminder_viz.html", selfcontained = TRUE)
