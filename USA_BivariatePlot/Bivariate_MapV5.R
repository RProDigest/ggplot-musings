################################################################################
# TITLE: "Does a Degree Still Pay in the USA?
# AUTHOR: @RProDigest
# DATE: 2026-01-16
# DATA SOURCE: ACS 2023, link https://www.census.gov/programs-surveys/acs
################################################################################

# 1. PACKAGES ---------------------------------------------------------------

if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidycensus, tidyverse, sf, biscale, cowplot, tigris, showtext)

# 2. SETUP FONTS ------------------------------------------------------------


font_add_google("Merriweather", "title_font")      # For the main Headline
font_add_google("Roboto Condensed", "body_font")   # For labels & subtitles
showtext_auto() 

# 3. GET & PROCESS DATA -----------------------------------------------------

# You will need a Census API key. If you don't have one, you can sign up 
# at the Census Bureau http://api.census.gov/data/key_signup.html
# Once you have your key, run the following line once to install it:
# census_api_key("YOUR_KEY_HERE", install = TRUE)
# Then restart R for the changes to take effect.


# then pull the data with the code below 

usa_raw <- get_acs(
  geography = "county",
  variables = c(income = "B19013_001", edu = "DP02_0068P"),
  year = 2023,
  geometry = TRUE,
  output = "wide"
) %>%
  rename(income = incomeE, education = eduE) %>% 
  drop_na(income, education)

# 4. Beacause of Alaska, hawaii we need to shift the Geometry ----------------

usa_shifted <- usa_raw %>% 
  tigris::shift_geometry(position = "below") 

# 5. Create Annotation Targets----------------------------------------------
# We have to calculate centroids to know exactly where to point the arrows

usa_coords <- usa_shifted %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() %>% 
  bind_cols(usa_shifted)

targets <- usa_coords %>% 
  filter(GEOID %in% c("20183", "02290", "15001")) %>% 
  mutate(label = case_when(
    GEOID == "20183" ~ "USA Mainland",
    GEOID == "02290" ~ "Alaska",
    GEOID == "15001" ~ "Hawaii"
  ))

ak_target <- targets %>% filter(label == "Alaska")
hi_target <- targets %>% filter(label == "Hawaii")
us_target <- targets %>% filter(label == "USA Mainland")

# 6. Lets Make Bivariate Classes-----------------------------------------------

usa_data <- bi_class(usa_shifted, x = income, y = education, style = "quantile", dim = 3)

oi_bi <- c(
  "1-1" = "#F7F7F7", "2-1" = "#FAD6A5", "3-1" = "#E69F00",
  "1-2" = "#CFE8F3", "2-2" = "#B9B3D6", "3-2" = "#8C6BB1",
  "1-3" = "#56B4E9", "2-3" = "#4C72B0", "3-3" = "#2B2D42"
)

# 7. We define a map with custom fonts-----------------------------------------
# Define sizes for consistency
anno_text_size <- 12    # Size for geom_text annotations
arrow_width <- 1        # Thickness of annotation lines

map <- ggplot() +
  # Map Layer
  geom_sf(data = usa_data, aes(fill = bi_class), color = NA, size = 0.1) +
  scale_fill_manual(values = oi_bi, guide = "none") +
  
  # --- ANNOTATIONS (Sizes increased) ---
  
  # 7.11. USA MAINLAND
  annotate("curve", 
           x = 1500000, y = 2800000, 
           xend = us_target$X, yend = us_target$Y, 
           curvature = -0.2, arrow = arrow(length = unit(0.02, "npc")), 
           color = "grey20", linewidth = arrow_width) +
  annotate("text", x = 1500000, y = 2950000, label = "USA Mainland", 
           family = "body_font", fontface = "bold", color = "grey20", size = anno_text_size) +
  
  # 7.2. ALASKA
  annotate("curve", 
           x = -2800000, y = 1000000, 
           xend = ak_target$X, yend = ak_target$Y, 
           curvature = 0.3, arrow = arrow(length = unit(0.02, "npc")), 
           color = "grey20", linewidth = arrow_width) +
  annotate("text", x = -2800000, y = 1150000, label = "Alaska", 
           family = "body_font", fontface = "bold", color = "grey20", size = anno_text_size) +
  
  # 7.3. HAWAII
  annotate("curve", 
           x = -500000, y = -1800000, 
           xend = hi_target$X, yend = hi_target$Y, 
           curvature = 0.3, arrow = arrow(length = unit(0.02, "npc")), 
           color = "grey20", linewidth = arrow_width) +
  annotate("text", x = -500000, y = -1950000, label = "Hawaii", 
           family = "body_font", fontface = "bold", color = "grey20", size = anno_text_size) +
  
  # Theme (Sizes significantly increased)
  theme_void() +
  labs(
    title = "Does a Degree Still Pay?",
    subtitle = "Mapping the link between Bachelor's Degrees and Median Income (2023)",
    caption = "Source: ACS 2023 | Graphic: @RProDigest"
  ) +
  theme(
    # Main Title: Much larger
    plot.title = element_text(family = "title_font", hjust = 0.5, face = "bold", size = 48, margin = margin(b = 15)),
    # Subtitle: Larger
    plot.subtitle = element_text(family = "body_font", hjust = 0.5, size = 24, margin = margin(b = 25)),
    # Caption: Larger
    plot.caption = element_text(family = "body_font", hjust = 0.95, color = "grey50", size = 16)
  )

# 8. LEGEND & FINAL COMPOSE -------------------------------------------------

legend_df <- expand.grid(x = 1:3, y = 1:3) %>% mutate(bi_class = paste0(x, "-", y))

legend <- legend_df |> 
  ggplot(aes(x, y, fill = bi_class)) +
  geom_tile() +
  scale_fill_manual(values = oi_bi, guide = "none") +
  coord_equal() +
  labs(x = "Higher Income ->", y = "Education ->") +
  theme_minimal() +
  theme(
    axis.text = element_blank(), 
    panel.grid = element_blank(),
    # Legend Axis Labels: Larger font
    axis.title = element_text(family = "body_font", size = 18, color = "grey40", face = "bold")
  )

final_plot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  # Adjusted legend position slightly
  draw_plot(legend, 0.76, 0.05, 0.22, 0.22) 

# Save high-res 
ggsave("USA_Degree_Income_Viral_FIXED.png", final_plot, width = 10, height = 8, dpi = 300, bg = "white")


final_plot
