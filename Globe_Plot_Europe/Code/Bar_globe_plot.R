
# =============================================================================
# Radial plot using ggplot, Eurostat data on Mean Age at First Marriage in Europe
# Plotted by @RProDigest
# 10th January 2026
# =============================================================================



# 0) LOAD LIBRARIES --------------------------------------------------------

if (!require(pacman)) {
  install.packages("pacman") 
}

pacman::p_load(eurostat,
               showtext,
               tidyverse,
               sf,
               rnaturalearth,
               rnaturalearthdata,
               patchwork,
               sysfonts
               )


# 1) SETUP FONTS & THEME --------------------------------------------------

font_add_google("Roboto", "roboto")
showtext_auto()

theme_pro_circular <- function() {
  theme_void() +
    theme(
      plot.margin = margin(20, 20, 20, 20),
      plot.background = element_rect(fill = "#F5F5F2", color = NA),
      panel.background = element_rect(fill = "#F5F5F2", color = NA),
      legend.position = "top",
      legend.title = element_text(family = "roboto", face = "bold", size = 10, hjust = 0.5),
      legend.text = element_text(family = "roboto", size = 9),
      plot.title = element_text(family = "roboto", face = "bold", size = 20, hjust = 0.5, margin = margin(b = 5)),
      plot.subtitle = element_text(family = "roboto", size = 12, hjust = 0.5, color = "grey30", margin = margin(b = 15)),
      plot.caption = element_text(family = "roboto", size = 12, hjust = 1, color = "grey5", margin = margin(t = 20))
    )
}

# 2) DATA PREPARATION ----------------------------------------------------------

mar_eur <- get_eurostat("tps00014") %>%
  filter(stringr::str_length(geo) == 2) %>%
  filter(indic_de == "FAGEMAR1") %>%
  group_by(geo) %>%
  slice_max(order_by = TIME_PERIOD, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(
    iso_a2 = geo,
    Women  = values,
    year   = as.integer(format(TIME_PERIOD, "%Y"))
  ) %>%
  filter(!is.na(Women))

# 3) MAP PREPARATION ---------------------------------------------------

world_sf <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  sf::st_make_valid()

map_sf <- world_sf %>%
  left_join(mar_eur, by = "iso_a2") %>%
  filter(continent == "Europe") %>%
  filter(!is.na(Women)) %>%
  st_crop(xmin = -25, xmax = 45, ymin = 30, ymax = 72)

# Prepare Bar Data
bar_eur <- map_sf %>%
  st_drop_geometry() %>%
  transmute(Country = name_long, Women, year) %>%
  arrange(desc(Women))

# Circular Math
bar_eur$id <- seq_len(nrow(bar_eur))
n_bar <- nrow(bar_eur)
angle <- 90 - 360 * (bar_eur$id - 0.5) / n_bar
bar_eur$hjust <- ifelse(angle < -90, 1, 0)
bar_eur$angle <- ifelse(angle < -90, angle + 180, angle)
latest_year <- max(bar_eur$year, na.rm = TRUE)

# 4) COLOR PALETTE --------------------------------------------------------

okabe_gradient <- c("#0072B2", "#009E73", "#F0E442")

# 5) MAP PLOT---------------------------------------------------------------

crs_ortho_eur <- "+proj=ortho +lat_0=50 +lon_0=15"

map_plot_eur <- ggplot(map_sf) +
  geom_sf(aes(fill = Women), size = 0.1, color = "white") +
  scale_fill_gradientn(colors = okabe_gradient, guide = "none") +
  coord_sf(crs = crs_ortho_eur) + 
  theme_void() +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA))

# 6) CIRCULAR BAR PLOT ----------------------------------------------------

bar_plot_eur <- ggplot(bar_eur, aes(x = as.factor(id), y = Women, fill = Women)) +
  geom_col(width = 0.9, color = NA) +
  geom_text(
    aes(x = as.factor(id), y = Women + 1.5, label = Country, hjust = hjust),
    angle = bar_eur$angle,
    family = "roboto",
    fontface = "bold",
    size = 4.5,
    color = "grey20",
    inherit.aes = FALSE
  ) +
  scale_fill_gradientn(
    colors = okabe_gradient,
    breaks = seq(
      floor(min(bar_eur$Women, na.rm = TRUE)),
      ceiling(max(bar_eur$Women, na.rm = TRUE)),
      by = 2
    ),
    labels = scales::label_number(accuracy = 1),
    name = "Mean Age"
  ) +
  
  
  ylim(-150, max(bar_eur$Women, na.rm = TRUE) + 10) +
  
  labs(
    title = "Marriage Age in Europe",
    subtitle = paste0("Mean age at first marriage for women (Latest data, max ", latest_year, ")"),
    caption = "Source: Eurostat (tps00014) | Chart by @RProDigest, inspired by Tanya Shapiro's Work"
  ) +
  coord_polar() +
  theme_pro_circular()

# 7) COMBINE (WITH MARGIN) ------------------------------------------------

europe <- bar_plot_eur +
  inset_element(
    map_plot_eur,
    
   
    left = 0.20,   
    bottom = 0.20, 
    right = 0.80,  
    top = 0.80,    
    
    align_to = "full"
  )

europe

# 8) EXPORT FINAL PLOT ----------------------------------------------------

ggsave("europe_marriage.png", europe, height = 10, width = 8, dpi = 300, bg = "#F5F5F2")
ggsave("europe_marriage.jpg", europe, height = 10, width = 8, dpi = 300, bg = "#F5F5F2")
