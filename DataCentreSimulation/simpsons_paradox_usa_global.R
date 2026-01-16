
################################################################################
# TITLE: "Does a Degree Still Pay in the USA?
# AUTHOR: @RProDigest
# DATE: 2026-01-16
# DATA SOURCE: ACS 2023, link https://www.census.gov/programs-surveys/acs
################################################################################

# 1. PACKAGES ---------------------------------------------------------------

if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, ggtext, patchwork)

# 2. Simulate Data: Two different Simpson's Paradoxes

set.seed(2026)

# Global View (Climate Paradox), Temperate: Golf is common, Heat is moderate----

global_temperate <- tibble(
  Context = "Global Temperate (e.g. US/EU)",
  Type = c(rep("Golf Course", 150), rep("Data Center", 80)),
  Usage = c(rnorm(150, 55, 10), rnorm(80, 25, 5))
)

# Arid: Golf is rare, Heat is extreme (Cooling demands spike)

global_arid <- tibble(
  Context = "Global Arid (e.g. Global South)",
  Type = c(rep("Golf Course", 10), rep("Data Center", 80)),
  Usage = c(rnorm(10, 20, 5), rnorm(80, 85, 10))
)

data_global <- bind_rows(global_temperate, global_arid) %>%
  group_by(Context, Type) %>% summarise(Total = sum(Usage), .groups = 'drop')

# USA VIEW (Infrastructure Paradox) National: 15k+ Golf courses dwarf the Data Center industry----

usa_national <- tibble(
  Context = "USA National Average",
  Type = c(rep("Golf Course", 1000), rep("Data Center", 50)),
  Usage = c(rnorm(1000, 50, 10), rnorm(50, 30, 5))
)

# Local Hub: A rural town with 1 golf course but a massive Hyperscale Campus -------------

usa_hub <- tibble(
  Context = "Local Hyperscale Hub",
  Type = c(rep("Golf Course", 2), rep("Data Center", 25)),
  Usage = c(rnorm(2, 40, 5), rnorm(25, 200, 20)) # High load per center
)


data_usa <- bind_rows(usa_national, usa_hub) %>%
  group_by(Context, Type) %>% summarise(Total = sum(Usage), .groups = 'drop')


# 3. Visualization

# Define Okabe-Ito colors -----

oi_red <- "#D55E00"
oi_blue <- "#0072B2"


# Plot 1: Global --------------

p1 <- data_global |> 
  ggplot(aes(Type, Total, fill = Type)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  facet_wrap(~factor(Context, levels = c("Global Temperate (e.g. US/EU)", "Global Arid (e.g. Global South)")), scales = "free_y") +
  scale_fill_manual(values = c("Data Center" = oi_red, "Golf Course" = oi_blue)) +
  theme_minimal(base_size = 11) +
  labs(title = "A) The Climate Paradox", 
       subtitle = "Globally, Golf wins. But in <span style='color:#D55E00;'><b>Arid Nations</b></span>,<br>Data Centers can outweigh Golf.",
       y = "Simulated Water Load", x = NULL) +
  theme(plot.subtitle = element_markdown(), axis.text.y = element_blank())

# Plot 2: USA-----------------

p2 <- data_usa |> 
  ggplot(aes(Type, Total, fill = Type)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  facet_wrap(~factor(Context, levels = c("USA National Average", "Local Hyperscale Hub")), scales = "free_y") +
  scale_fill_manual(values = c("Data Center" = oi_red, "Golf Course" = oi_blue)) +
  theme_minimal(base_size = 11) +
  labs(title = "B) The Infrastructure Paradox", 
       subtitle = "Nationally, Golf wins. But in <span style='color:#D55E00;'><b>Tech Hubs</b></span>,<br>the local grid sees a different reality.",
       y = NULL, x = NULL) +
  theme(plot.subtitle = element_markdown(), axis.text.y = element_blank())

# Combine using patchwork

final_plot <- p1 + p2 + 
  plot_annotation(
    title = "Simpson's Paradox: Why 'Average' Water Usage Misleads",
    subtitle = "Aggregates (Left Panels) support the 'Golf' defense. Local Realities (Right Panels) expose the infrastructure risk.",
    caption = "Visualization: @rprodigest | Palette: Okabe-Ito",
    theme = theme(plot.title = element_text(face = "bold", size = 16))
  )

final_plot

# Save the plot

ggsave("simpsons_paradox_water_usage.png", final_plot, width = 10, height = 6, dpi = 600)
