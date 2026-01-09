########################################################################
# Title: R Banner Code for my X handle                                 #
# Created by RProDigest                                                #
# 9th January 2026                                                     #
########################################################################

# 0. Load Libraries -----------------------------------------

library(tidyverse)
library(showtext)


# 1. Load Font ------------------------------------------------

font_add_google("Inter", "inter")
showtext_auto()

# 2. Generate Background Data ----
set.seed(123)
n_points <- 100
bg_data <- tibble(
  x = seq(0, 1, length.out = n_points),
  y1 = 0.2 + 0.1 * sin(10 * x) + 0.05 * runif(n_points),
  y2 = 0.3 + 0.1 * sin(10 * x + 1) + 0.05 * runif(n_points),
  y3 = 0.4 + 0.15 * cos(8 * x) + 0.05 * runif(n_points)
)

# 3. Create the Banner using ggplot ------------------------

p <- ggplot() +
  
  # --- Background Layers ---
  geom_ribbon(data = bg_data, aes(x = x, ymin = 0, ymax = y1), 
              fill = "#1e293b", alpha = 0.4) + 
  geom_ribbon(data = bg_data, aes(x = x, ymin = 0, ymax = y2), 
              fill = "#334155", alpha = 0.3) + 
  geom_ribbon(data = bg_data, aes(x = x, ymin = 0, ymax = y3), 
              fill = "#475569", alpha = 0.2) + 
  
  
  annotate(
    "text",
    x = 0.07, 
    y = 0.72, 
    label = "Practical R Programming\n& Data Science",
    family = "inter",
    fontface = "bold",
    size = 14,    
    lineheight = 0.95,
    hjust = 0,
    colour = "#f8fafc"
  ) +
  
  # Accent Line
 
  annotate(
    "segment",
    x = 0.07, xend = 0.17,
    y = 0.58, yend = 0.58,
    linewidth = 1.2,
    colour = "#38bdf8"
  ) +
  
  annotate(
    "text",
    x = 0.07,
    y = 0.50,
    label = "Short, actionable insights on tidyverse, data viz, and workflows",
    family = "inter",
    size = 6,
    hjust = 0,
    colour = "#94a3b8" 
  ) +
  
  # Caption
  
  annotate(
    "text",
    x = 0.98,
    y = 0.08, 
    label = "Generated using Tidyverse & R packages",
    family = "inter",
    size = 5,
    hjust = 1,
    colour = "#f8fafc"
  ) +
  
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#0f172a", colour = NA) 
  )

# 4. Save-----------------------------------------------------

ggsave(
  "RProDigest_Banner_.png",
  p,
  width = 1500,
  height = 500,
  units = "px",
  dpi = 72
)


# End of Code ------------------------------------------------