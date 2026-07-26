#####################
#
# Scaling Laws Visualization in R
# By RProDigest, 26.7.2026
#####################

library(ggplot2)
library(ggtext)

# 1. Okabe-Ito Color Palette Setup
okabe_ito <- c(
  black       = "#000000",
  orange      = "#E69F00",
  sky_blue    = "#56B4E9",
  bluish_green= "#009E73",
  yellow      = "#F0E442",
  blue        = "#0072B2",
  vermillion  = "#D55E00",
  red_purple  = "#CC79A7"
)

# 2. Simulate Realistic Scaling Data
set.seed(42)
log_compute <- seq(18, 24, length.out = 50)
log_loss <- 12 - 0.65 * log_compute + rnorm(50, mean = 0, sd = 0.12)

df <- data.frame(
  compute = 10^log_compute, 
  loss = 10^log_loss
)

# 3. Build Enhanced Plot
p <- ggplot(df, aes(x = compute, y = loss)) +
  theme_minimal(base_family = "sans") +
  
  # Linear Regression Trend Line
  geom_smooth(
    method = "lm", 
    color = okabe_ito["vermillion"], 
    fill = okabe_ito["vermillion"],
    alpha = 0.12, 
    linewidth = 1.1,
    se = FALSE
  ) +
  
  # Data Points
  geom_point(
    color = okabe_ito["blue"], 
    size = 2.5, 
    alpha = 0.85
  ) +
  
  # Annotate the 10x Compute Span (Dashed Lines & Arrows)
  geom_vline(xintercept = c(1e23, 1e24), linetype = "dashed", color = "grey50", linewidth = 0.5) +
  annotate(
    "segment", 
    x = 1e23, xend = 1e24, 
    y = 0.5, yend = 0.5,
    arrow = arrow(ends = "both", length = unit(0.2, "cm")),
    color = okabe_ito["black"], linewidth = 0.6
  ) +
  
  # 10x Compute Text Annotation
  annotate(
    "text", 
    x = 10^23.5, y = 0.65, 
    label = "10x Compute", 
    color = okabe_ito["black"], 
    fontface = "bold", 
    size = 3.8
  ) +
  
  # FIXED: Passing "richtext" as a string name to annotate()
  annotate(
    "richtext",
    x = 1e21, y = 10,
    label = "Each **10x** increase in compute buys a **steady,**<br>**predictable drop in error** — but costs exponentially more.",
    fill = "#F8F9FA",
    color = "#212529",
    label.color = "grey80",
    label.padding = unit(0.5, "lines"),
    label.r = unit(0.3, "lines"),
    size = 3.6,
    hjust = 0
  ) +
  
  # Logarithmic Scales
  scale_x_log10(
    name = "Compute Spent (FLOP)",
    breaks = 10^(18:24),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  scale_y_log10(
    name = "Loss / Prediction Error",
    breaks = 10^(-1:3),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  
  # Titles and Subtitles
  labs(
    title = "**MORE COMPUTE → LOWER ERROR** <span style='color:#0072B2;'>(SMARTER MODEL)</span>",
    subtitle = "Illustrative scaling laws based on *Kaplan et al. (2020)* & *Hoffmann et al. (2022)*",
    caption = "Log-Log Scale Plot| Plotted by RProDigest, 26.7.2026"
  ) +
  
  # Custom Styling
  theme(
    plot.title = element_markdown(size = 15, face = "bold", margin = margin(b = 4)),
    plot.subtitle = element_markdown(size = 11, color = "grey30", margin = margin(b = 15)),
    plot.caption = element_text(size = 8, color = "grey50", hjust = 1),
    
    axis.title.x = element_text(size = 11, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 11, face = "bold", margin = margin(r = 10)),
    axis.text = element_text(size = 9.5, color = "grey20"),
    
    panel.grid.major = element_line(color = "#E9ECEF", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    
    plot.margin = margin(20, 20, 20, 20)
  )




ggsave(
  filename = "scaling_laws_twitter.png",
  plot = p,
  width = 10,          # Width in inches
  height = 5.625,      # Height in inches (16:9 aspect ratio)
  dpi = 300,           # High-resolution (crisp on mobile & desktop)
  bg = "white"         # Ensures background isn't transparent on Twitter feed
)
