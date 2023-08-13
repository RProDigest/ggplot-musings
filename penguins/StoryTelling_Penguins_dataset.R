# @RProdigest
# 13th August 2023

library(tidyverse)
library(patchwork)
library(ggtext)
library(palmerpenguins)

p <- penguins |> 
  filter(!is.na(sex)) |> 
  ggplot(aes(flipper_length_mm, body_mass_g, color = species)) +
  geom_point(aes(shape = sex), size = 2) +
  geom_smooth(aes(linetype = sex, group = interaction(species, sex)), method = lm, se = FALSE, alpha = 0.5) +
  scale_color_manual(values = c("Adelie" = "#D55E00", "Gentoo" = "#0072B2", "Chinstrap" = "#009E73")) +
  labs(
    x = "flipper length (mm)", 
    y = "body mass (g)",
    title = "Body Mass vs Flipper Length"
  ) +
  scale_x_continuous(
    breaks = seq(170, 240, 10),
    position = 'top'
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0),
    axis.title.x = element_text(hjust = 0, vjust = 3),
    axis.title.y = element_text(vjust = 0, angle = 90, hjust = 1),
    legend.position = c(0, 1),
    legend.justification = c(0, 1),
    legend.title = element_blank(),
    plot.background = element_rect(fill = 'white', colour = NA)
  ) +
  guides(
    color = 'none',
    shape = guide_legend(override.aes = list(linetype = 0))
  )

title_text <- "**Observation:** <span style = 'color:#0072B2'>Gentoo penguins have larger body mass and flipper lengths</span>"
subtitle_text <- "**COMPARE:**<span style = 'color:#0072B2'> Gentoo's</span> distinct size advantage over <span style = 'color:#D55E00'>**Adelie</span> and <span style = 'color:#009E73'>**Chinstrap penguins**</span>.<br><span style = 'font-size:10pt;color:grey60'>Data sourced from the {palmerpenguins} dataset</span>"
caption_text <- "<span style = 'font-size:6pt;color:grey60'>Plotted by @RProdigest on twitter"

p +
  plot_annotation(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    theme = theme(
      plot.title = element_markdown(margin = margin(b = 0.4, unit = 'cm'), size = 14, hjust = 0),
      plot.subtitle = element_markdown(margin = margin(b = 0.4, unit = 'cm'), size = 11.5, hjust = 0),
      plot.caption.position = 'plot',
      plot.caption = element_markdown(hjust = 0, size = 7, lineheight = 1.25)
    )
  )