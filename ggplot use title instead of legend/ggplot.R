# Fancy ggplot with no legend reqiurements
# Credit to @rappa753
# Main change is definition of new vector called my_colors

if(!require(pacman)) install.packages("pacman")
pacman::p_load(
  tidyverse,
 palmerpenguins,
 glue,
 ggtext
)

# let me define colors as a named vector use https://imagecolorpicker.com/en to find the Hex codes

my_colors <- c(male = "#e2940c", female = "#0f9c6f") # newly added

penguins |>
  filter(!is.na(sex)) |>
  ggplot(aes(bill_length_mm, flipper_length_mm, fill = sex)) +
  geom_point(
    size = 3,
    alpha = 0.75,
    col = 'black',
    shape = 21
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = 'none',
    panel.grid.minor = element_blank(),
    plot.title.position = 'plot',
    plot.background = element_rect(fill = 'white', colour = NA),
    plot.title = element_markdown()
  ) +
  
  labs(
    x = 'Bill length (in mm)',
    y = 'Flipper length (in mm)',
    title = glue(
      'Measurements of <span style = "color:{my_colors["male"]}">**male**</span>
      and <span style = color:{my_colors["female"]}>**female**</span> penguins'
    )
  )+
  scale_fill_manual(values = my_colors) 

