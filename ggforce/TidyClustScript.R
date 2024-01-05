##############################################
## Written by @RProDigest
## Date 5th Jan 2024
## How clustering helps to identify Outliers
##############################################


# Load necessary libraries for data manipulation, visualization, and clustering

library(tidyverse)
library(tidymodels)
library(tidyclust)
library(ggtext)
library(ggforce)
library(palmerpenguins)

#' Prepare the penguins dataset by selecting relevant columns and removing missing
#'  values

penguins |>
  select(flipper_length_mm, bill_depth_mm) |>
  drop_na() -> penguins_two


#' Define a K-means clustering model with 3 clusters, setting the number of starts
#'  to 20 for robustness

kmeans_spec <- k_means(num_clusters = 3) %>%
  set_mode("partition") %>%
  set_engine("stats") %>%
  set_args(nstart = 20)

# Set a random seed for reproducibility and fit the K-means model to the data

set.seed(1234)
kmeans_fit <- kmeans_spec %>%
  fit( ~ ., data = penguins_two)


augment(kmeans_fit, new_data = penguins_two) |>
  ggplot(aes(flipper_length_mm, bill_depth_mm, color = .pred_cluster)) +
  geom_point()


# Prepare a K-means specification for tuning with a range of cluster numbers

kmeans_spec_tuned <- kmeans_spec %>%
  set_args(num_clusters = tune())


# Create a workflow to encapsulate the model and formula

kmeans_wf <- workflow() %>%
  add_model(kmeans_spec_tuned) %>%
  add_formula( ~ .)

#' Perform bootstrapping for model evaluation and define a grid of cluster numbers
#'  for tuning

set.seed(1234)
x_boots <- bootstraps(penguins_two, times = 10)

num_clusters_grid <- tibble(num_clusters = seq(1, 10))

# Tune the model across different numbers of clusters and visualize the results

tune_res <- tune_cluster(object = kmeans_wf,
                         resamples = x_boots,
                         grid = num_clusters_grid)

# Plot the tuning results to determine the optimal number of clusters

tune_res %>%
  autoplot()


# Update the model with the optimal number of clusters and fit it to the data

final_kmeans <- kmeans_wf %>%
  update_model(kmeans_spec %>% set_args(num_clusters = 2)) %>%
  fit(penguins_two)

# Visualize the final clustering results with enhanced aesthetics and annotations

augment(final_kmeans, new_data = penguins_two) %>%
  ggplot(aes(flipper_length_mm, bill_depth_mm, color = .pred_cluster)) +
  geom_point() +
  scale_color_manual(values = c(
    'Cluster_1' = '#0082f0',
    'Cluster_2' = '#0ec273'
  )) +
  theme_minimal() +
  labs(
    x = "Flipper Length(mm)",
    y = "Bill Depth(mm)",
    subtitle = "",
    caption = "Source: Palmer Penguins Dataset<br> Plotted by @RProDigest",
    #title = "<span style = 'color:#0082f0;'>**Adelie & Chinstrap**</span> have 
    #smaller flipper lengths than <span style = 'color:#0ec273;'>Gentoo</span>"
    
    title = "<span style = 'color:#0082f0;'>**Adelie & Chinstrap Penguins**</span>: A Tale of 
    <span style = 'color:#0082f0;'>Shorter Flippers</span> vs. 
    <span style = 'color:#0ec273;'>Gentoo's Longer Reach</span>"
    
  ) +
  theme(
#    coord_cartesian(expand = FALSE),
    plot.caption = element_markdown(lineheight = 1.2, face = 'bold'),
    plot.title.position = 'plot',
    plot.title = element_markdown(
      lineheight = 1.2,
      face = 'bold',
     # hjust = 0.5,
      size = 16
    ),
    legend.position = 'none',
     axis.text = element_text(size=14),
     axis.title = element_text(size = 14),
    #  axis.ticks.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0, color = "white"),
    panel.grid.major.y = element_blank(),
    axis.line.x = element_line(linewidth = 0.25, colour = "grey"),
    axis.line.y = element_line(linewidth = 0.25, colour = "grey"),
    plot.caption.position = 'plot'
  ) +
  ggforce::geom_mark_rect(
    con.colour = "#0ec273",
    label.colour = "#0ec273",
    aes(
      label = "Outliers in Gentoo Bill Depth",
      filter = (bill_depth_mm > 17.5 &  flipper_length_mm > 204)
      
    )
  )


# Save the final plot as an image file
ggsave("example_ggfotify.png")
