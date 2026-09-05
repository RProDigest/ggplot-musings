# ============================================================
# LET THE DATA SPEAK
# Reproducible ggplot2 infographic
# Created by @ProDigest
# ============================================================

# 1. Check packages -------------------------------------------------------

required_packages <- c("ggplot2", "patchwork")

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0) {
  stop(
    paste0(
      "Install the missing packages first:\n",
      "install.packages(c(",
      paste(sprintf('"%s"', missing_packages), collapse = ", "),
      "))"
    ),
    call. = FALSE
  )
}


# 2. Load packages --------------------------------------------------------

library(ggplot2)
library(patchwork)


# 3. Example data ---------------------------------------------------------

# Both charts use exactly the same data.
# Replace these figures with verified values if required.

results <- data.frame(
  period = factor(
    c("BEFORE", "AFTER"),
    levels = c("BEFORE", "AFTER")
  ),
  score = c(96, 100)
)


# 4. Colour palette -------------------------------------------------------

ink       <- "#102A43"
navy      <- "#073B4C"
blue      <- "#0072B2"  # Okabe-Ito blue
orange    <- "#E69F00"  # Okabe-Ito orange
grey      <- "#AAB2B9"
paper     <- "#F6F3EC"
muted     <- "#68737D"
grid_line <- "#D8D4CB"


# 5. Shared theme ---------------------------------------------------------

base_theme <- theme_minimal(
  base_family = "sans",
  base_size = 13
) +
  theme(
    plot.background = element_rect(
      fill = paper,
      colour = NA
    ),
    panel.background = element_rect(
      fill = paper,
      colour = NA
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(
      colour = grid_line,
      linewidth = 0.35
    ),
    axis.title = element_blank(),
    axis.text.x = element_text(
      colour = ink,
      face = "bold",
      size = 10,
      margin = margin(t = 8)
    ),
    axis.text.y = element_text(
      colour = muted,
      size = 9
    ),
    axis.ticks = element_blank(),
    plot.title = element_text(
      colour = ink,
      face = "bold",
      size = 16,
      margin = margin(b = 6)
    ),
    plot.subtitle = element_text(
      colour = muted,
      size = 10,
      lineheight = 1.05,
      margin = margin(b = 16)
    ),
    plot.margin = margin(
      t = 12,
      r = 18,
      b = 14,
      l = 18
    )
  )


# 6. Manipulated presentation --------------------------------------------

# coord_cartesian() zooms into the scale without removing
# the underlying bars.
#
# clip = "on" prevents the bars from extending outside
# the plotting panel.

manipulated <- ggplot(
  results,
  aes(
    x = period,
    y = score,
    fill = period
  )
) +
  geom_col(
    width = 0.62,
    show.legend = FALSE
  ) +
  geom_text(
    aes(label = score),
    vjust = -0.55,
    colour = ink,
    fontface = "bold",
    size = 5
  ) +
  scale_fill_manual(
    values = c(
      "BEFORE" = grey,
      "AFTER"  = orange
    )
  ) +
  scale_y_continuous(
    breaks = 96:100,
    expand = expansion(
      mult = c(0, 0.03)
    )
  ) +
  coord_cartesian(
    ylim = c(95, 101.5),
    clip = "on"
  ) +
  labs(
    title = "THE STORY SOMEONE WANTS",
    subtitle = paste0(
      "A truncated axis turns a 4-point change\n",
      "into a dramatic visual leap"
    )
  ) +
  base_theme


# 7. Honest presentation --------------------------------------------------

honest <- ggplot(
  results,
  aes(
    x = period,
    y = score,
    fill = period
  )
) +
  geom_col(
    width = 0.62,
    show.legend = FALSE
  ) +
  geom_text(
    aes(label = score),
    vjust = -0.55,
    colour = ink,
    fontface = "bold",
    size = 5
  ) +
  scale_fill_manual(
    values = c(
      "BEFORE" = grey,
      "AFTER"  = blue
    )
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 20),
    limits = c(0, 108),
    expand = expansion(
      mult = c(0, 0)
    )
  ) +
  coord_cartesian(
    clip = "on"
  ) +
  labs(
    title = "WHAT THE DATA ACTUALLY SAYS",
    subtitle = paste0(
      "The zero baseline restores proportion:\n",
      "96 to 100 is a 4.2% increase"
    )
  ) +
  base_theme


# 8. Footer ---------------------------------------------------------------

footer <- paste0(
  "Same data. Different framing.  |  ",
  "Check the baseline, scale, denominator and time window.  |  ",
  "Created by @ProDigest"
)


# 9. Combine the charts ---------------------------------------------------

visual <- manipulated + honest +
  plot_layout(
    ncol = 2,
    widths = c(1, 1)
  ) +
  plot_annotation(
    title = "LET THE DATA SPEAK",
    subtitle = paste0(
      "Numbers do not need an advocate. They need context.\n",
      "THE DANGER OF MANIPULATION"
    ),
    caption = footer,
    theme = theme(
      plot.background = element_rect(
        fill = paper,
        colour = NA
      ),
      plot.title = element_text(
        colour = navy,
        face = "bold",
        size = 25,
        hjust = 0,
        lineheight = 1.05,
        margin = margin(b = 8)
      ),
      plot.subtitle = element_text(
        colour = orange,
        face = "bold",
        size = 14,
        hjust = 0,
        lineheight = 1.1,
        margin = margin(b = 16)
      ),
      plot.caption = element_text(
        colour = muted,
        size = 9,
        hjust = 0,
        margin = margin(t = 16)
      ),
      plot.margin = margin(
        t = 24,
        r = 28,
        b = 22,
        l = 28
      )
    )
  )


# 10. Display the visual --------------------------------------------------

print(visual)


# 11. Export a high-resolution PNG ----------------------------------------

ggsave(
  filename = "let-the-data-speak.png",
  plot = visual,
  width = 13.333,
  height = 7.5,
  units = "in",
  dpi = 300,
  bg = paper
)


# 12. Export a scalable SVG -----------------------------------------------

ggsave(
  filename = "let-the-data-speak.svg",
  plot = visual,
  width = 13.333,
  height = 7.5,
  units = "in",
  bg = paper
)