library(tidyverse)
library(ggtext)
library(patchwork)

# ==============================================================================
# 0. TYPOGRAPHY: use a modern UI font if installed, otherwise fall back to sans
#    (install Inter from https://rsms.me/inter/ for the intended look)
# ==============================================================================
font_fam <- if ("Inter" %in% systemfonts::system_fonts()$family) "Inter" else "sans"

# ==============================================================================
# 1. THEME & OKABE-ITO COLORBLIND-SAFE CONFIGURATION
# ==============================================================================
dark_mode <- FALSE

pal <- list(
  bg         = if (dark_mode) "#0B0F19" else "#FFFFFF",
  panel_alt  = if (dark_mode) "#131A2A" else "#F8FAFC",
  text_main  = if (dark_mode) "#F8FAFC" else "#0F172A",
  text_muted = if (dark_mode) "#94A3B8" else "#475569",
  grid_line  = if (dark_mode) "#334155" else "#CBD5E1",
  curve_pre  = "#0072B2", # Okabe-Ito Blue: innovators & early adopters
  curve_post = "#009E73", # Okabe-Ito Bluish Green: mainstream
  chasm_fill = if (dark_mode) "#1E293B" else "#EEF2F6",
  accent_kpi = if (dark_mode) "#009E73" else "#0072B2",
  pillar_col = "#D55E00"  # Okabe-Ito Vermilion: the bridge / catalyst
)

# ==============================================================================
# 2. DIFFUSION CURVE: boundaries DERIVED from mu/sd so areas match the labels
#    (Rogers: mu-2sd | mu-sd | mu | mu+sd  ->  2.5 / 13.5 / 34 / 34 / 16 %)
# ==============================================================================
mu <- 0
sd_c <- 1.05
dens <- function(x) dnorm(x, mu, sd_c)

chasm_ctr   <- mu - sd_c         # the chasm sits on the EA / EM boundary
chasm_left  <- chasm_ctr - 0.20
chasm_right <- chasm_ctr + 0.20

curve_df <- tibble(x = seq(-3.2, 3.2, length.out = 1000)) |>
  mutate(
    y = dens(x),
    regime = case_when(
      x < chasm_left  ~ "Pre-Chasm",
      x > chasm_right ~ "Post-Chasm",
      TRUE            ~ "Chasm"
    )
  )

# Minor dividers (the chasm itself replaces the mu - sd divider)
dividers <- tibble(x = mu + c(-2, 0, 1) * sd_c)

# Segment labels: mid-points between boundaries; % computed, not typed
cuts <- c(-3.2, mu + c(-2, -1, 0, 1) * sd_c, 3.2)
segments_labels <- tibble(
  name = c("Innovators", "Early Adopters", "Early Majority", "Late Majority", "Laggards"),
  x    = (head(cuts, -1) + tail(cuts, -1)) / 2,
  pct  = diff(pnorm(c(-Inf, mu + c(-2, -1, 0, 1) * sd_c, Inf), mu, sd_c)) * 100
) |>
  mutate(
    x = replace(x, c(1, 5), c(-2.6, 2.1)),   # pull tail labels inward
    # Rogers' canonical rounding (exact normal areas are 2.3 / 13.6 / 34.1 / 34.1 / 15.9)
    label = glue::glue("{name}\n({c('2.5', '13.5', '34', '34', '16')}%)")
  )

# Three pillars: rich text = title + description in one object (no collisions)
pillars_df <- tibble(
  x = c(-2.35, -0.95, 0.45),
  y = 0.505,
  label = c(
    "**Software-Defined**<br><span style='font-size:8.5pt;color:%s'>GPU-accelerated L1/L2/L3 on<br>commercial off-the-shelf servers</span>",
    "**AI-Native**<br><span style='font-size:8.5pt;color:%s'>Unified compute fabric for<br>RAN + Edge AI inference</span>",
    "**Open Innovation**<br><span style='font-size:8.5pt;color:%s'>CUDA-X ecosystem & open<br>6G research frameworks</span>"
  ) |> sprintf(pal$text_muted)
)

# Bridge end-points: where the two curve pieces stop
y_l <- dens(chasm_left)
y_r <- dens(chasm_right)

# ==============================================================================
# 3. MAIN PLOT
# ==============================================================================
p_main <- ggplot() +
  # Chasm zone, drawn only up to the curve so it reads as a "gap", not a box
  annotate("rect", xmin = chasm_left, xmax = chasm_right, ymin = 0, ymax = y_r,
           fill = pal$chasm_fill) +
  annotate("text", x = chasm_ctr, y = 0.085, label = "THE CHASM", angle = 90,
           size = 3.2, fontface = "bold", color = pal$text_muted, family = font_fam) +

  # Areas
  geom_area(data = filter(curve_df, regime == "Pre-Chasm"), aes(x, y),
            fill = pal$curve_pre, alpha = 0.14) +
  geom_area(data = filter(curve_df, regime == "Post-Chasm"), aes(x, y),
            fill = pal$curve_post, alpha = 0.16) +

  # Minor dividers truncated at curve height
  geom_segment(data = dividers, aes(x = x, xend = x, y = 0, yend = dens(x)),
               color = pal$bg, linewidth = 0.8) +

  # Baseline, with the chasm floor in vermilion
  annotate("segment", x = -3.2, xend = 3.2, y = 0, yend = 0,
           color = pal$grid_line, linewidth = 0.6) +
  annotate("segment", x = chasm_left, xend = chasm_right, y = 0, yend = 0,
           color = pal$pillar_col, linewidth = 1.1) +

  # Curves
  geom_line(data = filter(curve_df, regime == "Pre-Chasm"), aes(x, y),
            color = pal$curve_pre, linewidth = 1.6, lineend = "round") +
  geom_line(data = filter(curve_df, regime == "Post-Chasm"), aes(x, y),
            color = pal$curve_post, linewidth = 1.8, lineend = "round") +

  # THE BRIDGE: the three pillars literally span the gap
  annotate("curve", x = chasm_left, y = y_l, xend = chasm_right, yend = y_r,
           curvature = -0.45, color = pal$pillar_col, linewidth = 1.3, lineend = "round") +
  annotate("point", x = chasm_left, y = y_l, color = pal$curve_pre, size = 2.6) +
  annotate("point", x = chasm_right, y = y_r, color = pal$curve_post, size = 3.6) +

  # Pillars bracket -> drop line lands on the bridge
  annotate("segment", x = -3.0, xend = 1.1, y = 0.435, yend = 0.435,
           color = pal$text_muted, linewidth = 0.5) +
  annotate("segment", x = c(-3.0, 1.1), xend = c(-3.0, 1.1), y = 0.435, yend = 0.447,
           color = pal$text_muted, linewidth = 0.5) +
  annotate("segment", x = chasm_ctr, xend = chasm_ctr, y = 0.435, yend = 0.305,
           arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
           color = pal$pillar_col, linewidth = 0.9) +
  geom_richtext(data = pillars_df, aes(x, y, label = label),
                family = font_fam, size = 3.9, color = pal$text_main,
                fill = NA, label.color = NA, lineheight = 1.25) +

  # Milestone: label sits INSIDE the post-chasm area, short leader to the point
  annotate("label", x = -0.68, y = 0.15,
           label = "Commercial Inflection:\n1st Live 5G AI-RAN Call (T-Mobile / Nokia)",
           hjust = 0, vjust = 0.5, size = 3.1, fontface = "bold", family = font_fam,
           fill = pal$panel_alt, color = pal$text_main, lineheight = 1.1,
           linewidth = 0.3, label.padding = unit(0.4, "lines")) +
  annotate("segment", x = -0.66, y = 0.178, xend = chasm_right + 0.03, yend = y_r - 0.012,
           color = pal$curve_post, linewidth = 0.7,
           arrow = arrow(length = unit(0.15, "cm"), type = "closed")) +

  # Segment labels
  geom_text(data = segments_labels, aes(x, y = -0.03, label = label),
            size = 3.3, fontface = "bold", color = pal$text_muted,
            lineheight = 1.1, family = font_fam) +

  coord_cartesian(xlim = c(-3.3, 3.3), ylim = c(-0.06, 0.55), clip = "off") +
  labs(
    title = "AI-RAN Is Crossing the Chasm",
    subtitle = glue::glue(
      "Transitioning from <b style='color:{pal$curve_pre}'>early trials</b> to ",
      "<b style='color:{pal$curve_post}'>commercial mainstream deployment</b> via unified, ",
      "<b style='color:{pal$pillar_col}'>software-defined GPU infrastructure</b>"
    )
  ) +
  theme_void(base_family = font_fam) +
  theme(
    plot.background  = element_rect(fill = pal$bg, color = NA),
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", size = 24, color = pal$text_main,
                              margin = margin(t = 8, b = 6)),
    plot.subtitle = element_markdown(size = 12.5, color = pal$text_muted,
                                     margin = margin(b = 4)),
    plot.margin = margin(t = 12, r = 24, b = 4, l = 24)
  )

# ==============================================================================
# 4. KPI STRIP: numbers + short labels, all metrics in the same grammar
# ==============================================================================
kpi_df <- tibble(
  x = 1:5,
  metric = c("19", "147", "6", "5", "4"),
  label = c("Leading Telco Trials", "AI-RAN Alliance Members", "NEP / RAN Providers",
            "Global Server OEMs", "Nations: UK · US · KR · JP")
)

p_kpi <- ggplot(kpi_df, aes(x = x)) +
  annotate("segment", x = 0.5, xend = 5.5, y = 1, yend = 1,
           color = pal$grid_line, linewidth = 0.5) +
  annotate("segment", x = 1.5:4.5, xend = 1.5:4.5, y = 0.2, yend = 0.85,
           color = pal$grid_line, linewidth = 0.5) +
  geom_text(aes(y = 0.62, label = metric), size = 9, fontface = "bold",
            color = pal$accent_kpi, family = font_fam) +
  geom_text(aes(y = 0.27, label = label), size = 3.4, fontface = "bold",
            color = pal$text_muted, family = font_fam) +
  coord_cartesian(xlim = c(0.5, 5.5), ylim = c(0.1, 1.02), expand = FALSE) +
  labs(caption = "Source: NVIDIA, \"From AI Infrastructure to the 6G Future\" (Nov 2026) &middot; Okabe-Ito colorblind-safe palette &middot; Graphic: **@RProDigest**") +
  theme_void(base_family = font_fam) +
  theme(
    plot.background = element_rect(fill = pal$bg, color = NA),
    plot.caption = element_markdown(size = 9, color = pal$text_muted, hjust = 1,
                                    margin = margin(t = 8, b = 4)),
    plot.margin = margin(t = 0, r = 24, b = 8, l = 24)
  )

# ==============================================================================
# 5. COMBINE & EXPORT (16:9 is safe for the X timeline; ragg = crisp text)
# ==============================================================================
final_plot <- p_main / p_kpi +
  plot_layout(heights = c(4.6, 1)) &
  theme(plot.background = element_rect(fill = pal$bg, color = NA))

ggsave("ai_ran_crossing_the_chasm_v2.png", final_plot,
       width = 12, height = 6.75, dpi = 200, bg = pal$bg,
       device = ragg::agg_png)
