# ==============================================================================
# X TREND ATLAS | @RProDigest
# Run live:    Rscript x_trends.R
# Run offline: Rscript x_trends.R --offline
# ==============================================================================

pkgs <- c(
  "httr2", "rvest", "dplyr", "tidyr", "purrr", "readr",
  "stringr", "forcats", "glue", "ggplot2", "ggtext", "ragg"
)

missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing) > 0) {
  stop(
    "Install required packages first:\ninstall.packages(c(",
    paste(sprintf('"%s"', missing), collapse = ", "), "))",
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(httr2)
  library(rvest)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
  library(stringr)
  library(forcats)
  library(glue)
  library(ggplot2)
  library(ggtext)
  library(ragg)
})

# 1. Configuration & CLI Arguments ---------------------------------------------
location   <- "south-africa" # e.g., "kenya", or "" for worldwide
place      <- "South Africa"
tz_target  <- "Africa/Johannesburg"
source_url <- if (nzchar(location)) {
  glue("https://trends24.in/{location}/")
} else {
  "https://trends24.in/"
}

args       <- commandArgs(trailingOnly = TRUE)
is_offline <- "--offline" %in% args

escape_ggtext <- function(x) {
  x |>
    str_replace_all("&", "&amp;") |>
    str_replace_all("<", "&lt;") |>
    str_replace_all(">", "&gt;") |>
    str_replace_all("\\.", "&#46;") |>
    str_replace_all(":", "&#58;") |>
    str_replace_all("_", "&#95;") |>
    str_replace_all("\\*", "&#42;") |>
    str_replace_all("`", "&#96;") |>
    str_replace_all("\\[", "&#91;") |>
    str_replace_all("\\]", "&#93;")
}

format_sast <- function(epoch_vec, fmt) {
  as.POSIXct(epoch_vec, origin = "1970-01-01", tz = "UTC") |>
    format(tz = tz_target, format = fmt)
}

# 2. Data Acquisition & Parsing (with Automatic Offline Fallback) --------------
parse_trend_card <- function(card, retrieved_utc, source_url, place) {
  epoch <- card |>
    html_element("[data-timestamp]") |>
    html_attr("data-timestamp") |>
    readr::parse_double(na = c("", "NA"))
  
  topics <- card |>
    html_elements("ol.trend-card__list li .trend-name > a") |>
    html_text2()
  
  if (length(topics) == 0L || length(epoch) != 1L || is.na(epoch)) {
    return(NULL)
  }
  
  tibble(
    epoch         = as.integer(round(epoch)),
    topic         = topics,
    rank          = seq_along(topics),
    retrieved_utc = retrieved_utc,
    source_url    = as.character(source_url),
    place         = place
  )
}

load_local_fallback <- function() {
  csv_path <- if (file.exists("snapshots.csv")) {
    "snapshots.csv"
  } else if (file.exists("x_trends/snapshots.csv")) {
    "x_trends/snapshots.csv"
  } else {
    NULL
  }
  
  html_path <- if (file.exists("source_snapshot.html")) {
    "source_snapshot.html"
  } else if (file.exists("x_trends/source_snapshot.html")) {
    "x_trends/source_snapshot.html"
  } else {
    NULL
  }
  
  if (!is.null(csv_path)) {
    message(glue("-> Loading cached dataset from '{csv_path}'..."))
    raw <- read_csv(csv_path, show_col_types = FALSE) |>
      mutate(epoch = as.integer(round(epoch)))
    req_cols <- c("epoch", "topic", "rank", "retrieved_utc", "source_url", "place")
    if (!all(req_cols %in% names(raw))) {
      stop(glue("Unexpected CSV schema in '{csv_path}'."), call. = FALSE)
    }
    return(raw)
  }
  
  if (!is.null(html_path)) {
    message(glue("-> Parsing cached HTML from '{html_path}'..."))
    html_text     <- read_file(html_path)
    retrieved_utc <- format(file.info(html_path)$mtime, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
    raw <- read_html(html_text) |>
      html_elements(".list-container") |>
      map(\(card) parse_trend_card(card, retrieved_utc, source_url, place)) |>
      list_rbind()
    return(raw)
  }
  
  stop(
    "Could not reach 'trends24.in' and no local 'snapshots.csv' or 'source_snapshot.html' was found.",
    call. = FALSE
  )
}

fetch_live_trends <- function(url) {
  request(url) |>
    req_user_agent("RProDigest-TrendAtlas/2.0 (public trend visualisation; @RProDigest)") |>
    req_timeout(20) |>
    req_perform()
}

safe_fetch <- possibly(fetch_live_trends, otherwise = NULL)

if (is_offline) {
  raw        <- load_local_fallback()
  place      <- first(raw$place)
  source_url <- first(raw$source_url)
} else {
  response <- safe_fetch(source_url)
  
  if (is.null(response)) {
    warning(
      "Live request to '", source_url, "' failed. Falling back to local snapshot archive.",
      call. = FALSE
    )
    raw        <- load_local_fallback()
    place      <- first(raw$place)
    source_url <- first(raw$source_url)
  } else {
    html_text     <- resp_body_string(response)
    retrieved_utc <- format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
    
    raw <- read_html(html_text) |>
      html_elements(".list-container") |>
      map(\(card) parse_trend_card(card, retrieved_utc, source_url, place)) |>
      list_rbind()
    
    if (nrow(raw) == 0L) {
      warning("No trend cards parsed from live page; falling back to local snapshot.", call. = FALSE)
      raw <- load_local_fallback()
    } else {
      write_lines(html_text, "source_snapshot.html")
    }
  }
}

# 3. Data Validation & Minute-Bucket Deduplication -----------------------------
raw <- raw |>
  mutate(epoch = as.integer(round(epoch)))

if (anyNA(raw$epoch) || anyNA(raw$topic) || any(raw$rank < 1L)) {
  stop("Invalid trend data detected (NAs or rank < 1).", call. = FALSE)
}

raw <- raw |>
  distinct(epoch, topic, .keep_all = TRUE)

if (nrow(raw) != nrow(distinct(raw, epoch, rank))) {
  stop("Duplicate (epoch, rank) pairs found in source snapshot.", call. = FALSE)
}

# Save full validated archive
write_csv(raw, "snapshots.csv")

# Deduplicate snapshots falling within the exact same calendar minute (YYYY-MM-DD HH:MM)
# keeping the newest epoch in that minute so no two columns share an identical HH:MM label
dedup_epochs <- raw |>
  distinct(epoch) |>
  mutate(minute_key = format_sast(epoch, "%Y-%m-%d %H:%M")) |>
  slice_max(order_by = epoch, n = 1, by = minute_key, with_ties = FALSE) |>
  pull(epoch)

raw_clean <- raw |>
  filter(epoch %in% dedup_epochs)

# 4. Chronological Ordering & Complete Matrix Construction ---------------------
times <- sort(unique(raw_clean$epoch)) |>
  tail(12)

n_snaps_total <- length(times)
if (n_snaps_total < 2L) {
  stop("At least two source snapshots are required for a history chart.", call. = FALSE)
}

latest_epoch <- max(times)
if (as.numeric(Sys.time()) - latest_epoch > 10800) {
  warning("Newest source snapshot is over three hours old.", call. = FALSE)
}

# Map each chronological snapshot to integer x = 1..n (1 = oldest, n = latest)
time_lookup <- tibble(
  epoch     = times,
  x_idx     = seq_along(times),
  time_str  = format_sast(times, "%H:%M"),
  is_latest = epoch == latest_epoch
)

window_stats <- raw_clean |>
  filter(epoch %in% times) |>
  summarize(
    n_snaps   = n(),
    best_rank = min(rank),
    .by       = topic
  )

leaders <- raw_clean |>
  filter(epoch == latest_epoch) |>
  arrange(rank) |>
  slice_head(n = 12) |>
  left_join(window_stats, by = join_by(topic)) |>
  mutate(
    y_idx       = rev(seq_len(n())),
    rank_badge  = sprintf("%02d", rank),
    persist     = glue("{n_snaps}/{n_snaps_total}h"),
    latest_band = case_when(
      rank <= 5L  ~ "01–05 (Peak)",
      rank <= 10L ~ "06–10 (High)",
      rank <= 20L ~ "11–20 (Mid)",
      .default    = "21+ (Tail)"
    )
  )

band_levels <- c(
  "01–05 (Peak)",
  "06–10 (High)",
  "11–20 (Mid)",
  "21+ (Tail)",
  "Not listed"
)

okabe_ito_bands <- c(
  "01–05 (Peak)" = "#0072B2", # Deep Blue
  "06–10 (High)" = "#56B4E9", # Sky Blue
  "11–20 (Mid)"  = "#009E73", # Bluish Green
  "21+ (Tail)"   = "#E69F00", # Orange
  "Not listed"   = "#E2E8F0"  # Cool Slate Grey
)

grid_df <- expand_grid(
  topic = leaders$topic,
  epoch = times
) |>
  left_join(
    raw_clean |> select(topic, epoch, rank),
    by = join_by(topic, epoch)
  ) |>
  left_join(
    leaders |> select(topic, latest_rank = rank, y_idx),
    by = join_by(topic)
  ) |>
  left_join(
    time_lookup,
    by = join_by(epoch)
  ) |>
  mutate(
    band = case_when(
      is.na(rank) ~ "Not listed",
      rank <= 5L  ~ "01–05 (Peak)",
      rank <= 10L ~ "06–10 (High)",
      rank <= 20L ~ "11–20 (Mid)",
      .default    = "21+ (Tail)"
    ) |> factor(levels = band_levels),
    rank_label = if_else(is.na(rank), "–", sprintf("%02d", rank)),
    text_tone  = case_when(
      is.na(rank) ~ "muted",
      rank <= 5L | (rank > 10L & rank <= 20L) ~ "light",
      .default    = "dark"
    )
  )

# 5. World-Class Multi-Column Table + Heatmap Visual ---------------------------
title_text <- glue("WHAT HAS {str_to_upper(place)} TALKING?")

display_url <- source_url |>
  str_remove("^https?://") |>
  str_remove("/$") |>
  escape_ggtext()

subtitle_html <- glue(
  "<b>{escape_ggtext(place)}</b>  •  Latest snapshot captured <b>{format_sast(latest_epoch, '%d %B %Y at %H&#58;%M')} SAST</b> (UTC+2)<br>",
  "<span style='color:#475569;'>Current <b>Top 12</b> trending topics traced chronologically from oldest (left) to newest (right) across <b>{n_snaps_total}</b> distinct snapshots&#46;</span>"
)

caption_html <- glue(
  "<b>Source&#58;</b> <span style='color:#0072B2;'>{display_url}</span>  •  ",
  "<b>Graphic&#58;</b> @RProDigest  •  <b>Stack&#58;</b> R + tidyverse<br>",
  "<span style='color:#64748B;'>Cells show rank at capture (01 = highest); ‘–’ indicates absence from that snapshot's top list&#46; ",
  "<b>STAY</b> counts appearances in the {n_snaps_total}-snapshot window&#46; Okabe-Ito colorblind-safe palette&#46;</span>"
)

# Left table spine coordinates (x <= 0) + Heatmap coordinates (x = 1..n_snaps_total)
x_badge   <- -4.8
x_topic   <- -4.25
x_persist <- -0.55

p <- ggplot(grid_df, aes(x = x_idx, y = y_idx)) +
  # Alternating subtle zebra striping across rows for horizontal tracking
  geom_rect(
    data = leaders |> filter(y_idx %% 2 == 1),
    aes(xmin = -5.25, xmax = n_snaps_total + 0.55, ymin = y_idx - 0.5, ymax = y_idx + 0.5),
    inherit.aes = FALSE,
    fill = "#F1F5F9",
    color = NA
  ) +
  # Soft highlight column behind the LATEST snapshot (far right)
  geom_rect(
    xmin  = n_snaps_total - 0.5,
    xmax  = n_snaps_total + 0.5,
    ymin  = 0.4,
    ymax  = 12.6,
    fill  = "#0072B2",
    alpha = 0.08,
    color = NA
  ) +
  # Column Header Bar
  annotate(
    "text",
    x        = c(x_badge, x_topic, x_persist, 1),
    y        = 13.25,
    label    = c("#", "TRENDING TOPIC", "STAY", "SNAPSHOT TIMELINE (SAST  •  OLDEST → LATEST)"),
    hjust    = c(0.5, 0, 0.5, 0),
    size     = 3.2,
    fontface = "bold",
    color    = "#475569",
    family   = "sans"
  ) +
  # Thin structural divider line under column headers
  annotate(
    "segment",
    x         = -5.25,
    xend      = n_snaps_total + 0.5,
    y         = 12.65,
    yend      = 12.65,
    color     = "#CBD5E1",
    linewidth = 0.6
  ) +
  # Time labels along the top of the heatmap (historical columns)
  geom_text(
    data        = time_lookup |> filter(!is_latest),
    aes(x = x_idx, y = 12.92, label = time_str),
    inherit.aes = FALSE,
    size        = 3.3,
    color       = "#475569",
    family      = "sans"
  ) +
  # Emphasized timestamp + badge for the LATEST snapshot on the far right
  geom_text(
    data        = time_lookup |> filter(is_latest),
    aes(x = x_idx, y = 13.02, label = time_str),
    inherit.aes = FALSE,
    size        = 3.5,
    fontface    = "bold",
    color       = "#0072B2",
    family      = "sans"
  ) +
  geom_text(
    data        = time_lookup |> filter(is_latest),
    aes(x = x_idx, y = 12.78, label = "LATEST"),
    inherit.aes = FALSE,
    size        = 2.3,
    fontface    = "bold",
    color       = "#0072B2",
    family      = "sans"
  ) +
  # Left Spine Column 1: Colored Rank Badge Tile
  geom_tile(
    data        = leaders,
    aes(x = x_badge, y = y_idx, fill = latest_band),
    inherit.aes = FALSE,
    width       = 0.62,
    height      = 0.68,
    color       = NA
  ) +
  geom_text(
    data = leaders,
    aes(
      x     = x_badge,
      y     = y_idx,
      label = rank_badge,
      color = if_else(rank <= 5L | rank > 10L, "light", "dark")
    ),
    inherit.aes = FALSE,
    size        = 3.4,
    fontface    = "bold",
    family      = "sans"
  ) +
  # Left Spine Column 2: Topic Name
  geom_text(
    data        = leaders,
    aes(x = x_topic, y = y_idx, label = str_trunc(topic, 28)),
    inherit.aes = FALSE,
    hjust       = 0,
    size        = 4.0,
    fontface    = "bold",
    color       = "#0F172A",
    family      = "sans"
  ) +
  # Left Spine Column 3: Persistence Counter
  geom_text(
    data        = leaders,
    aes(x = x_persist, y = y_idx, label = persist),
    inherit.aes = FALSE,
    hjust       = 0.5,
    size        = 3.3,
    fontface    = "bold",
    color       = "#64748B",
    family      = "sans"
  ) +
  # Primary Heatmap Tiles
  geom_tile(
    aes(fill = band),
    width     = 0.88,
    height    = 0.82,
    color     = "#F8FAFC",
    linewidth = 0.6
  ) +
  # Rank Numerals inside Heatmap Cells
  geom_text(
    aes(label = rank_label, color = text_tone),
    size     = 3.6,
    fontface = "bold",
    family   = "sans"
  ) +
  scale_fill_manual(
    values = okabe_ito_bands,
    breaks = band_levels,
    name   = "Rank Tier",
    drop   = FALSE
  ) +
  scale_color_manual(
    values = c(
      "light" = "#FFFFFF",
      "dark"  = "#0F172A",
      "muted" = "#94A3B8"
    ),
    guide = "none"
  ) +
  scale_x_continuous(
    limits = c(-5.3, n_snaps_total + 0.6),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0.35, 13.6),
    expand = c(0, 0)
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title    = title_text,
    subtitle = subtitle_html,
    x        = NULL,
    y        = NULL,
    caption  = caption_html
  ) +
  guides(
    fill = guide_legend(
      nrow           = 1,
      title.position = "left",
      label.position = "right",
      keywidth       = unit(1.1, "lines"),
      keyheight      = unit(1.1, "lines"),
      override.aes   = list(color = NA)
    )
  ) +
  theme_void(base_size = 13, base_family = "sans") +
  theme(
    plot.background       = element_rect(fill = "#F8FAFC", color = NA),
    panel.background      = element_rect(fill = "#F8FAFC", color = NA),
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    plot.title            = element_text(
      size   = 23,
      face   = "bold",
      color  = "#0F172A",
      margin = margin(b = 6)
    ),
    plot.subtitle         = element_markdown(
      size       = 11.5,
      color      = "#1E293B",
      lineheight = 1.35,
      margin     = margin(b = 14)
    ),
    legend.position       = "top",
    legend.justification  = "left",
    legend.title          = element_text(size = 10, face = "bold", color = "#0F172A", margin = margin(r = 10)),
    legend.text           = element_text(size = 9.5, color = "#334155", margin = margin(r = 14)),
    legend.margin         = margin(b = 8, l = 0),
    plot.caption          = element_markdown(
      size       = 9.0,
      color      = "#475569",
      hjust      = 0,
      lineheight = 1.35,
      margin     = margin(t = 16)
    ),
    plot.margin           = margin(t = 24, r = 24, b = 20, l = 24)
  )

# 6. High-DPI Export via ragg --------------------------------------------------
ggsave(
  filename = "x_trends.png",
  plot     = p,
  device   = ragg::agg_png,
  width    = 12.5,
  height   = 8.0,
  units    = "in",
  dpi      = 320
)

print(p)
message("Saved high-resolution 'x_trends.png' (320 DPI) and 'snapshots.csv'.")