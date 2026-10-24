# ==============================================================================
# GOOGLE TREND ATLAS  |  @RProDigest
# "One country, many searches": what each US state is searching for right now
# on Google, coloured by a 3x3 bivariate scheme.
#
#   X dimension  NATIONAL ECHO   share of a state's trending search VOLUME that goes
#                                to searches also trending in the US national feed
#                                (high = searching what America searches)
#   Y dimension  STAYING POWER   median hours since the state's current trending
#                                searches started trending
#                                (high = slow-burn interest, low = fresh spikes)
#
#   Each state is labelled with its highest-volume trending search.
#
# Run live:    Rscript google_trend_atlas.R
# Run offline: Rscript google_trend_atlas.R --offline   (uses google_snapshots.csv)
#
# Data: Google Trends "Trending now" RSS, one feed per state:
#       https://trends.google.com/trending/rss?geo=US-CA
# Unlike X, Google publishes a feed for every state and DC, so no state has to
# be borrowed from a neighbour. A state is hatched only if its feed fails.
# ==============================================================================

pkgs <- c(
  "httr2", "xml2", "dplyr", "tidyr", "purrr", "readr", "stringr", "glue",
  "ggplot2", "ggtext", "ragg", "sf", "tigris", "ggrepel", "patchwork",
  "systemfonts"
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
  library(xml2)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
  library(stringr)
  library(glue)
  library(ggplot2)
  library(ggtext)
  library(ragg)
  library(sf)
  library(ggrepel)
  library(patchwork)
})

options(tigris_use_cache = TRUE)

# 1. Configuration -------------------------------------------------------------
rss_base  <- "https://trends.google.com/trending/rss?geo="
tz_target <- "America/New_York"
tz_label  <- "ET"
max_age_h <- 48   # cap on hours-since-start, so one stale item can't dominate
cache_csv <- "google_snapshots.csv"

args       <- commandArgs(trailingOnly = TRUE)
is_offline <- "--offline" %in% args

# systemfonts >= 1.2 can fetch Google Fonts for the session if they aren't
# installed; older versions skip this and fall back below.
if ("require_font" %in% getNamespaceExports("systemfonts")) {
  for (f in c("Merriweather", "Roboto Condensed")) {
    try(systemfonts::require_font(f, fallback = NULL), silent = TRUE)
  }
}

installed_fonts <- unique(c(
  systemfonts::system_fonts()$family,
  systemfonts::registry_fonts()$family
))
if (!"Merriweather" %in% installed_fonts) {
  message("-> Merriweather not found; title uses a fallback serif. ",
          "Install it from fonts.google.com or update systemfonts (>= 1.2).")
}
body_family  <- if ("Roboto Condensed" %in% installed_fonts) "Roboto Condensed" else "sans"
title_family <- if ("Merriweather" %in% installed_fonts) "Merriweather" else "serif"

state_codes <- c(state.abb, "DC")

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

format_local <- function(t, fmt) format(t, tz = tz_target, format = fmt)

norm_term <- function(x) {
  x |> str_to_lower() |> str_replace_all("[^a-z0-9 ]", " ") |> str_squish()
}

# Google returns lowercase terms; title-case them for display, but keep
# small joining words lowercase and common acronyms in capitals.
acronyms <- c("nfl", "nba", "mlb", "nhl", "wnba", "mls", "ufc", "wwe", "ncaa", "usa",
              "uk", "ai", "fc", "sec", "acc", "cnn", "fbi", "cia", "irs", "ufo", "nasa",
              "cj", "aj", "tj", "jj", "nsaa", "fl",
              "ucla", "usc", "lsu", "tcu", "byu", "smu", "unlv", "uconn", "espn", "hbo",
              "gop", "dnc", "rnc", "fema", "noaa", "cdc", "fda", "nyc", "dc")
pretty_term <- function(x) {
  x |>
    str_to_title() |>
    str_replace_all("\\b(Vs|And|Of|The|In|On|At|For|To)\\b", \(w) str_to_lower(w)) |>
    str_replace_all(
      regex(paste0("\\b(", paste(acronyms, collapse = "|"), ")\\b"), ignore_case = TRUE),
      \(w) str_to_upper(w)
    ) |>
    str_replace_all("\\bF\\.c\\b\\.?", "F.C.") |>
    (\(s) paste0(str_to_upper(str_sub(s, 1, 1)), str_sub(s, 2)))()
}

# "2000+" -> 2000, "10K+" -> 10000, "1M+" -> 1e6
parse_traffic <- function(x) {
  num  <- readr::parse_number(x)
  mult <- case_when(
    str_detect(x, "[Mm]") ~ 1e6,
    str_detect(x, "[Kk]") ~ 1e3,
    .default = 1
  )
  num * mult
}

# 2. Data acquisition ----------------------------------------------------------
child_text <- function(node, name) {
  node |>
    xml_find_first(glue("./*[local-name()='{name}']")) |>
    xml_text()
}

parse_feed <- function(xml_text, geo) {
  items <- read_xml(xml_text) |> xml_find_all("//item")
  if (length(items) == 0L) return(NULL)

  tibble(
    geo         = geo,
    title       = map_chr(items, \(i) child_text(i, "title")),
    traffic_txt = map_chr(items, \(i) child_text(i, "approx_traffic")),
    pub_raw     = map_chr(items, \(i) child_text(i, "pubDate")),
    news_title  = map_chr(items, \(i) {
      i |>
        xml_find_first("./*[local-name()='news_item']/*[local-name()='news_item_title']") |>
        xml_text()
    })
  ) |>
    mutate(
      pub_utc       = readr::parse_datetime(pub_raw, "%a, %d %b %Y %H:%M:%S %z"),
      retrieved_utc = Sys.time()
    ) |>
    select(-pub_raw)
}

fetch_feed <- function(geo) {
  request(paste0(rss_base, geo)) |>
    req_user_agent("RProDigest-GoogleTrendAtlas/1.0 (public trend visualisation; @RProDigest)") |>
    req_timeout(20) |>
    req_retry(max_tries = 3) |>
    req_throttle(rate = 1 / 2) |>   # polite: one request every ~2 seconds
    req_perform() |>
    resp_body_string()
}
safe_feed <- possibly(fetch_feed, otherwise = NULL)

scrape_live <- function() {
  geos <- c("US", paste0("US-", state_codes))
  message(glue("-> Fetching {length(geos)} Google Trends feeds (throttled)..."))
  fetch_set <- function(geo_set, pass) {
    map(seq_along(geo_set), \(i) {
      geo <- geo_set[i]
      xml <- safe_feed(geo)
      out <- if (is.null(xml)) NULL else possibly(parse_feed, NULL)(xml, geo)
      message(glue("   [{pass} {i}/{length(geo_set)}] {geo}{if (is.null(out)) '  (failed)' else ''}"))
      out
    }) |>
      list_rbind()
  }

  got    <- fetch_set(geos, "pass 1")
  failed <- setdiff(geos, got$geo)

  # Second chance for feeds that failed (usually transient rate limiting)
  if (length(failed) > 0) {
    message(glue("-> Retrying {length(failed)} failed feed(s) after a pause: {paste(failed, collapse = ', ')}"))
    Sys.sleep(20)
    got <- bind_rows(got, fetch_set(failed, "retry"))
  }
  still_failed <- setdiff(geos, got$geo)
  if (length(still_failed) > 0) {
    warning("Feeds unavailable after retry: ", paste(still_failed, collapse = ", "), call. = FALSE)
  }
  got
}

load_cache <- function() {
  if (!file.exists(cache_csv)) {
    stop(glue("No live data and no '{cache_csv}' cache found."), call. = FALSE)
  }
  message(glue("-> Loading cached dataset from '{cache_csv}'..."))
  read_csv(cache_csv, show_col_types = FALSE)
}

if (is_offline) {
  raw <- load_cache()
} else {
  raw <- scrape_live()
  if (NROW(raw) == 0L || !"US" %in% raw$geo) {
    warning("Live fetch failed or returned no national feed; using cache.", call. = FALSE)
    raw <- load_cache()
  }
}

# 3. Validation ----------------------------------------------------------------
raw <- raw |>
  filter(!is.na(title), nzchar(title)) |>
  distinct(geo, title, .keep_all = TRUE) |>
  mutate(
    traffic = parse_traffic(traffic_txt),
    key     = norm_term(title)
  )

if (!"US" %in% raw$geo) stop("National (geo=US) feed missing.", call. = FALSE)
write_csv(raw |> select(-key), cache_csv)

capture_time <- max(raw$retrieved_utc)
if (as.numeric(difftime(Sys.time(), capture_time, units = "hours")) > 3) {
  warning("Newest capture is over three hours old.", call. = FALSE)
}

national <- raw |> filter(geo == "US") |> arrange(desc(traffic), pub_utc)
nat_keys <- national$key

# A state search "echoes" the nation if it matches a national trend exactly,
# or one contains the other (e.g. "arsenal" vs "arsenal vs everton").
# Containment needs 4+ characters so short words don't match by accident.
echoes_nation <- function(k) {
  any(
    k == nat_keys |
      (nchar(k) >= 4 & str_detect(nat_keys, fixed(k))) |
      (nchar(nat_keys) >= 4 & str_detect(k, fixed(nat_keys)))
  )
}

# 4. State metrics -------------------------------------------------------------
state_items <- raw |>
  filter(geo != "US") |>
  mutate(
    state_abb = str_remove(geo, "^US-"),
    age_h     = pmin(as.numeric(difftime(retrieved_utc, pub_utc, units = "hours")), max_age_h),
    echo_hit  = map_lgl(key, echoes_nation),
    traffic_w = coalesce(traffic, 100)   # unparseable traffic counts as the smallest bucket
  )

state_top <- state_items |>
  arrange(desc(traffic), pub_utc) |>
  slice_head(n = 1, by = state_abb) |>
  select(state_abb, top_key = key, top_topic = title, top_traffic = traffic_txt, top_news = news_title)

state_metrics <- state_items |>
  summarise(
    # Volume-weighted: share of the state's trending search traffic that goes
    # to national stories. Continuous, so tertiles don't collapse on ties the
    # way a simple count out of 10 does.
    echo    = sum(traffic_w[echo_hit]) / sum(traffic_w),
    stay    = median(age_h, na.rm = TRUE),
    n_items = n(),
    .by     = state_abb
  ) |>
  filter(n_items >= 3) |>
  left_join(state_top, by = join_by(state_abb))

if (nrow(state_metrics) < 6L) stop("Too few states with data to draw a bivariate map.", call. = FALSE)

# Tertiles, robust to heavy ties. If more than a third of states share the
# lowest value (typically echo = 0, because Google's national list is only 10
# searches), the usual cut-points collapse and the middle class disappears.
# Then: states at the lowest value form the low class, and the rest are split
# at their median into middle and high.
tie_collapsed <- function(ref) {
  mean(ref == min(ref)) >= 1 / 3
}

tertile <- function(x, ref) {
  if (!tie_collapsed(ref)) {
    q <- quantile(ref, c(1 / 3, 2 / 3), names = FALSE)
    return(findInterval(x, q, left.open = TRUE) + 1L)
  }
  lo   <- min(ref)
  rest <- ref[ref > lo]
  if (length(rest) == 0L) return(rep(2L, length(x)))
  m <- median(rest)
  case_when(x <= lo ~ 1L, x <= m ~ 2L, .default = 3L)
}

state_metrics <- state_metrics |>
  mutate(
    echo_t   = tertile(echo, echo),
    stay_t   = tertile(stay, stay),
    bi_class = paste0(echo_t, "-", stay_t)
  )

# Guard: tell the user if heavy ties still emptied a tertile class
for (dim in c("echo_t", "stay_t")) {
  empty <- setdiff(1:3, unique(state_metrics[[dim]]))
  if (length(empty) > 0) {
    warning(glue("Tertile class {paste(empty, collapse = ', ')} of {dim} is empty (heavy ties); ",
                 "the legend will show colours no state uses."), call. = FALSE)
  }
}

n_places   <- nrow(state_metrics)
n_states   <- sum(state_metrics$state_abb != "DC")
n_topics   <- n_distinct(state_metrics$top_key)
national_no1 <- pretty_term(national$title[1])

lead <- state_metrics |>
  count(top_key, sort = TRUE) |>
  slice_head(n = 1) |>
  left_join(state_metrics |> distinct(top_key, top_topic), by = join_by(top_key)) |>
  slice_head(n = 1)

# 5. Geometry ------------------------------------------------------------------
states_sf <- tigris::states(cb = TRUE, resolution = "20m", year = 2023, progress_bar = FALSE) |>
  filter(!STUSPS %in% c("PR", "VI", "GU", "MP", "AS")) |>
  tigris::shift_geometry() |>
  left_join(state_metrics, by = join_by(STUSPS == state_abb)) |>
  mutate(label_txt = if_else(
    is.na(top_topic), NA_character_,
    paste0(STUSPS, "  ", str_trunc(pretty_term(top_topic), 22))
  ))

# 6. Bivariate palette (matches the reference map) -----------------------------
bi_pal <- c(
  "1-1" = "#F3F3F3", "2-1" = "#F9D9A6", "3-1" = "#E8A202",
  "1-2" = "#CFE7F5", "2-2" = "#BDB6D8", "3-2" = "#8E6BB8",
  "1-3" = "#55B4E6", "2-3" = "#4C74B2", "3-3" = "#2B2E46"
)
ink          <- "#1F2937"
muted        <- "#6B7280"
no_data_fill <- "#FFFFFF"
no_data_line <- "#CBD5E1"

make_hatch <- function(polys, spacing = 45000) {
  if (nrow(polys) == 0L) return(NULL)
  bb <- sf::st_bbox(polys)
  h  <- unname(bb["ymax"] - bb["ymin"])
  w  <- unname(bb["xmax"] - bb["xmin"])
  lines <- lapply(seq(-h, w, by = spacing), \(o) {
    sf::st_linestring(rbind(
      c(bb[["xmin"]] + o,     bb[["ymin"]]),
      c(bb[["xmin"]] + o + h, bb[["ymax"]])
    ))
  })
  hatch <- sf::st_sfc(lines, crs = sf::st_crs(polys)) |>
    sf::st_intersection(sf::st_union(polys))
  sf::st_sf(geometry = hatch[!sf::st_is_empty(hatch)])
}

# 7. Map -----------------------------------------------------------------------
label_data    <- states_sf |> filter(!is.na(label_txt))
no_data_sf    <- states_sf |> filter(is.na(bi_class))
no_data_hatch <- make_hatch(no_data_sf)

p_map <- ggplot(states_sf) +
  geom_sf(aes(fill = bi_class), color = "#FFFFFF", linewidth = 0.4) +
  scale_fill_manual(values = bi_pal, na.value = no_data_fill, guide = "none") +
  { if (!is.null(no_data_hatch)) geom_sf(data = no_data_hatch, color = no_data_line, linewidth = 0.25) } +
  geom_sf(data = no_data_sf, fill = NA, color = no_data_line, linewidth = 0.4) +
  geom_label_repel(
    data          = label_data,
    aes(geometry = geometry, label = label_txt),
    stat          = "sf_coordinates",
    fun.geometry  = sf::st_point_on_surface,
    family        = body_family,
    size          = 2.35,
    fontface      = "bold",
    color         = ink,
    fill          = alpha("#FFFFFF", 0.88),
    label.padding = unit(0.13, "lines"),
    label.r       = unit(0.08, "lines"),
    segment.color = "#9CA3AF",
    segment.size  = 0.3,
    min.segment.length = 0.15,
    box.padding   = 0.2,
    max.overlaps  = Inf,
    seed          = 42
  ) +
  coord_sf(clip = "off") +
  theme_void(base_family = body_family)

# 8. Bivariate legend ----------------------------------------------------------
legend_df <- expand_grid(x = 1:3, y = 1:3) |>
  mutate(bi_class = paste0(x, "-", y))

corner_lab <- tribble(
  ~x,   ~y,   ~hjust, ~vjust, ~label,
  0.5,  3.62, 0,      0,      "Local obsession",
  3.5,  3.62, 1,      0,      "All-in on America",
  0.5,  0.38, 0,      1,      "Doing its own thing",
  3.5,  0.38, 1,      1,      "National wave"
)

p_leg <- ggplot(legend_df, aes(x, y, fill = bi_class)) +
  geom_tile(color = "#FFFFFF", linewidth = 0.9) +
  geom_text(
    data = corner_lab,
    aes(x, y, label = label, hjust = hjust, vjust = vjust),
    inherit.aes = FALSE,
    family = body_family, fontface = "bold", size = 2.6,
    color = muted
  ) +
  scale_fill_manual(values = bi_pal, guide = "none") +
  labs(
    x       = "National echo →",
    y       = "Staying power →",
    caption = if (nrow(no_data_sf) > 0) "<b>Hatched&#58;</b> feed unavailable at capture" else NULL
  ) +
  coord_fixed(clip = "off") +
  theme_void(base_family = body_family) +
  theme(
    axis.title.x = element_text(size = 9, face = "bold", color = ink, margin = margin(t = 13)),
    axis.title.y = element_text(size = 9, face = "bold", color = ink, angle = 90, margin = margin(r = 8)),
    plot.caption = element_markdown(
      size = 7.8, color = muted, hjust = 0, lineheight = 1.3, margin = margin(t = 10)
    ),
    plot.caption.position = "plot",
    plot.margin  = margin(t = 18, r = 8, b = 4, l = 8)
  )

# 9. Titles and composition ----------------------------------------------------
title_text <- if (n_topics == 1L) {
  "One Country. One Search."
} else {
  glue("One Country. {n_topics} Different Top Searches.")
}

subtitle_html <- glue(
  "<b>{escape_ggtext(pretty_term(lead$top_topic))}</b> is the top trending search in ",
  "<b>{lead$n}</b> of {n_places} places ({n_states} states",
  "{if ('DC' %in% state_metrics$state_abb) ' + DC' else ''})&#46; ",
  "Nationally, <b>{escape_ggtext(national_no1)}</b> leads&#46;<br>",
  "Colour shows whether a state's searches ",
  "<span style='color:#C98A00;'><b>echo the national list</b></span> and how long they have ",
  "<span style='color:#2F8FC4;'><b>stayed trending</b></span>&#46; ",
  "Captured&#58; <b>{format_local(capture_time, '%d %B %Y, %H&#58;%M')} {tz_label}</b>"
)

caption_html <- glue(
  "<b>Source&#58;</b> Google Trends, Trending now (per-state RSS)  •  ",
  "<b>Graphic&#58;</b> @RProDigest  •  <b>Stack&#58;</b> R + tidyverse + sf<br>",
  "<span style='color:{muted};'>",
  "National echo &#61; share of a state's trending search volume going to searches that also trend nationally&#46; ",
  "Staying power &#61; median hours since those searches started trending (capped at {max_age_h}h)&#46; ",
  if (tie_collapsed(state_metrics$echo)) {
    "<br>Many states share no searches with the national list&#59; those form the low echo class and the rest are split at their median&#46; Staying power classes are tertiles&#46;<br>"
  } else {
    "Classes are tertiles across states&#46;<br>"
  },
  "Labels show each state's highest-volume trending search&#46; ",
  "Google's traffic figures are bucketed estimates, so ties are broken by the earliest start&#46;</span>"
)

layout <- "
AAAA#
AAAAB
"

final <- p_map + p_leg +
  plot_layout(design = layout, heights = c(1.1, 1)) +
  plot_annotation(
    title    = title_text,
    subtitle = subtitle_html,
    caption  = caption_html,
    theme    = theme(
      plot.background = element_rect(fill = "#FFFFFF", color = NA),
      plot.title      = element_text(
        family = title_family, face = "bold", size = 28, color = ink,
        hjust = 0.5, margin = margin(b = 8)
      ),
      plot.subtitle   = element_markdown(
        family = body_family, size = 11.5, color = "#374151",
        hjust = 0.5, halign = 0.5, lineheight = 1.35, margin = margin(b = 10)
      ),
      plot.caption    = element_markdown(
        family = body_family, size = 8.3, color = muted,
        hjust = 0, lineheight = 1.35, margin = margin(t = 10)
      ),
      plot.margin     = margin(t = 22, r = 24, b = 16, l = 24)
    )
  )

# 10. High-DPI export ----------------------------------------------------------
ggsave(
  filename = "google_trend_atlas.png",
  plot     = final,
  device   = ragg::agg_png,
  width    = 13,
  height   = 9.2,
  units    = "in",
  dpi      = 320,
  bg       = "#FFFFFF"
)

write_csv(
  state_metrics |> arrange(state_abb),
  "google_state_metrics.csv"
)

message(glue(
  "Saved 'google_trend_atlas.png' ({n_places} places), 'google_state_metrics.csv' and '{cache_csv}'."
))


final