# globals.R
# Loaded first by app.R. Contains libraries, data, constants, palettes,
# precomputed objects, and shared theme/helper functions.

# --- Libraries ----------------------------------------------------------------
library(shiny)
library(bslib)
library(tidyverse)
library(lubridate)
library(thematic)
library(plotly)
library(ggridges)

thematic_shiny()

# --- Data loading -------------------------------------------------------------
apps     <- readRDS("data/apps_clean.rds")
websites <- readRDS("data/websites_clean.rds")

# --- Constants ----------------------------------------------------------------
MIN_HOURS       <- 1
OTHER_COLOR     <- "#6c757d"
PRODUCTIVE_APPS <- c("Browser", "Development", "Communication", "Productivity", "OS & System")
PRODUCTIVE_WEB  <- c("Learning", "Documentation", "Work", "Development", "Utilities")

# --- Category lists -----------------------------------------------------------
app_cats <- apps |>
  group_by(category) |>
  summarise(hours = sum(active_duration) / 3600) |>
  filter(hours >= MIN_HOURS) |>
  arrange(desc(hours)) |>
  pull(category)

web_cats <- websites |>
  group_by(category) |>
  summarise(hours = sum(active_duration) / 3600) |>
  filter(hours >= MIN_HOURS, !is.na(category)) |>
  arrange(desc(hours)) |>
  pull(category)

apps     <- apps     |> filter(category %in% app_cats)
websites <- websites |> filter(category %in% web_cats)

# --- Color palettes -----------------------------------------------------------
tableau_20 <- c(
  "#4E79A7", "#A0CBE8", "#F28E2B", "#FFBE7D", "#59A14F",
  "#8CD17D", "#B6992D", "#F1CE63", "#499894", "#86BCB6",
  "#E15759", "#FF9D9A", "#79706E", "#BAB0AC", "#D37295",
  "#FABFD2", "#B07AA1", "#D4A6C8", "#9D7660", "#D7B5A6"
)

app_colors <- setNames(tableau_20[seq_along(app_cats)], app_cats)
web_colors <- c(setNames(tableau_20[seq_along(web_cats)], web_cats), "Other" = OTHER_COLOR)

# --- Precomputed y-axis ceilings (fixed scale when categories are toggled) ----
make_max_y <- function(df) {
  list(
    week = df |>
      mutate(period = floor_date(event_start, "week", week_start = 1)) |>
      group_by(period) |>
      summarise(hours = sum(active_duration) / 3600) |>
      pull(hours) |> max() * 1.2,
    month = df |>
      mutate(period = floor_date(event_start, "month")) |>
      group_by(period) |>
      summarise(hours = sum(active_duration) / 3600) |>
      pull(hours) |> max() * 1.2
  )
}

max_y_apps <- make_max_y(apps)
max_y_web  <- make_max_y(websites)

# --- Precomputed Time of Day data ---------------------------------------------
dotw_counts <- apps |>
  mutate(day  = floor_date(event_start, "day"),
         dotw = wday(event_start, label = TRUE, week_start = 1)) |>
  distinct(day, dotw) |>
  count(dotw, name = "n_days")

ridge_data <- apps |>
  mutate(
    day  = floor_date(event_start, "day"),
    dotw = wday(event_start, label = TRUE, week_start = 1)
  ) |>
  group_by(day, hour, dotw) |>
  summarise(hour_total = sum(active_duration), .groups = "drop") |>
  group_by(hour, dotw) |>
  summarise(total = sum(hour_total), .groups = "drop") |>
  left_join(dotw_counts, by = "dotw") |>
  mutate(mean_minutes = total / n_days / 60) |>
  complete(hour = 0:23, dotw, fill = list(mean_minutes = 0)) |>
  mutate(
    tooltip = paste0(
      "<b>", dotw, "  ", sprintf("%02d:00", hour), "</b><br>",
      round(mean_minutes, 1), " min avg"
    )
  )

# Time of Day KPI metrics
tod_peak_hour <- ridge_data |>
  group_by(hour) |>
  summarise(m = mean(mean_minutes, na.rm = TRUE)) |>
  slice_max(m, n = 1, with_ties = FALSE) |>
  pull(hour)

tod_busiest_day <- ridge_data |>
  group_by(dotw) |>
  summarise(t = sum(mean_minutes, na.rm = TRUE)) |>
  slice_max(t, n = 1, with_ties = FALSE) |>
  pull(dotw) |>
  as.character()

tod_wk_vs_we <- apps |>
  mutate(
    day        = floor_date(event_start, "day"),
    is_weekend = wday(event_start, week_start = 1) >= 6
  ) |>
  group_by(day, is_weekend) |>
  summarise(day_hours = sum(active_duration) / 3600, .groups = "drop") |>
  group_by(is_weekend) |>
  summarise(avg = round(mean(day_hours), 1))

# --- Helper functions ---------------------------------------------------------

# Recodes anything outside the top N categories as "Other".
# Factor levels = top cats in rank order + "Other" (biggest at bottom in ggplot).
apply_top_n <- function(df, cat_ranking, n) {
  top_cats <- cat_ranking[seq_len(n)]
  df |>
    mutate(
      category = if_else(category %in% top_cats, category, "Other"),
      category = factor(category, levels = c(top_cats, "Other"))
    )
}

# --- Theme & styling ----------------------------------------------------------
app_theme <- bs_theme(
  version    = 5,
  bootswatch = "darkly",
  base_font  = font_google("Inter"),
  primary    = "#6366f1",
  secondary  = "#ec4899"
) |>
  bs_add_rules("
    .metrics-row {
      margin-bottom: 2rem;
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(280px, 1fr));
      gap: 1.5rem;
    }
    .metric-card {
      background: linear-gradient(135deg, #1f2937 0%, #111827 100%);
      border: 1px solid #374151;
      border-radius: 0.75rem;
      padding: 1.5rem;
      display: flex;
      flex-direction: column;
      justify-content: space-between;
      transition: all 0.3s ease;
      box-shadow: 0 4px 12px rgba(0, 0, 0, 0.3);
    }
    .metric-card:hover {
      border-color: #6366f1;
      box-shadow: 0 8px 20px rgba(99, 102, 241, 0.15);
      transform: translateY(-2px);
    }
    .metric-label {
      font-size: 0.875rem;
      color: #9ca3af;
      font-weight: 500;
      margin-bottom: 0.5rem;
      text-transform: uppercase;
      letter-spacing: 0.05em;
    }
    .metric-value {
      font-size: 2.25rem;
      font-weight: 700;
      color: #f3f4f6;
      margin-bottom: 0.75rem;
      font-family: 'Courier New', monospace;
    }
    .metric-subtitle {
      font-size: 0.875rem;
      color: #6b7280;
      display: flex;
      align-items: center;
      gap: 0.5rem;
    }
    .metric-trend          { font-weight: 600; color: #10b981; }
    .metric-trend.negative { color: #ef4444; }
  ")

# Reusable ggplot theme layer
chart_theme <- function() {
  list(
    theme_minimal(base_size = 14, base_family = "Inter") +
      theme(
        plot.background    = element_rect(fill = "transparent", color = NA),
        panel.background   = element_rect(fill = "transparent", color = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor   = element_blank(),
        panel.grid.major.y = element_line(color = "#444444"),
        axis.text          = element_text(color = "#aaaaaa"),
        axis.title.y       = element_text(color = "#aaaaaa", margin = margin(r = 10)),
        legend.background  = element_rect(fill = "transparent", color = NA),
        legend.text        = element_text(color = "#fcfcfc"),
        legend.title       = element_blank()
      )
  )
}

# Shared plotly layout args (dark theme)
plotly_dark_layout <- list(
  paper_bgcolor = "rgba(0,0,0,0)",
  plot_bgcolor  = "rgba(0,0,0,0)",
  font          = list(family = "Inter", color = "#aaaaaa"),
  legend        = list(font = list(color = "#fcfcfc")),
  hoverlabel    = list(
    bgcolor     = "#1f2937",
    bordercolor = "#6366f1",
    font        = list(family = "Inter", size = 14, color = "#f3f4f6")
  )
)

# Applies plotly_dark_layout to a plotly object — avoids repeating layout() args
apply_dark_layout <- function(plt) {
  do.call(layout, c(list(p = plt), plotly_dark_layout))
}

# JS for per-segment bar highlight on hover (shared by apps + websites charts)
bar_highlight_js <- "
  function(el, x) {
    el.on('plotly_hover', function(d) {
      var hoveredCurve = d.points[0].curveNumber;
      var hoveredPoint = d.points[0].pointNumber;
      var n = el.data.length;
      var opacityArrays = [];
      for (var i = 0; i < n; i++) {
        var pts = el.data[i].x.length;
        var arr = [];
        for (var j = 0; j < pts; j++) {
          arr.push((i === hoveredCurve && j === hoveredPoint) ? 0.7 : 1);
        }
        opacityArrays.push(arr);
      }
      Plotly.restyle(el, {'marker.opacity': opacityArrays});
    });
    el.on('plotly_unhover', function() {
      var n = el.data.length;
      var resets = [];
      for (var i = 0; i < n; i++) {
        resets.push(Array(el.data[i].x.length).fill(1));
      }
      Plotly.restyle(el, {'marker.opacity': resets});
    });
  }
"
