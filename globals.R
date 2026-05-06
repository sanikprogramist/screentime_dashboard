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
MIN_HOURS   <- 1
OTHER_COLOR <- "#6c757d"

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
         dotw = lubridate::wday(event_start, label = TRUE, week_start = 1)) |>
  distinct(day, dotw) |>
  count(dotw, name = "n_days")

ridge_data <- apps |>
  mutate(
    day  = floor_date(event_start, "day"),
    dotw = lubridate::wday(event_start, label = TRUE, week_start = 1)
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
    is_weekend = lubridate::wday(event_start, week_start = 1) >= 6
  ) |>
  group_by(day, is_weekend) |>
  summarise(day_hours = sum(active_duration) / 3600, .groups = "drop") |>
  group_by(is_weekend) |>
  summarise(avg = round(mean(day_hours), 1))

# --- Business Hours Productivity precomputed data ----------------------------
productive_app_cats <- c("Learning & Education", "Development & Programming",
                         "Productivity & Work", "Art & Creative")
productive_web_cats <- c("Communication", "Reference & Research", "Productivity & Work",
                         "Development & Programming", "Learning & Education", "Job Search",
                         "AI Tools", "Science & Academia", "Government & Administration")

bh_apps <- apps |>
  mutate(productive = category %in% productive_app_cats) |>
  filter(category != "Browser")

bh_websites <- websites |>
  mutate(productive = category %in% productive_web_cats)

business_hours <- bind_rows(
  select(bh_apps,     event_start, active_duration, category, productive),
  select(bh_websites, event_start, active_duration, category, productive)
) |>
  mutate(weekday_num = lubridate::wday(event_start, week_start = 1)) |>
  filter(hour(event_start) > 6, hour(event_start) < 17,
         weekday_num >= 1, weekday_num <= 5)

weekly_business <- business_hours |>
  group_by(week = floor_date(event_start, "week", week_start = 1), productive) |>
  summarise(total_time = sum(active_duration) / 3600, .groups = "drop") |>
  pivot_wider(names_from = productive, values_from = total_time, values_fill = 0) |>
  rename(unproductive = `FALSE`, productive = `TRUE`) |>
  mutate(
    total             = productive + unproductive,
    productivity_ratio = round(productive / total * 100, 1)
  )

category_business <- business_hours |>
  group_by(category, productive) |>
  summarise(hours = sum(active_duration) / 3600, sessions = n(), .groups = "drop") |>
  mutate(category = if_else(is.na(category), "Misc", category))

top_drains_business <- category_business |>
  filter(!productive) |>
  arrange(desc(hours)) |>
  head(8)

productive_business <- category_business |>
  filter(productive) |>
  arrange(desc(hours)) |>
  head(8)

prod_rate       <- round(sum(weekly_business$productive) /
                           sum(weekly_business$total) * 100, 1)
prod_hrs_weekly <- round(mean(weekly_business$productive), 1)
top_drain_cat   <- top_drains_business$category[[1]]
top_drain_hrs   <- round(top_drains_business$hours[[1]], 1)

# --- Intro tab precomputed stats ----------------------------------------------
intro_n_weeks <- apps |>
  mutate(w = floor_date(event_start, "week", week_start = 1)) |>
  pull(w) |> n_distinct()

intro_total_hours <- round(sum(apps$active_duration) / 3600, 0)

intro_date_from <- format(min(apps$event_start), "%b %Y")
intro_date_to   <- format(max(apps$event_start), "%b %Y")

intro_gaming_drop <- local({
  gw <- apps |>
    mutate(week = floor_date(event_start, "week", week_start = 1)) |>
    group_by(week, category) |>
    summarise(hours = sum(active_duration) / 3600, .groups = "drop") |>
    complete(week, category, fill = list(hours = 0)) |>
    filter(category == "Gaming") |>
    arrange(week)
  pct <- (mean(tail(gw$hours, 4)) - mean(head(gw$hours, 4))) / mean(head(gw$hours, 4)) * 100
  round(abs(pct), 0)
})

intro_ai_weekly <- websites |>
  filter(category == "AI Tools") |>
  mutate(week = floor_date(event_start, "week", week_start = 1)) |>
  group_by(week) |>
  summarise(hrs = sum(active_duration) / 3600, .groups = "drop") |>
  pull(hrs) |> mean() |> round(1)

intro_top_domain <- websites |>
  filter(!is.na(domain)) |>
  group_by(domain) |>
  summarise(hours = sum(active_duration) / 3600, .groups = "drop") |>
  slice_max(hours, n = 1) |>
  pull(domain)

intro_email_weekly <- websites |>
  filter(category == "Communication") |>
  mutate(week = floor_date(event_start, "week", week_start = 1)) |>
  group_by(week) |>
  summarise(hrs = sum(active_duration) / 3600, .groups = "drop") |>
  pull(hrs) |> mean() |> round(1)



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

    /* --- About tab --------------------------------------------------------- */
    .intro-hero {
      background: linear-gradient(135deg, #1a1f35 0%, #0f1117 100%);
      border: 1px solid #374151;
      border-left: 4px solid #6366f1;
      border-radius: 0.75rem;
      padding: 2.5rem;
      margin-bottom: 2rem;
    }
    .intro-hero h1 {
      color: #f3f4f6;
      font-size: 2rem;
      font-weight: 700;
      margin: 0 0 0.5rem;
    }
    .intro-hero .hero-sub {
      color: #9ca3af;
      font-size: 1rem;
      line-height: 1.6;
      margin: 0 0 1.25rem;
      max-width: 680px;
    }
    .hero-pill {
      display: inline-block;
      background: #1f2937;
      border: 1px solid #374151;
      color: #9ca3af;
      font-size: 0.78rem;
      padding: 0.25rem 0.8rem;
      border-radius: 9999px;
      font-family: 'Courier New', monospace;
      margin-right: 0.5rem;
    }
    .insight-value {
      font-size: 2.4rem;
      font-weight: 700;
      font-family: 'Courier New', monospace;
      color: #6366f1;
      line-height: 1;
      margin-bottom: 0.4rem;
    }
    .insight-value.green { color: #10b981; }
    .insight-label {
      font-size: 0.8rem;
      color: #9ca3af;
      text-transform: uppercase;
      letter-spacing: 0.06em;
      font-weight: 500;
      margin-bottom: 0.35rem;
    }
    .insight-note {
      font-size: 0.82rem;
      color: #6b7280;
      line-height: 1.5;
    }
    .tech-badge {
      display: inline-block;
      background: #1f2937;
      border: 1px solid #374151;
      color: #d1d5db;
      font-size: 0.8rem;
      padding: 0.3rem 0.75rem;
      border-radius: 0.4rem;
      margin: 0.2rem 0.2rem 0.2rem 0;
      font-weight: 500;
    }
    .tech-badge.primary {
      border-color: #6366f1;
      color: #a5b4fc;
      background: #1e1f3a;
    }
    .about-body {
      color: #d1d5db;
      font-size: 0.9rem;
      line-height: 1.75;
    }
    .about-body strong { color: #f3f4f6; }

    /* --- Navbar / tab styling ------------------------------------------------ */
    .navbar .nav-link {
      font-size: 1.1rem;
      font-weight: 600;
      color: #9ca3af !important;
      padding: 0.75rem 1.5rem !important;
      text-transform: uppercase;
      letter-spacing: 0.05em;
      transition: all 0.25s ease;
    }
    .navbar .nav-link:hover {
      color: #f3f4f6 !important;
    }
    .navbar .nav-link.active {
      color: #6366f1 !important;
      border-bottom: 3px solid #6366f1;
      background: transparent;
    }
    .navbar-brand {
      font-size: 1.3rem;
      font-weight: 700;
      color: #f3f4f6 !important;
      letter-spacing: 0.05em;
      margin-right: 2rem;
    }
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
        legend.text        = element_blank(),
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
