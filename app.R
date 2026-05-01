library(shiny)
library(bslib)
library(tidyverse)
library(lubridate)
library(thematic)

thematic_shiny()

# --- Load data ----------------------------------------------------------------
apps     <- readRDS("data/apps_clean.rds")
websites <- readRDS("data/websites_clean.rds")

# Drop categories with less than 30 minutes of total usage
MIN_HOURS <- 1 # keep the min hours as 1

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

# Tableau 20 palette — extended version for when we need more than 10 colors
tableau_20 <- c(
  "#4E79A7", "#A0CBE8", "#F28E2B", "#FFBE7D", "#59A14F",
  "#8CD17D", "#B6992D", "#F1CE63", "#499894", "#86BCB6",
  "#E15759", "#FF9D9A", "#79706E", "#BAB0AC", "#D37295",
  "#FABFD2", "#B07AA1", "#D4A6C8", "#9D7660", "#D7B5A6"
)
OTHER_COLOR <- "#6c757d"


app_colors <- setNames(tableau_20[seq_along(app_cats)], app_cats)
web_colors  <- c(setNames(tableau_20[seq_along(web_cats)], web_cats), "Other" = OTHER_COLOR)

# Precompute y-axis ceilings so scale stays fixed when categories are toggled
make_max_y <- function(df) {
  list(
    week = df |>
      mutate(period = floor_date(event_start, "week", week_start = 1)) |>
      group_by(period) |>
      summarise(hours = sum(active_duration) / 3600) |>
      pull(hours) |> max() *1.2,
    month = df |>
      mutate(period = floor_date(event_start, "month")) |>
      group_by(period) |>
      summarise(hours = sum(active_duration) / 3600) |>
      pull(hours) |> max() *1.2
  )
}

max_y_apps <- make_max_y(apps)
max_y_web  <- make_max_y(websites)

# --- Shared theme & plot function ---------------------------------------------

# --- Helper: apply top-N and set factor order ---------------------------------
# Recodes anything outside the top N as "Other".
# Factor levels = top cats in rank order + "Other", so ggplot stacks:
#   rank 1 (biggest) at the bottom, "Other" at the top.
apply_top_n <- function(df, cat_ranking, n) {
  top_cats <- cat_ranking[seq_len(n)]
  df |>
    mutate(
      category = if_else(category %in% top_cats, category, "Other"),
      category = factor(category, levels = rev(c(top_cats, "Other")))
    )
}

app_theme <- bs_theme(
  version    = 5,
  bootswatch = "darkly",
  base_font  = font_google("Inter"),
  primary    = "#6366f1",
  secondary  = "#ec4899"
) |>
  bs_add_rules("
    /* Metrics row styling */
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
    
    .metric-trend {
      font-weight: 600;
      color: #10b981;
    }
    
    .metric-trend.negative {
      color: #ef4444;
    }
    
    .metric-icon {
      font-size: 1.5rem;
      opacity: 0.5;
    }
  ")

# Reusable ggplot theme layer — keeps both charts consistent
chart_theme <- function() {
  list(
    theme_minimal(base_size = 14,  base_family = "Inter") +
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

# --- UI -----------------------------------------------------------------------
ui <- page_navbar(
  title  = "Screen Time Dashboard",
  theme  = app_theme,

  # --- Apps tab ---
  nav_panel(
    title = "Apps",
    layout_sidebar(
      sidebar = sidebar(
        width = 200,
        radioButtons("period_apps", "Group by",
          choices = c("Week" = "week", "Month" = "month"), selected = "week"),
        hr(),
        checkboxGroupInput("cats_apps", "Categories",
          choices = app_cats, selected = app_cats)
      ),
      
      # METRICS ROW - Add your reactive metric outputs here
      div(
        class = "metrics-row",
        # Metric 1: Weekly Screen Time
        div(
          class = "metric-card",
          div(class = "metric-label", "📱 Weekly Average Screen Time"),
          div(class = "metric-value", uiOutput("metric_weekly_hours")),
          div(class = "metric-subtitle", 
              span("Since February 2026"))
        ),
        # Metric 2: Weekly Average
        div(
          class = "metric-card",
          div(class = "metric-label", "📊 Last 4 weeks average"),
          div(class = "metric-value", uiOutput("metric_current_period")),
          div(class = "metric-subtitle",
              uiOutput("metric_trend_arrow"))
        ),
        # Metric 3: Productivity Split
        div(
          class = "metric-card",
          div(class = "metric-label", "✨ Productivity Score"),
          div(class = "metric-value", uiOutput("metric_productivity")),
          div(class = "metric-subtitle",
              span("Productive vs. distracting"))
        )
      ),
      
      card(
        full_screen = TRUE,
        card_header("App Usage Over Time"),
        plotOutput("chart_apps", height = "100%")
      )
    )
  ),

  # --- Websites tab ---
  nav_panel(
    title = "Websites",
    layout_sidebar(
      sidebar = sidebar(
        width = 230,
        radioButtons("period_web", "Group by",
                     choices = c("Week" = "week", "Month" = "month"), selected = "week"),
        hr(),
        sliderInput("top_n_web", "Top categories",
                    min = 1, max = length(web_cats), value = 8, step = 1)
      ),
      
      card(
        full_screen = TRUE,
        card_header("Website Usage Over Time"),
        plotOutput("chart_web", height = "100%")
      )
    )
  )
)

# --- Category Classifications (for productivity metrics) ----------------------
# Define which categories count as "productive" vs "distracting"
PRODUCTIVE_APPS <- c("Browser", "Development", "Communication", "Productivity", "OS & System")
PRODUCTIVE_WEB <- c("Learning", "Documentation", "Work", "Development", "Utilities")

# --- Server -------------------------------------------------------------------
server <- function(input, output, session) {

  # Apps chart
  apps_data <- reactive({
    apps |>
      filter(category %in% input$cats_apps) |>
      mutate(period = floor_date(event_start, input$period_apps, week_start = 1)) |>
      group_by(period, category) |>
      summarise(hours = sum(active_duration) / 3600, .groups = "drop")
  })

  output$chart_apps <- renderPlot(bg = "transparent",{
    date_fmt <- if (input$period_apps == "week") "%b %d" else "%b '%y"
    ggplot(apps_data(), aes(x = period, y = hours, fill = category)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = app_colors) +
      scale_y_continuous(limits = c(0, max_y_apps[[input$period_apps]]), expand = c(0, 0)) +
      scale_x_datetime(date_labels = date_fmt) +
      chart_theme() +
      labs(x = NULL, y = "Hours")
  })

  # === METRICS FOR APPS TAB ===
  
  # Total screen time (apps + websites combined)
  output$metric_weekly_hours <- renderUI({
    metric_df = apps %>% 
      mutate(week = floor_date(event_start, unit="week"))
    total <- (sum(apps$active_duration) / (3600*length(unique(metric_df[["week"]]))))
    HTML(paste0(round(total, 0), "h"))
  })
  
  # Current average for last 4 weeks
  output$metric_current_period <- renderUI({
    metric_df = apps %>% 
      mutate(week = floor_date(event_start, unit="week"))
    metric_df = metric_df %>% 
      group_by(week) %>% 
      summarise(total=sum(active_duration)/3600)# %>% sort(week, decreasing=FALSE)
    current_week_time = metric_df %>% 
      slice_tail(n=4) %>% 
      pull(total) %>% 
      mean() # final 4 weeks average
    HTML(paste0(round(current_week_time, 1), "h"))
  })
  
  # Trend indicator (compare first four weeks vs last 4 weeks)
  output$metric_trend_arrow <- renderUI({
    metric_df = apps %>% 
      mutate(week = floor_date(event_start, unit="week"))
    metric_df = metric_df %>% 
      group_by(week) %>% 
      summarise(total=sum(active_duration)/3600)# %>% sort(week, decreasing=FALSE)
    
    last_four_weeks = metric_df %>% 
      slice_tail(n=4) %>% 
      pull(total) %>% 
      mean() # final 4 weeks average
    
    first_four_weeks = metric_df %>% 
      slice_head(n=4) %>% 
      pull(total) %>% 
      mean() # final 4 weeks average
    
    pct_change <- ((last_four_weeks - first_four_weeks) / first_four_weeks) * 100
    
    if (pct_change < 0) {
      HTML(paste0('<span class="metric-trend">↓ ', round(abs(pct_change), 0), '% from start</span>'))
    } else {
      HTML(paste0('<span class="metric-trend negative">↑ ', round(pct_change, 0), '% from start</span>'))
    }
  })
  
  # Productivity score (% of time on productive categories)
  output$metric_productivity <- renderUI({
    productive_hours <- apps |>
      filter(category %in% PRODUCTIVE_APPS) |>
      pull(active_duration) |> sum() / 3600
    
    web_productive_hours <- websites |>
      filter(category %in% PRODUCTIVE_WEB) |>
      pull(active_duration) |> sum() / 3600
    
    total_hours <- (sum(apps$active_duration) + sum(websites$active_duration)) / 3600
    productivity_pct <- (productive_hours + web_productive_hours) / total_hours * 100
    
    HTML(paste0(round(productivity_pct, 0), "%"))
  })

  # Websites chart
  web_data <- reactive({
    apply_top_n(websites, web_cats, input$top_n_web) |>
      mutate(period = floor_date(event_start, input$period_web, week_start = 1)) |>
      group_by(period, category) |>
      summarise(hours = sum(active_duration) / 3600, .groups = "drop")
  })
  
  output$chart_web <- renderPlot(bg = "transparent", {
    date_fmt <- if (input$period_web == "week") "%b %d" else "%b '%y"
    ggplot(web_data(), aes(x = period, y = hours, fill = category)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = web_colors, drop = TRUE) +
      scale_y_continuous(limits = c(0, max_y_web[[input$period_web]]), expand = c(0, 0)) +
      scale_x_datetime(date_labels = date_fmt) +
      chart_theme() +
      labs(x = NULL, y = "Hours")
  })

  # === METRICS FOR WEBSITES TAB ===
  
}

shinyApp(ui, server)
