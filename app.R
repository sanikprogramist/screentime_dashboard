library(shiny)
library(bslib)
library(tidyverse)
library(lubridate)
library(thematic)
library(plotly)

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
      category = factor(category, levels = c(top_cats, "Other"))
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
          div(class = "metric-label", "✨Last 4 Weeks Video Game time"),
          div(class = "metric-value", uiOutput("metric_video_games")),
          div(class = "metric-subtitle",
              uiOutput("metric_video_game_trend_arrow"))
        )
      ),
      
      card(
        full_screen = TRUE,
        card_header("App Usage Over Time"),
        plotlyOutput("chart_apps", height = "100%")
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

      div(
        class = "metrics-row",
        div(
          class = "metric-card",
          div(class = "metric-label", "🏆 Top Domain"),
          div(class = "metric-value", style = "font-size: 1.2rem; word-break: break-all;",
              uiOutput("metric_web_top_domain")),
          div(class = "metric-subtitle", span("Most visited site overall"))
        ),
        div(
          class = "metric-card",
          div(class = "metric-label", "🤖 AI Usage"),
          div(class = "metric-value", uiOutput("metric_web_ai")),
          div(class = "metric-subtitle", span("Weekly avg · claude.ai + chatgpt.com"))
        ),
        div(
          class = "metric-card",
          div(class = "metric-label", "🌐 Sites Explored"),
          div(class = "metric-value", uiOutput("metric_web_domains")),
          div(class = "metric-subtitle", span("Unique domains visited"))
        )
      ),

      card(
        full_screen = TRUE,
        card_header("Website Usage Over Time"),
        plotlyOutput("chart_web", height = "100%")
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

  # Apps chart — includes top 3 apps per (period, category) for tooltip
  apps_data <- reactive({
    # app_cats is sorted desc by total hours; preserve that order for selected cats
    cat_order <- app_cats[app_cats %in% input$cats_apps]

    filtered <- apps |>
      filter(category %in% input$cats_apps) |>
      mutate(
        period   = floor_date(event_start, input$period_apps, week_start = 1),
        category = factor(category, levels = cat_order)
      )

    # Pre-compute top 3 apps per segment for tooltip
    top3 <- filtered |>
      group_by(period, category, app) |>
      summarise(app_hours = sum(active_duration) / 3600, .groups = "drop") |>
      group_by(period, category) |>
      slice_max(app_hours, n = 3, with_ties = FALSE) |>
      mutate(app_label = paste0(app, " (", round(app_hours, 1), "h)")) |>
      summarise(top_apps = paste(app_label, collapse = "<br>"), .groups = "drop")

    filtered |>
      group_by(period, category) |>
      summarise(hours = sum(active_duration) / 3600, .groups = "drop") |>
      left_join(top3, by = c("period", "category")) |>
      mutate(
        tooltip = paste0(
          "<b>", category, "</b>  ", round(hours, 1), "h<br>",
          "<span style='color:#9ca3af'>", top_apps, "</span>"
        )
      )
  })

  output$chart_apps <- renderPlotly({
    date_fmt <- if (input$period_apps == "week") "%b %d" else "%b %Y"

    p <- ggplot(apps_data(), aes(x = period, y = hours, fill = category, text = tooltip)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = app_colors) +
      scale_y_continuous(limits = c(0, max_y_apps[[input$period_apps]]), expand = c(0, 0)) +
      scale_x_datetime(date_labels = date_fmt) +
      chart_theme() +
      labs(x = NULL, y = "Hours")

    ggplotly(p, tooltip = "text") |>
      layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        font          = list(family = "Inter", color = "#aaaaaa"),
        legend        = list(font = list(color = "#fcfcfc")),
        hoverlabel    = list(
          bgcolor   = "#1f2937",
          bordercolor = "#6366f1",
          font      = list(family = "Inter", size = 16, color = "#f3f4f6")
        )
      ) %>%
      htmlwidgets::onRender("
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
      ") |>
      config(displayModeBar = FALSE)
    
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
      HTML(paste0('<span class="metric-trend">↓ ', round(abs(pct_change), 0), '% compared to first 4 weeks</span>'))
    } else {
      HTML(paste0('<span class="metric-trend negative">↑ ', round(pct_change, 0), '% compared to first 4 weeks</span>'))
    }
  })
  
  # Video game trend
  output$metric_video_games <- renderUI({
    metric_df <- apps %>%
      mutate(week = floor_date(event_start, unit = "week")) %>%
      group_by(week, category) %>%
      summarise(total = sum(active_duration) / 3600, .groups = "drop") %>%
      complete(week, category, fill = list(total = 0)) %>% 
      filter(category == "Gaming")
    
    current_week_time = metric_df %>% 
      slice_tail(n=4) %>% 
      pull(total) %>% 
      mean() # final 4 weeks average
    
    HTML(paste0(round(current_week_time, 1), "h"))
  })
  
  output$metric_video_game_trend_arrow <- renderUI({
    metric_df <- apps %>%
      mutate(week = floor_date(event_start, unit = "week")) %>%
      group_by(week, category) %>%
      summarise(total = sum(active_duration) / 3600, .groups = "drop") %>%
      complete(week, category, fill = list(total = 0)) %>% 
      filter(category == "Gaming")
    
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
      HTML(paste0('<span class="metric-trend">↓ ', round(abs(pct_change), 0), '% compared to first 4 weeks</span>'))
    } else {
      HTML(paste0('<span class="metric-trend negative">↑ ', round(pct_change, 0), '% compared to first 4 weeks</span>'))
    }
  })

  # Websites chart — tooltips handle "Other" by computing top domains per display category
  web_data <- reactive({
    top_cats <- web_cats[seq_len(input$top_n_web)]

    enriched <- websites |>
      mutate(
        period      = floor_date(event_start, input$period_web, week_start = 1),
        display_cat = if_else(category %in% top_cats, category, "Other")
      )

    # Top 3 domains per (period, display_cat) — correctly groups "Other" domains together
    top3 <- enriched |>
      filter(!is.na(domain)) |>
      group_by(period, display_cat, domain) |>
      summarise(dom_hours = sum(active_duration) / 3600, .groups = "drop") |>
      group_by(period, display_cat) |>
      slice_max(dom_hours, n = 3, with_ties = FALSE) |>
      mutate(dom_label = paste0(domain, " (", round(dom_hours, 1), "h)")) |>
      summarise(top_domains = paste(dom_label, collapse = "<br>"), .groups = "drop")

    enriched |>
      mutate(
        category = factor(display_cat, levels = c(top_cats, "Other"))
      ) |>
      group_by(period, category) |>
      summarise(hours = sum(active_duration) / 3600, .groups = "drop") |>
      left_join(top3, by = c("period", "category" = "display_cat")) |>
      mutate(
        tooltip = paste0(
          "<b>", category, "</b>  ", round(hours, 1), "h<br>",
          "<span style='color:#9ca3af'>", top_domains, "</span>"
        )
      )
  })

  output$chart_web <- renderPlotly({
    date_fmt <- if (input$period_web == "week") "%b %d" else "%b %Y"

    p <- ggplot(web_data(), aes(x = period, y = hours, fill = category, text = tooltip)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = web_colors, drop = TRUE) +
      scale_y_continuous(limits = c(0, max_y_web[[input$period_web]]), expand = c(0, 0)) +
      scale_x_datetime(date_labels = date_fmt) +
      chart_theme() +
      labs(x = NULL, y = "Hours")

    ggplotly(p, tooltip = "text") |>
      layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        font          = list(family = "Inter", color = "#aaaaaa"),
        legend        = list(font = list(color = "#fcfcfc")),
        hoverlabel    = list(
          bgcolor     = "#1f2937",
          bordercolor = "#6366f1",
          font        = list(family = "Inter", size = 16, color = "#f3f4f6")
        )
      ) %>%
      htmlwidgets::onRender("
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
      ") |>
      config(displayModeBar = FALSE)
  })

  # === METRICS FOR WEBSITES TAB ===

  # Top domain by total hours
  output$metric_web_top_domain <- renderUI({
    top <- websites |>
      filter(!is.na(domain)) |>
      group_by(domain) |>
      summarise(hours = sum(active_duration) / 3600, .groups = "drop") |>
      slice_max(hours, n = 1)
    HTML(paste0(top$domain, "<br><small style='color:#9ca3af;font-size:0.75rem'>",
                round(top$hours, 0), "h total</small>"))
  })

  # AI tools weekly average
  output$metric_web_ai <- renderUI({
    ai_weekly <- websites |>
      filter(category == "AI Tools") |>
      mutate(week = floor_date(event_start, "week")) |>
      group_by(week) |>
      summarise(hrs = sum(active_duration) / 3600, .groups = "drop") |>
      pull(hrs) |> mean()
    HTML(paste0(round(ai_weekly, 1), "h/wk"))
  })

  # Unique domains visited
  output$metric_web_domains <- renderUI({
    n <- websites |> pull(domain) |> n_distinct()
    HTML(as.character(n))
  })
}

shinyApp(ui, server)
