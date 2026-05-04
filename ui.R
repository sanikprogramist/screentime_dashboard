# ui.R

ui <- page_navbar(
  title = "Screen Time Dashboard",
  theme = app_theme,

  # --- Apps tab ---------------------------------------------------------------
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
      div(
        class = "metrics-row",
        div(
          class = "metric-card",
          div(class = "metric-label", "📱 Weekly Average Screen Time"),
          div(class = "metric-value", uiOutput("metric_weekly_hours")),
          div(class = "metric-subtitle", span("Since February 2026"))
        ),
        div(
          class = "metric-card",
          div(class = "metric-label", "📊 Last 4 Weeks Average"),
          div(class = "metric-value", uiOutput("metric_current_period")),
          div(class = "metric-subtitle", uiOutput("metric_trend_arrow"))
        ),
        div(
          class = "metric-card",
          div(class = "metric-label", "✨ Last 4 Weeks Gaming"),
          div(class = "metric-value", uiOutput("metric_video_games")),
          div(class = "metric-subtitle", uiOutput("metric_video_game_trend_arrow"))
        )
      ),
      card(
        full_screen = TRUE,
        card_header("App Usage Over Time"),
        plotlyOutput("chart_apps", height = "100%")
      )
    )
  ),

  # --- Time of Day tab --------------------------------------------------------
  nav_panel(
    title = "Time of Day",
    div(
      style = "padding: 1.5rem;",
      div(
        class = "metrics-row",
        div(
          class = "metric-card",
          div(class = "metric-label", "⏰ Peak Usage Hour"),
          div(class = "metric-value", sprintf("%02d:00", tod_peak_hour)),
          div(class = "metric-subtitle", span("Most active hour across all days"))
        ),
        div(
          class = "metric-card",
          div(class = "metric-label", "📅 Busiest Day"),
          div(class = "metric-value", style = "font-size: 1.6rem;", tod_busiest_day),
          div(class = "metric-subtitle", span("Highest average daily screen time"))
        ),
        div(
          class = "metric-card",
          div(class = "metric-label", "⚖️ Weekday vs Weekend"),
          div(
            class = "metric-value", style = "font-size: 1.6rem;",
            paste0(
              tod_wk_vs_we$avg[!tod_wk_vs_we$is_weekend], "h",
              " · ",
              tod_wk_vs_we$avg[tod_wk_vs_we$is_weekend], "h"
            )
          ),
          div(class = "metric-subtitle", span("Avg daily usage · weekdays vs weekends"))
        )
      ),
      card(
        full_screen = TRUE,
        card_header("App Usage by Hour of Day"),
        plotlyOutput("chart_tod", height = "500px")
      ),
      div(
        style = "margin-top: 1rem; padding: 0.75rem 1rem;
                 background: #1f2937; border-radius: 0.5rem;
                 border-left: 3px solid #6366f1; color: #9ca3af; font-size: 0.85rem;",
        HTML("🎲 <b style='color:#f3f4f6'>Monday ~21:00</b> peak: Boardgames session &nbsp;&nbsp;
              📞 <b style='color:#f3f4f6'>Wednesday ~21:00</b> peak: Family call &nbsp;&nbsp;
              🎲 <b style='color:#f3f4f6'>Tuesday ~21:00</b> peak: D&D session")
      )
    )
  ),

  # --- Websites tab -----------------------------------------------------------
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
