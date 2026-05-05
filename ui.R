# ui.R
if (!exists("app_theme")) source("globals.R")

ui <- page_navbar(
  title = "Screen Time Dashboard",
  theme = app_theme,

  # --- About tab --------------------------------------------------------------
  nav_panel(
    title = "About",
    div(
      style = "padding: 1.5rem; max-width: 1100px; margin: 0 auto;",

      # Hero
      div(
        class = "intro-hero",
        tags$h1("Screen Time Dashboard"),
        p(class = "hero-sub",
          "A 13-week personal analytics project: tracking, cleaning, and visualising ",
          "my digital habits to understand where time actually goes — and whether I could change it."
        ),
        span(class = "hero-pill", paste0(intro_date_from, " – ", intro_date_to)),
        span(class = "hero-pill", paste0(intro_n_weeks, " weeks tracked")),
        span(class = "hero-pill", paste0(intro_total_hours, "h total screen time"))
      ),

      # Insight cards
      div(
        class = "metrics-row",
        div(
          class = "metric-card",
          div(class = "insight-label", "Gaming hours reduced"),
          div(class = "insight-value green", paste0("↓ ", intro_gaming_drop, "%")),
          div(class = "insight-note",
            "Weekly gaming avg fell from ~12.9h in the first four weeks to ~0.7h in the last four.")
        ),
        div(
          class = "metric-card",
          div(class = "insight-label", "Total screen time tracked"),
          div(class = "insight-value", paste0(intro_total_hours, "h")),
          div(class = "insight-note",
            paste0("Across ", intro_n_weeks, " weeks of continuous tracking via ActivityWatch."))
        ),
        div(
          class = "metric-card",
          div(class = "insight-label", "AI tools — weekly avg"),
          div(class = "insight-value", paste0(intro_ai_weekly, "h")),
          div(class = "insight-note",
            "Time on claude.ai and chatgpt.com — a consistent part of the development workflow.")
        ),
        div(
          class = "metric-card",
          div(class = "insight-label", "Top domain overall"),
          div(class = "insight-value",
              style = "font-size: 1.3rem; word-break: break-all;",
              intro_top_domain),
          div(class = "insight-note",
            "The most visited site by a wide margin, reflecting heavy use of Google Docs for work.")
        ),
        div(
          class = "metric-card",
          div(class = "insight-label", "Email — weekly avg"),
          div(class = "insight-value", paste0(intro_email_weekly, "h")),
          div(class = "insight-note",
            "Time spent on email and communication platforms each week.")
        )
      ),

      # Goal + Tech stack
      layout_columns(
        col_widths = c(6, 6),
        gap = "1.5rem",

        card(
          card_header("The Goal"),
          card_body(
            div(
              class = "about-body",
              p("Windows doesn't provide a native way to deeply analyse your own screen time. ",
                "To answer the question ",
                tags$strong("where does my time actually go?"),
                ", I needed to build it myself. I suspected heavy gaming habits and wanted real data to confirm it — ",
                "and to hold myself accountable."),
              p("The data worked. Gaming peaked at nearly ",
                tags$strong("13 hours a week"),
                " in the first month but dropped to under an hour by the end. ",
                "Seeing the numbers made the habit visible and gave me something concrete to act on."),
              p("The secondary goal was learning: this project was a practical introduction to ",
                tags$strong("R Shiny"), ", ", tags$strong("Plotly"), ", ",
                tags$strong("SQL"), " for data cleaning, and using ",
                tags$strong("agentic AI"), " to accelerate development.")
            )
          )
        ),

        card(
          card_header("How It Was Built"),
          card_body(
            div(
              class = "about-body",
              p(tags$strong("Data collection"), " — ActivityWatch runs in the background, ",
                "capturing window titles and active applications at second-level resolution."),
              p(tags$strong("Processing"), " — raw event logs were cleaned and categorised ",
                "using SQL, then imported into R as structured data frames for analysis."),
              p(tags$strong("Dashboard"), " — built with R Shiny for the interactive framework, ",
                "Plotly for rich visualisations, and responsive design for readability."),
              p(tags$strong("Development"), " — agentic AI assisted with code generation, ",
                "debugging, and workflow optimisation throughout the project."),
              div(
                style = "margin-top: 1rem;",
                span(class = "tech-badge primary", "ActivityWatch"),
                span(class = "tech-badge primary", "R Shiny"),
                span(class = "tech-badge primary", "Plotly"),
                span(class = "tech-badge primary", "SQL"),
                span(class = "tech-badge", "ggplot2"),
                span(class = "tech-badge", "tidyverse"),
                span(class = "tech-badge", "Bootstrap 5")
              )
            )
          )
        )
      )
    )
  ),

  # --- Apps tab ---------------------------------------------------------------
  nav_panel(
    title = "Apps",
    layout_sidebar(
      sidebar = sidebar(
        width = 290,
        h6("APPS", style = "color:#6366f1; letter-spacing:.1em; margin-bottom:.75rem;"),
        uiOutput("sidebar_apps_story"),
        hr(style = "border-color:#374151; margin: 1rem 0;"),
        p(style = "color:#6b7280; font-size:0.82rem; line-height:1.6; margin:0;",
          "Adjust the time period and category filters below the chart to explore different breakdowns of app usage."
        )
      ),
      div(
        style = "display: flex; flex-direction: column; gap: 1.5rem;",
        div(
          class = "metrics-row",
          style = "grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); margin-bottom: 0.5rem;",
          div(
            class = "metric-card",
            style = "padding: 1rem;",
            div(class = "metric-label", style = "font-size: 0.75rem;", "📱 Weekly Average"),
            div(class = "metric-value", style = "font-size: 1.3rem;", uiOutput("metric_weekly_hours")),
            div(class = "metric-subtitle", style = "font-size: 0.7rem;", span("Since Feb 2026"))
          ),
          div(
            class = "metric-card",
            style = "padding: 1rem;",
            div(class = "metric-label", style = "font-size: 0.75rem;", "📊 Last 4 Weeks"),
            div(class = "metric-value", style = "font-size: 1.3rem;", uiOutput("metric_current_period")),
            div(class = "metric-subtitle", style = "font-size: 0.7rem;", uiOutput("metric_trend_arrow"))
          ),
          div(
            class = "metric-card",
            style = "padding: 1rem;",
            div(class = "metric-label", style = "font-size: 0.75rem;", "✨ Gaming Trend"),
            div(class = "metric-value", style = "font-size: 1.3rem;", uiOutput("metric_video_games")),
            div(class = "metric-subtitle", style = "font-size: 0.7rem;", uiOutput("metric_video_game_trend_arrow"))
          )
        ),
        card(
          full_screen = TRUE,
          card_header("App Usage Over Time"),
          card_body(plotlyOutput("chart_apps", height = "450px")),
          card_footer(
            div(
              style = "display:flex; gap:2.5rem; flex-wrap:wrap; align-items:flex-start;",
              radioButtons("period_apps", "Group by",
                choices = c("Week" = "week", "Month" = "month"),
                selected = "week", inline = TRUE),
              checkboxGroupInput("cats_apps", "Categories",
                choices = app_cats, selected = app_cats, inline = TRUE)
            )
          )
        )
      )
    )
  ),

  # --- Time of Day tab --------------------------------------------------------
  nav_panel(
    title = "Time of Day",
    layout_sidebar(
      sidebar = sidebar(
        width = 290,
        h6("TIME OF DAY", style = "color:#6366f1; letter-spacing:.1em; margin-bottom:.75rem;"),
        p(style = "color:#d1d5db; font-size:0.9rem; line-height:1.7; margin:0 0 .75rem;",
          "Screen time isn't spread evenly through the day. Usage peaks around ",
          tags$strong(style = "color:#f3f4f6;",
            sprintf("%02d:00", tod_peak_hour)),
          " on most days, and ",
          tags$strong(style = "color:#f3f4f6;", tod_busiest_day),
          " is consistently the heaviest day of the week."
        ),
        p(style = "color:#d1d5db; font-size:0.9rem; line-height:1.7; margin:0 0 .75rem;",
          "Weekdays average ",
          tags$strong(style = "color:#f3f4f6;",
            paste0(tod_wk_vs_we$avg[!tod_wk_vs_we$is_weekend], "h")),
          " daily vs. ",
          tags$strong(style = "color:#f3f4f6;",
            paste0(tod_wk_vs_we$avg[tod_wk_vs_we$is_weekend], "h")),
          " on weekends."
        ),
        hr(style = "border-color:#374151; margin: 1rem 0;"),
        p(style = "color:#6b7280; font-size:0.82rem; line-height:1.6; margin:0;",
          "A few recurring evening sessions create distinctive spikes: ",
          "board games on Monday, a family call on Wednesday, D&D on Tuesday — ",
          "all landing around 21:00."
        )
      ),
      div(
        class = "metrics-row",
        style = "grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); margin-bottom: 0.5rem;",
        div(
          class = "metric-card",
          style = "padding: 1rem;",
          div(class = "metric-label", style = "font-size: 0.75rem;", "⏰ Peak Usage Hour"),
          div(class = "metric-value", style = "font-size: 1.3rem;", sprintf("%02d:00", tod_peak_hour)),
          div(class = "metric-subtitle", style = "font-size: 0.7rem;", span("Most active hour across all days"))
        ),
        div(
          class = "metric-card",
          style = "padding: 1rem;",
          div(class = "metric-label", style = "font-size: 0.75rem;", "📅 Busiest Day"),
          div(class = "metric-value", style = "font-size: 1.3rem;", tod_busiest_day),
          div(class = "metric-subtitle", style = "font-size: 0.7rem;", span("Highest average daily screen time"))
        ),
        div(
          class = "metric-card",
          style = "padding: 1rem;",
          div(class = "metric-label", style = "font-size: 0.75rem;", "⚖️ Weekday vs Weekend"),
          div(
            class = "metric-value", style = "font-size: 1.3rem;",
            paste0(
              tod_wk_vs_we$avg[!tod_wk_vs_we$is_weekend], "h",
              " · ",
              tod_wk_vs_we$avg[tod_wk_vs_we$is_weekend], "h"
            )
          ),
          div(class = "metric-subtitle", style = "font-size: 0.7rem;", span("Weekdays vs weekends"))
        )
      ),
      card(
        full_screen = TRUE,
        card_header("App Usage by Hour of Day"),
        plotlyOutput("chart_tod", height = "450px")
      )
    )
  ),

  # --- Websites tab -----------------------------------------------------------
  nav_panel(
    title = "Websites",
    layout_sidebar(
      sidebar = sidebar(
        width = 290,
        h6("WEBSITES", style = "color:#6366f1; letter-spacing:.1em; margin-bottom:.75rem;"),
        uiOutput("sidebar_web_story"),
        hr(style = "border-color:#374151; margin: 1rem 0;"),
        p(style = "color:#6b7280; font-size:0.82rem; line-height:1.6; margin:0;",
          "Switch between weekly and monthly views, or adjust how many categories are shown in the breakdown below."
        )
      ),
      div(
        style = "display: flex; flex-direction: column; gap: 1.5rem;",
        div(
          class = "metrics-row",
          style = "grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); margin-bottom: 0.5rem;",
          div(
            class = "metric-card",
            style = "padding: 1rem;",
            div(class = "metric-label", style = "font-size: 0.75rem;", "🏆 Top Domain"),
            uiOutput("metric_web_top_domain")
          ),
          div(
            class = "metric-card",
            style = "padding: 1rem;",
            div(class = "metric-label", style = "font-size: 0.75rem;", "🤖 AI Usage"),
            div(class = "metric-value", style = "font-size: 1.3rem;", uiOutput("metric_web_ai")),
            div(class = "metric-subtitle", style = "font-size: 0.7rem;", span("Weekly avg"))
          ),
          div(
            class = "metric-card",
            style = "padding: 1rem;",
            div(class = "metric-label", style = "font-size: 0.75rem;", "🌐 Sites Explored"),
            div(class = "metric-value", style = "font-size: 1.3rem;", uiOutput("metric_web_domains")),
            div(class = "metric-subtitle", style = "font-size: 0.7rem;", span("Unique domains"))
          )
        ),
        card(
          full_screen = TRUE,
          card_header("Website Usage Over Time"),
          card_body(plotlyOutput("chart_web", height = "450px")),
          card_footer(
            div(
              style = "display:flex; gap:2.5rem; flex-wrap:wrap; align-items:flex-start;",
              radioButtons("period_web", "Group by",
                choices = c("Week" = "week", "Month" = "month"),
                selected = "week", inline = TRUE),
              sliderInput("top_n_web", "Top categories",
                min = 1, max = length(web_cats), value = 8, step = 1,
                width = "220px")
            )
          )
        )
      )
    )
  )
)
