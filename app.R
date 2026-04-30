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
      pull(hours) |> max(),
    month = df |>
      mutate(period = floor_date(event_start, "month")) |>
      group_by(period) |>
      summarise(hours = sum(active_duration) / 3600) |>
      pull(hours) |> max()
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
  base_font  = font_google("Inter")
)

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
        width = 230,
        radioButtons("period_apps", "Group by",
          choices = c("Week" = "week", "Month" = "month"), selected = "week"),
        hr(),
        checkboxGroupInput("cats_apps", "Categories",
          choices = app_cats, selected = app_cats)
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
}

shinyApp(ui, server)
