# server.R

server <- function(input, output, session) {

  # ===========================================================================
  # APPS TAB
  # ===========================================================================

  # Reactive: aggregate app usage by period + category, with top-3 tooltip
  apps_data <- reactive({
    cat_order <- app_cats[app_cats %in% input$cats_apps]

    filtered <- apps |>
      filter(category %in% input$cats_apps) |>
      mutate(
        period   = floor_date(event_start, input$period_apps, week_start = 1),
        category = factor(category, levels = cat_order)
      )

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
      apply_dark_layout() %>%
      htmlwidgets::onRender(bar_highlight_js) |>
      config(displayModeBar = FALSE)
  })

  # --- Apps metrics ---

  output$metric_weekly_hours <- renderUI({
    n_weeks <- apps |>
      mutate(week = floor_date(event_start, "week")) |>
      pull(week) |> n_distinct()
    avg <- sum(apps$active_duration) / 3600 / n_weeks
    HTML(paste0(round(avg, 0), "h"))
  })

  output$metric_current_period <- renderUI({
    avg_last4 <- apps |>
      mutate(week = floor_date(event_start, "week")) |>
      group_by(week) |>
      summarise(total = sum(active_duration) / 3600, .groups = "drop") |>
      slice_tail(n = 4) |>
      pull(total) |> mean()
    HTML(paste0(round(avg_last4, 1), "h"))
  })

  output$metric_trend_arrow <- renderUI({
    weekly <- apps |>
      mutate(week = floor_date(event_start, "week")) |>
      group_by(week) |>
      summarise(total = sum(active_duration) / 3600, .groups = "drop")
    first4 <- mean(slice_head(weekly, n = 4)$total)
    last4  <- mean(slice_tail(weekly, n = 4)$total)
    pct    <- (last4 - first4) / first4 * 100
    if (pct < 0) {
      HTML(paste0('<span class="metric-trend">↓ ', round(abs(pct), 0), '% vs first 4 weeks</span>'))
    } else {
      HTML(paste0('<span class="metric-trend negative">↑ ', round(pct, 0), '% vs first 4 weeks</span>'))
    }
  })

  output$metric_video_games <- renderUI({
    avg_last4 <- apps |>
      mutate(week = floor_date(event_start, "week")) |>
      group_by(week, category) |>
      summarise(total = sum(active_duration) / 3600, .groups = "drop") |>
      complete(week, category, fill = list(total = 0)) |>
      filter(category == "Gaming") |>
      slice_tail(n = 4) |>
      pull(total) |> mean()
    HTML(paste0(round(avg_last4, 1), "h"))
  })

  output$metric_video_game_trend_arrow <- renderUI({
    gaming <- apps |>
      mutate(week = floor_date(event_start, "week")) |>
      group_by(week, category) |>
      summarise(total = sum(active_duration) / 3600, .groups = "drop") |>
      complete(week, category, fill = list(total = 0)) |>
      filter(category == "Gaming")
    first4 <- mean(slice_head(gaming, n = 4)$total)
    last4  <- mean(slice_tail(gaming, n = 4)$total)
    pct    <- (last4 - first4) / first4 * 100
    if (pct < 0) {
      HTML(paste0('<span class="metric-trend">↓ ', round(abs(pct), 0), '% vs first 4 weeks</span>'))
    } else {
      HTML(paste0('<span class="metric-trend negative">↑ ', round(pct, 0), '% vs first 4 weeks</span>'))
    }
  })

  # ===========================================================================
  # TIME OF DAY TAB
  # ===========================================================================

  output$chart_tod <- renderPlotly({
    days   <- levels(ridge_data$dotw)
    n      <- length(days)
    colors <- scales::viridis_pal(option = "magma")(n)
    scale  <- 1.8 / max(ridge_data$mean_minutes, na.rm = TRUE)

    plt <- plot_ly()

    for (i in seq_along(days)) {
      d      <- ridge_data |> filter(dotw == days[i]) |> arrange(hour)
      y_base <- n + 1 - i
      y_top  <- y_base + d$mean_minutes * scale

      plt <- add_trace(plt,
        x = d$hour, y = rep(y_base, nrow(d)),
        type = "scatter", mode = "lines",
        line = list(color = "transparent", width = 0),
        showlegend = FALSE, hoverinfo = "none"
      )
      plt <- add_trace(plt,
        x = d$hour, y = y_top,
        type = "scatter", mode = "lines",
        fill = "tonexty",
        fillcolor = paste0(colors[i], "CC"),
        line = list(color = colors[i], width = 1),
        name = days[i],
        text = d$tooltip,
        hoverinfo = "text",
        showlegend = TRUE
      )
    }

    plt |>
      layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        font  = list(family = "Inter", color = "#aaaaaa"),
        xaxis = list(
          tickvals = seq(0, 21, 3),
          ticktext = sprintf("%02d:00", seq(0, 21, 3)),
          range    = c(-1.5, 23),
          showgrid = FALSE, zeroline = FALSE, color = "#aaaaaa"
        ),
        yaxis = list(
          tickvals = rev(seq_along(days)), ticktext = days,
          showgrid = FALSE, zeroline = FALSE, color = "#aaaaaa"
        ),
        legend     = list(font = list(color = "#fcfcfc")),
        hoverlabel = list(
          bgcolor     = "#1f2937",
          bordercolor = "#6366f1",
          font = list(family = "Inter", size = 13, color = "#f3f4f6")
        )
      ) |>
      config(displayModeBar = FALSE)
  })

  # ===========================================================================
  # WEBSITES TAB
  # ===========================================================================

  # Reactive: aggregate web usage by period + display category, with top-3 domains
  web_data <- reactive({
    top_cats <- web_cats[seq_len(input$top_n_web)]

    enriched <- websites |>
      mutate(
        period      = floor_date(event_start, input$period_web, week_start = 1),
        display_cat = if_else(category %in% top_cats, category, "Other")
      )

    top3 <- enriched |>
      filter(!is.na(domain)) |>
      group_by(period, display_cat, domain) |>
      summarise(dom_hours = sum(active_duration) / 3600, .groups = "drop") |>
      group_by(period, display_cat) |>
      slice_max(dom_hours, n = 3, with_ties = FALSE) |>
      mutate(dom_label = paste0(domain, " (", round(dom_hours, 1), "h)")) |>
      summarise(top_domains = paste(dom_label, collapse = "<br>"), .groups = "drop")

    enriched |>
      mutate(category = factor(display_cat, levels = c(top_cats, "Other"))) |>
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
      apply_dark_layout() %>%
      htmlwidgets::onRender(bar_highlight_js) |>
      config(displayModeBar = FALSE)
  })

  # --- Website metrics ---

  output$metric_web_top_domain <- renderUI({
    top <- websites |>
      filter(!is.na(domain)) |>
      group_by(domain) |>
      summarise(hours = sum(active_duration) / 3600, .groups = "drop") |>
      slice_max(hours, n = 1)
    HTML(paste0(top$domain,
                "<br><small style='color:#9ca3af;font-size:0.75rem'>",
                round(top$hours, 0), "h total</small>"))
  })

  output$metric_web_ai <- renderUI({
    ai_weekly <- websites |>
      filter(category == "AI Tools") |>
      mutate(week = floor_date(event_start, "week")) |>
      group_by(week) |>
      summarise(hrs = sum(active_duration) / 3600, .groups = "drop") |>
      pull(hrs) |> mean()
    HTML(paste0(round(ai_weekly, 1), "h/wk"))
  })

  output$metric_web_domains <- renderUI({
    n <- websites |> filter(!is.na(domain)) |> pull(domain) |> n_distinct()
    HTML(as.character(n))
  })
}
