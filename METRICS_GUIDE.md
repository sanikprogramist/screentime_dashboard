# Screen Time Dashboard Metrics Guide

## Overview
Your dashboard now displays three key performance indicators at the top of each tab that tell the story of your screen time discipline and productivity.

## Metrics Architecture

### 1. **Total Screen Time** 📱
- **What it shows:** Total cumulative hours across apps and websites
- **Why it matters:** Portfolio piece baseline — shows total digital engagement
- **Location in code:** 
  - UI: `metric_total_hours` / `metric_total_hours_web`
  - Calculation: `(sum(apps$active_duration) + sum(websites$active_duration)) / 3600`

### 2. **Weekly Average** 📊
- **What it shows:** Average screen time per week
- **Why it matters:** Normalizes total time across your data collection period; easier to understand daily habits
- **The Trend Arrow:** Shows % change from first half of data to second half
  - **↓ Green arrow** = Decreasing usage (positive story!)
  - **↑ Red arrow** = Increasing usage
- **Location in code:**
  - UI: `metric_weekly_avg` / `metric_weekly_avg_web`
  - Calculation: Total hours ÷ weeks elapsed

### 3. **Productivity Score** ✨
- **What it shows:** Percentage of your screen time spent on productive/professional categories
- **Why it matters:** Shows balance and intentionality — not just "low screen time" but "smart screen time"
- **Categories counted as "productive":**
  - Apps: Browser, Development, Communication, Productivity, OS & System
  - Websites: Learning, Documentation, Work, Development, Utilities
- **Location in code:**
  - UI: `metric_productivity` / `metric_productivity_web`
  - Calculation: (Productive hours) / (Total hours) × 100

## Customization

### Adjusting Category Classifications
Edit these variables at the top of the server function to match your actual category names:

```r
PRODUCTIVE_APPS <- c("Browser", "Development", "Communication", "Productivity", "OS & System")
PRODUCTIVE_WEB <- c("Learning", "Documentation", "Work", "Development", "Utilities")
```

**To see your actual category names:**
```r
# Check what categories are in your data
apps |> distinct(category) |> arrange(category)
websites |> distinct(category) |> arrange(category)
```

### Styling
The metrics use custom CSS defined in `app_theme`. Edit these classes in the `bs_add_rules()` section to customize:

- `.metrics-row` — Grid layout and spacing
- `.metric-card` — Card styling, hover effects, shadows
- `.metric-value` — Font size and color of the big number
- `.metric-trend` — Color of the trend arrow (green for positive)

### Making Metrics Reactive to Filters
Currently, metrics display overall totals. To make them react to your category filters (sidebar checkboxes), change the calculation to use `apps_data()` and `web_data()` reactives instead of raw data.

**Example for productivity that reacts to category filters:**
```r
output$metric_productivity <- renderUI({
  all_data <- bind_rows(apps_data(), web_data())
  productive_hours <- all_data |>
    filter(category %in% c(PRODUCTIVE_APPS, PRODUCTIVE_WEB)) |>
    pull(hours) |> sum()
  
  total_hours <- sum(all_data$hours)
  productivity_pct <- (productive_hours / total_hours) * 100
  
  HTML(paste0(round(productivity_pct, 0), "%"))
})
```

## Story Elements

This design helps communicate:

1. **Responsibility** → High productivity score
2. **Self-awareness** → Trend arrow shows you're actively managing
3. **Technical skill** → Custom styling and data pipeline
4. **Balance** → Metrics show intentional, not reckless reduction

The downward trend in particular is powerful for a portfolio piece — it shows you don't just analyze data, you act on insights.

## Future Enhancements

- Add a sparkline chart inside each metric card (small mini-chart of trend)
- Make metrics filterable by date range
- Add comparison view: "This week vs. last week"
- Export metrics as image/PDF for social/LinkedIn
