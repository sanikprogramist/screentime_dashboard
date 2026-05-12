# Screen Time Dashboard

A 13-week personal analytics project built to answer one question: **where does my time actually go?**
👉 [Access the dashboard here](https://alex-markov.shinyapps.io/screentime-dashboard/)

---

## Rationale

**My generation uses technology constantly—and often unhealthily.** The average person now spends [over 7 hours per day](https://datareportal.com/global-overview-report) on digital media. For knowledge workers, it's often much higher.

Research is increasingly clear: [heavy digital entertainment consumption is linked to anxiety, depression, and reduced focus](https://www.apa.org/science/about/psa/social-media). Yet most people drastically underestimate their own usage. Studies show we're often [off by hours](https://www.theverge.com/2021/1/29/22252019/time-on-app-screen-time-estimates-inaccurate-study).

**The problem:** iPhones and Android phones offer detailed screen time breakdowns. Windows does not. I suspected I was spending too much time on video games and wasting my days on YouTube during work hours, but I had no solid data to confront the habit.

**The solution:** I built this dashboard. Raw ActivityWatch data → SQL, cleaning → R data analysis → interactive Shiny visualization. Now I can see exactly where time goes and, crucially, whether I'm actually changing my behavior.

---

## Key Findings

| | |
|---|---|
| 🕐 **501 hours** of screen time tracked | across 13 weeks (Feb – May 2026) | That's around 39h per week on my laptop. |
| 🎮 **97% drop** in gaming | from ~13h/wk in week 1 to under 1h by the end |
| 🎯 **61% productivity rate** during business hours | ~10h of focused work per week (9AM–5PM, weekdays) |
| 🚨 **Entertainment & Leisure** is the #1 drain | 22.4h lost during work hours, mostly YouTube |
| 🤖 **2.6h/wk** on AI tools | claude.ai and chatgpt.com — a consistent part of the dev workflow |
| 🌐 **docs.google.com** is the top domain | driven by writing scientific articles during a research assistant contract |
| ⏰ **Peak usage hour: 21:00** | evening sessions skew the all-day average significantly |


## Dashboard Tabs

**About** — project overview, key stats, and how it was built.

**Apps** — weekly/monthly breakdown of all desktop app usage by category, with interactive category filtering and trend metrics.

**Websites** — browser activity by category and domain. Reveals a worrying recent trend toward social media and entertainment usage.

**Time of Day** — average usage by hour across each day of the week. Reveals a strong 21:00 spike from recurring evening activities (board games, D&D, family calls).

**Productivity** — business hours analysis (weekdays, 9AM–5PM). Stacked area chart of productive vs. unproductive time over the 13 weeks, plus bar charts for the top productivity drains and where focused time actually goes.

---

## Tools Used

| Layer | Tools |
|---|---|
| Data collection | [ActivityWatch](https://activitywatch.net/) |
| Storage | SQLite (via ActivityWatch) |
| Processing | R, tidyverse, lubridate, DBI / RSQLite |
| Dashboard | R Shiny, Plotly, ggplot2, plotly |

---

## Project Structure

```
screentime_app/
├── app.R               # Entry point
├── globals.R           # Data loading, precomputed stats, theme
├── ui.R                # Shiny UI
├── server.R            # Shiny server logic
├── data_preparation.R  # SQL → RDS cleaning pipeline
└── data/               # Generated .rds files (not committed)
```
