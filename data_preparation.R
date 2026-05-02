# DATA PREPARATION SCRIPT
#   Reads the ActivityWatch SQLite database, removes AFK (idle) time from
#   app and website events, and saves clean data for the Shiny app.

library(DBI)
library(RSQLite)
library(tidyverse)
library(lubridate)
library(data.table)
library(jsonlite)
# --- 1. Load raw data ---------------------------------------------------------

db_path <- "C:/Users/Sanik/AppData/Local/activitywatch/activitywatch/aw-server/peewee-sqlite.v2.db"

con     <- dbConnect(SQLite(), db_path)
full_df <- dbGetQuery(con, "SELECT bucket_id, timestamp, duration, datastr FROM eventmodel")
dbDisconnect(con)

cutoff_date <- ymd_hms("2026-02-01 00:00:00")
full_df <- full_df |>
  mutate(timestamp = ymd_hms(timestamp)) |>
  filter(timestamp >= cutoff_date)

apps     <- full_df |> filter(bucket_id == 1)  # active window events
afk_df   <- full_df |> filter(bucket_id == 2)  # idle/active status events
websites <- full_df |> filter(bucket_id == 3)  # firefox tab events

# --- 2. Build merged AFK intervals --------------------------------------------
# Our first task is to remove the time where I was afk. 
# AFK time is not meaningful laptop usage, and therefore should be excluded


# Parse AFK events, keep only "afk" (idle) periods, and deduplicate
afk_raw <- afk_df |>
  mutate(
    status    = str_match(datastr, '"status":\\s*"([^"]+)"')[, 2],
    afk_start = ymd_hms(timestamp),
    afk_end   = afk_start + duration
  ) |>
  filter(status == "afk") |>
  distinct(afk_start, afk_end) |>
  mutate(a_start = as.numeric(afk_start), a_end = as.numeric(afk_end))

# Merge overlapping intervals to prevent double-counting when subtracting
merge_afk_intervals <- function(ivs) {
  ivs       <- arrange(ivs, afk_start)
  merged    <- list()
  cur_start <- ivs$afk_start[1]
  cur_end   <- ivs$afk_end[1]

  for (i in seq(2, nrow(ivs))) {
    if (ivs$afk_start[i] <= cur_end) {
      cur_end <- max(cur_end, ivs$afk_end[i])
    } else {
      merged[[length(merged) + 1]] <- list(afk_start = cur_start, afk_end = cur_end)
      cur_start <- ivs$afk_start[i]
      cur_end   <- ivs$afk_end[i]
    }
  }
  merged[[length(merged) + 1]] <- list(afk_start = cur_start, afk_end = cur_end)

  bind_rows(merged) |>
    mutate(a_start = as.numeric(afk_start), a_end = as.numeric(afk_end))
}

afk_merged <- merge_afk_intervals(afk_raw)

# --- 3. Subtract AFK time from events ----------------------------------------

# For each event, sums the overlap with AFK intervals and subtracts it from
# duration. Uses a data.table non-equi join for performance.
subtract_afk <- function(events_df, afk_ivs) {
  evt_dt <- events_df |>
    mutate(
      event_start = ymd_hms(timestamp),
      event_end   = event_start + duration,
      row_id      = row_number(),
      e_start     = as.numeric(event_start),
      e_end       = as.numeric(event_end)
    ) |>
    as.data.table()

  afk_dt <- as.data.table(afk_ivs)

  # Non-equi join: overlap condition is afk_start < event_end AND afk_end > event_start
  joined <- afk_dt[evt_dt,
    on     = .(a_start < e_end, a_end > e_start),
    .(row_id = i.row_id,
      overlap = pmax(0, pmin(x.a_end, i.e_end) - pmax(x.a_start, i.e_start))),
    allow.cartesian = TRUE,
    nomatch = NA
  ]

  afk_totals <- joined[!is.na(overlap), .(total_afk = sum(overlap)), by = row_id]

  evt_dt[afk_totals, on = "row_id", total_afk := i.total_afk]
  evt_dt[is.na(total_afk), total_afk := 0]
  evt_dt[, active_duration := pmax(0, duration - total_afk)]

  as_tibble(evt_dt) |> select(-e_start, -e_end)
}

apps_clean     <- subtract_afk(apps, afk_merged) 
websites_clean <- subtract_afk(websites, afk_merged)

# --- 4. Further cleaning -----------------------------------------------------

local_tz <- "Europe/Berlin"

apps_clean <- apps_clean |>
  filter(active_duration > 0.001) |>
  mutate(
    app         = str_match(datastr, '"app":\\s*"([^"]+)"')[, 2],
    title       = str_match(datastr, '"title":\\s*"([^"]*)"')[, 2],
    event_start = with_tz(event_start, local_tz),
    event_end   = with_tz(event_end,   local_tz)
  ) |>
  select(event_start, event_end, active_duration, app, title)

websites_clean <- websites_clean |>
  filter(active_duration > 0.001) |>
  mutate(
    url         = str_match(datastr, '"url":\\s*"([^"]+)"')[, 2],
    title       = str_match(datastr, '"title":\\s*"([^"]*)"')[, 2],
    domain      = str_extract(url, "(?<=://)([^/]+)"),
    event_start = with_tz(event_start, local_tz),
    event_end   = with_tz(event_end,   local_tz)
  ) |>
  select(event_start, event_end, active_duration, domain, title)

# --- 5. Categorisation -------------------------------------------------------
# Give top 100 domains and all apps categories using LLMs
library(tidyverse)

# Top 100 domains with hours + a few sample titles for LLM context
domains_template <- websites_clean |>
  group_by(name = domain) |>
  summarise(
    hours         = round(sum(active_duration) / 3600, 2),
    sample_titles = paste(head(unique(title[title != "" & !is.na(title)]), 3), collapse = " | "),
    .groups = "drop"
  ) |>
  arrange(desc(hours)) |>
  slice_head(n = 100) |>
  mutate(type = "website", category = "")

# All apps with hours + sample titles
apps_template <- apps_clean |>
  group_by(name = app) |>
  summarise(
    hours         = round(sum(active_duration) / 3600, 2),
    sample_titles = paste(head(unique(title[title != "" & !is.na(title)]), 3), collapse = " | "),
    .groups = "drop"
  ) |>
  arrange(desc(hours)) |>
  mutate(type = "app", category = "")

template <- bind_rows(domains_template, apps_template) |>
  select(type, name, hours, category, sample_titles)

write_csv(template, "data/categories_template.csv")
# This template can be fed into an LLM which will automatically categorise the top websites and apps into categories
# The prompt used was: "I have a CSV with columns: type (app or website), name (domain or .exe name), hours (time spent), and sample_titles (an example of a window titles seen while using it, just for context for you). Please fill in the category column for each row. Use consistent category names like. If you're unsure you can look up the website/process name. Return the result as a CSV with only the name and category columns."
# This was done using Anthropic Claude model Sonnet 4.6
# it is then saved under "data/categories.csv"

categories <- read_csv("data/categories.csv")
apps_clean = apps_clean |> 
  left_join(categories |> mutate(app=name) |> select(-name))
websites_clean = websites_clean |>
  left_join(categories |> mutate(domain=name) |> select(-name))




# --- 6. Split events by hour for time-of-day analysis ------------------------
# AFK time has already been removed from active_duration, so we are
# distributing genuine active time across hour bins. Each event is split at
# hour boundaries so the ToD tab can aggregate per-hour correctly.

split_by_hour <- function(df) {
  # First pass with plain ceiling_date (events starting exactly on the hour
  # will have time_until_next_hour == 0 and land in overflow — handled below)
  df <- df |>
    mutate(
      next_hour_boundary   = ceiling_date(event_start, "hour"),
      time_until_next_hour = as.numeric(next_hour_boundary - event_start),
      excess_time          = active_duration - time_until_next_hour
    )

  no_overflow <- df |>
    filter(excess_time <= 0) |>
    select(-next_hour_boundary, -time_until_next_hour, -excess_time)

  current <- df |> filter(excess_time > 0)

  while (TRUE) {
    # +1 ensures ceiling_date moves forward even when event_start is on the
    # hour boundary (ceiling_date of an exact boundary returns the same time)
    current <- current |>
      mutate(
        next_hour_boundary   = ceiling_date(event_start + 1, "hour"),
        time_until_next_hour = as.numeric(next_hour_boundary - event_start),
        excess_time          = active_duration - time_until_next_hour,
        active_duration      = as.numeric(time_until_next_hour)
      )

    overflow_rows <- current |>
      filter(excess_time > 0) |>
      mutate(
        event_start     = next_hour_boundary,
        active_duration = as.numeric(excess_time)
      )

    current <- current |>
      select(-next_hour_boundary, -time_until_next_hour, -excess_time)

    if (nrow(overflow_rows) == 0) break

    overflow_rows <- overflow_rows |>
      select(-next_hour_boundary, -time_until_next_hour, -excess_time)

    current <- bind_rows(current, overflow_rows)
  }

  bind_rows(no_overflow, current) |>
    mutate(hour = hour(event_start))
}

apps_tod     <- split_by_hour(apps_clean)
websites_tod <- split_by_hour(websites_clean)

# --- 7. Save -----------------------------------------------------------------

dir.create("data", showWarnings = FALSE)
#saveRDS(apps_clean,     "data/apps_clean.rds")
#saveRDS(websites_clean, "data/websites_clean.rds")
saveRDS(apps_tod,       "data/apps_clean.rds")
saveRDS(websites_tod,   "data/websites_clean.rds")

message(
  "Done. Saved ", nrow(apps_clean), " app events and ",
  nrow(websites_clean), " website events.\n",
  "ToD files: ", nrow(apps_tod), " app rows and ",
  nrow(websites_tod), " website rows after hour splitting."
)
