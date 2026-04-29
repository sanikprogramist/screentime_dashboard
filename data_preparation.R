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

apps     <- full_df |> filter(bucket_id == 1)  # active window events
afk_df   <- full_df |> filter(bucket_id == 2)  # idle/active status events
websites <- full_df |> filter(bucket_id == 3)  # firefox tab events

# --- 2. Build merged AFK intervals --------------------------------------------

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
  select(event_start, event_end, active_duration, url, domain, title)




# --- 4. Save -----------------------------------------------------------------

dir.create("data", showWarnings = FALSE)
saveRDS(apps_clean,     "data/apps_clean.rds")
saveRDS(websites_clean, "data/websites_clean.rds")

message(
  "Done. Saved ", nrow(apps_clean), " app events and ",
  nrow(websites_clean), " website events."
)
