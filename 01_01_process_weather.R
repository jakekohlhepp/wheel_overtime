#' =============================================================================
#' PROCESS DAILY WEATHER DATA
#' =============================================================================
#' Reads raw NOAA weather CSV for the configured station, selects and types the
#' relevant columns (date, tmax, tmin, prcp), and saves an .rds for downstream
#' merges in 01_04_mk_expanded_pay.R.
#'
#' Input:  20190811_weather/data/1834210.csv
#' Output: data/weather_daily.rds
#' =============================================================================

library('data.table')

source('config.R')
source('utils/logging.R')

log_init('01_01_process_weather.R')

input_path  <- file.path(CONFIG$raw_weather_dir, 'data', '1834210.csv')
output_path <- file.path(CONFIG$data_dir, 'weather_daily.rds')

assert_required_files(input_path)
ensure_directory(CONFIG$data_dir)

weather <- fread(input_path)
## Keep only the configured station (one station per file in practice, but be explicit)
weather <- weather[NAME == CONFIG$weather_station_name]
stopifnot(nrow(weather) > 0)

weather[, date := as.Date(DATE)]
weather[, tmax := as.numeric(TMAX)]
weather[, tmin := as.numeric(TMIN)]
weather[, prcp := as.numeric(PRCP)]
## All three weather variables must be present for every observation day
stopifnot(all(!is.na(weather$tmax)))
stopifnot(all(!is.na(weather$tmin)))
stopifnot(all(!is.na(weather$prcp)))

weather <- weather[, .(prcp, tmax, tmin, date)]
setorder(weather, date)
## One row per calendar day
stopifnot(nrow(weather) == uniqueN(weather$date))

saveRDS(weather, output_path)
log_message(paste('Saved', output_path))
log_complete(success = TRUE)
