library('data.table')
library('haven')

source('config.R')
source('utils/logging.R')

log_init('01_01_process_weather.R')

input_path <- file.path(CONFIG$raw_weather_dir, 'data', '1834210.csv')
output_path <- file.path(CONFIG$data_dir, 'weather_daily.dta')

assert_required_files(input_path)
ensure_directory(CONFIG$data_dir)

weather <- fread(input_path)
weather <- weather[NAME == CONFIG$weather_station_name]
stopifnot(nrow(weather) > 0)

weather[, date := as.Date(DATE)]
weather[, tmax := as.numeric(TMAX)]
weather[, tmin := as.numeric(TMIN)]
weather[, prcp := as.numeric(PRCP)]
stopifnot(all(!is.na(weather$tmax)))
stopifnot(all(!is.na(weather$tmin)))
stopifnot(all(!is.na(weather$prcp)))

weather <- weather[, .(prcp, tmax, tmin, date)]
setorder(weather, date)
stopifnot(nrow(weather) == uniqueN(weather$date))
attr(weather$date, 'format.stata') <- '%td'

write_dta(weather, output_path, version = 14)
log_message(paste('Saved', output_path))
log_complete(success = TRUE)

