#' =============================================================================
#' PROCESS US FEDERAL HOLIDAYS
#' =============================================================================
#' Reads raw holiday calendar CSV (2011-2020), parses dates, and saves an .rds
#' used to flag holiday days in the expanded officer panel (01_04).
#'
#' Input:  20190814_fed_holidays/data/us-federal-holidays-2011-2020.csv
#' Output: data/holidays.rds
#' =============================================================================

library('data.table')

source('config.R')
source('utils/logging.R')

log_init('01_02_process_holidays.R')

input_path  <- file.path(CONFIG$raw_holidays_dir, 'data', 'us-federal-holidays-2011-2020.csv')
output_path <- file.path(CONFIG$data_dir, 'holidays.rds')

assert_required_files(input_path)
ensure_directory(CONFIG$data_dir)

holidays <- fread(input_path)
stopifnot('Date' %in% names(holidays))

holidays[, date := as.Date(Date, format = '%m/%d/%Y')]
stopifnot(all(!is.na(holidays$date)))
holidays <- holidays[, .(holiday = Holiday, date)]

saveRDS(holidays, output_path)
log_message(paste('Saved', output_path))
log_complete(success = TRUE)
