library('data.table')
library('haven')

source('config.R')
source('utils/logging.R')

log_init('process_holidays.R')

input_path <- file.path(CONFIG$raw_holidays_dir, 'data', 'us-federal-holidays-2011-2020.csv')
output_path <- file.path(CONFIG$data_dir, 'holidays.dta')

assert_required_files(input_path)
ensure_directory(CONFIG$data_dir)

holidays <- fread(input_path)
stopifnot('Date' %in% names(holidays))

holidays[, date := as.Date(Date, format = '%m/%d/%Y')]
stopifnot(all(!is.na(holidays$date)))
holidays <- holidays[, .(holiday = Holiday, date)]
attr(holidays$holiday, 'label') <- 'Holiday'
attr(holidays$holiday, 'format.stata') <- '%35s'
attr(holidays$date, 'format.stata') <- '%td'

write_dta(holidays, output_path, version = 14)
log_message(paste('Saved', output_path))
log_complete(success = TRUE)

