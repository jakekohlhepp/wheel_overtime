library('data.table')
library('haven')

normalize_column <- function(x) {
  if (inherits(x, 'haven_labelled')) return(as.numeric(x))
  if (inherits(x, 'POSIXct')) return(as.numeric(as.POSIXct(x, tz = 'UTC')))
  if (inherits(x, 'Date')) return(as.numeric(as.Date(x)))
  x
}

compare_numeric <- function(x, y, tol = 1e-10) {
  same_na <- identical(is.na(x), is.na(y))
  if (!same_na) return(FALSE)
  keep <- !(is.na(x) | is.na(y))
  if (!any(keep)) return(TRUE)
  max(abs(x[keep] - y[keep])) <= tol
}

compare_dta <- function(candidate_path, target_path) {
  candidate <- read_dta(candidate_path)
  target <- read_dta(target_path)
  stopifnot(identical(names(candidate), names(target)))
  stopifnot(nrow(candidate) == nrow(target))
  for (col in names(candidate)) {
    x <- normalize_column(candidate[[col]])
    y <- normalize_column(target[[col]])
    if (is.numeric(x) || is.integer(x)) {
      if (!compare_numeric(as.numeric(x), as.numeric(y))) stop('Mismatch in ', basename(candidate_path), ' column ', col)
    } else {
      if (!identical(x, y)) stop('Mismatch in ', basename(candidate_path), ' column ', col)
    }
  }
}

compare_csv_hash <- function(candidate_path, target_path) {
  cand_hash <- unname(tools::md5sum(candidate_path))
  targ_hash <- unname(tools::md5sum(target_path))
  if (!identical(cand_hash, targ_hash)) {
    stop('CSV hash mismatch for ', basename(candidate_path), ': ', cand_hash, ' vs ', targ_hash)
  }
}

scratch_root <- file.path(tempdir(), paste0('stata_rewrite_', format(Sys.time(), '%Y%m%d%H%M%S')))
scratch_data <- file.path(scratch_root, 'data')
scratch_out <- file.path(scratch_root, 'out')
scratch_logs <- file.path(scratch_root, 'logs')
dir.create(scratch_data, recursive = TRUE)
dir.create(scratch_out, recursive = TRUE)
dir.create(scratch_logs, recursive = TRUE)
file.copy(file.path('out', 'list_var_desc.csv'), file.path(scratch_out, 'list_var_desc.csv'), overwrite = TRUE)
Sys.setenv(WHEEL_DATA_DIR = scratch_data)
Sys.setenv(WHEEL_OUT_DIR = scratch_out)
Sys.setenv(WHEEL_LOG_DIR = scratch_logs)

source('run_stata_rewrites.R')

compare_dta(file.path(scratch_data, 'weather_daily.dta'), file.path('20190811_weather', 'data', 'weather_daily.dta'))
compare_dta(file.path(scratch_data, 'holidays.dta'), file.path('20190814_fed_holidays', 'data', 'holidays.dta'))
compare_dta(file.path(scratch_data, 'employee_data.dta'), file.path('data', 'employee_data.dta'))
compare_dta(file.path(scratch_data, 'pay_data.dta'), file.path('data', 'pay_data.dta'))
compare_dta(file.path(scratch_data, 'workers_comp.dta'), file.path('data', 'workers_comp.dta'))
compare_dta(file.path(scratch_data, '01_02_fornetwork.dta'), file.path('data', '01_02_fornetwork.dta'))
compare_dta(file.path(scratch_data, 'working_expanded.dta'), file.path('data', 'working_expanded.dta'))
for (window in c(30, 90, 180, 1000)) {
  compare_csv_hash(file.path(scratch_data, paste0('01_03_pre_network_', window, '.csv')),
                   file.path('data', paste0('01_03_pre_network_', window, '.csv')))
}

message('Stata rewrite verification passed. Scratch outputs: ', scratch_root)
