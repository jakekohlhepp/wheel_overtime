library('data.table')
library('haven')

normalize_column <- function(x) {
  if (inherits(x, 'haven_labelled')) return(as.numeric(x))
  if (inherits(x, 'POSIXct')) return(as.numeric(as.POSIXct(x, tz = 'UTC')))
  if (inherits(x, 'Date')) return(as.numeric(as.Date(x)))
  if (is.character(x)) return(unname(x))
  x
}

compare_numeric <- function(x, y, tol = 1e-10) {
  same_na <- identical(is.na(x), is.na(y))
  if (!same_na) return(FALSE)
  keep <- !(is.na(x) | is.na(y))
  if (!any(keep)) return(TRUE)
  max(abs(x[keep] - y[keep])) <= tol
}

normalize_dta_for_compare <- function(path) {
  dt <- as.data.table(read_dta(path))
  cols <- names(dt)
  out <- as.data.table(stats::setNames(lapply(cols, function(col) normalize_column(dt[[col]])), cols))
  out
}

compare_dta <- function(candidate_path, target_path) {
  candidate <- normalize_dta_for_compare(candidate_path)
  target <- normalize_dta_for_compare(target_path)
  stopifnot(identical(names(candidate), names(target)))
  if (nrow(candidate) != nrow(target)) stop('Row-count mismatch in ', basename(candidate_path))
  setkeyv(candidate, names(candidate))
  setkeyv(target, names(target))
  extra <- fsetdiff(candidate, target)
  missing <- fsetdiff(target, candidate)
  if (nrow(extra) > 0 || nrow(missing) > 0) {
    stop(
      'Row-set mismatch in ', basename(candidate_path),
      ': extra=', nrow(extra), ', missing=', nrow(missing)
    )
  }
}

compare_csv_hash <- function(candidate_path, target_path) {
  cand_hash <- unname(tools::md5sum(candidate_path))
  targ_hash <- unname(tools::md5sum(target_path))
  if (!identical(cand_hash, targ_hash)) {
    stop('CSV hash mismatch for ', basename(candidate_path), ': ', cand_hash, ' vs ', targ_hash)
  }
}

baseline_data_dir <- file.path('..', 'mkdata', 'data')
scratch_root <- file.path('.verify_scratch', paste0('stata_rewrite_', format(Sys.time(), '%Y%m%d%H%M%S')))
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

runner_env <- new.env(parent = globalenv())
sys.source('run_stata_rewrites.R', envir = runner_env)

compare_dta(file.path(scratch_data, 'weather_daily.dta'), file.path('20190811_weather', 'data', 'weather_daily.dta'))
compare_dta(file.path(scratch_data, 'holidays.dta'), file.path('20190814_fed_holidays', 'data', 'holidays.dta'))
compare_dta(file.path(scratch_data, 'employee_data.dta'), file.path(baseline_data_dir, 'employee_data.dta'))
compare_dta(file.path(scratch_data, 'pay_data.dta'), file.path(baseline_data_dir, 'pay_data.dta'))
compare_dta(file.path(scratch_data, 'workers_comp.dta'), file.path(baseline_data_dir, 'workers_comp.dta'))
compare_dta(file.path(scratch_data, '01_02_fornetwork.dta'), file.path(baseline_data_dir, '01_02_fornetwork.dta'))
compare_dta(file.path(scratch_data, 'working_expanded.dta'), file.path(baseline_data_dir, 'working_expanded.dta'))
for (window in c(30, 90, 180, 1000)) {
  compare_csv_hash(file.path(scratch_data, paste0('01_03_pre_network_', window, '.csv')),
                   file.path(baseline_data_dir, paste0('01_03_pre_network_', window, '.csv')))
}

message('Stata rewrite verification passed. Scratch outputs: ', scratch_root)


