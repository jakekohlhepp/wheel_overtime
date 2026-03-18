library('data.table')
library('haven')

source('config.R')
source('utils/logging.R')

log_init('01_03_mk_pre_network.R')

working_path <- file.path(CONFIG$data_dir, 'working_expanded.dta')
pay_path <- file.path(CONFIG$data_dir, 'pay_data.dta')
fornetwork_path <- file.path(CONFIG$data_dir, '01_02_fornetwork.dta')
employee_path <- file.path(CONFIG$data_dir, 'employee_data.dta')

assert_required_files(c(working_path, pay_path, fornetwork_path, employee_path))
ensure_directory(CONFIG$data_dir)

rolling_window_matrix <- function(mat, window) {
  if (!nrow(mat) || !ncol(mat)) return(mat)
  cs <- apply(mat, 2, cumsum)
  if (is.null(dim(cs))) cs <- matrix(cs, ncol = ncol(mat))
  out <- cs
  lag_n <- window + 1L
  if (nrow(mat) > lag_n) {
    out[(lag_n + 1L):nrow(mat), ] <- cs[(lag_n + 1L):nrow(mat), , drop = FALSE] - cs[1:(nrow(mat) - lag_n), , drop = FALSE]
  }
  out
}

sum_na0 <- function(x) sum(x, na.rm = TRUE)

working_expanded <- as.data.table(read_dta(working_path))
stopifnot(working_expanded[, all(job_class_title[1] == job_class_title), by = employee_name][, all(V1)])
stopifnot(sum((working_expanded$varot_hours > 0) & working_expanded$job_class_title == 'TRAF OFFICER I', na.rm = TRUE) == 1L)

pay <- as.data.table(read_dta(pay_path))
stopifnot(all(!is.na(pay$yearsoldonworkdate)))
pay[, analysis_workdate := as.Date(work_date)]
pay[, age_20150101 := yearsoldonworkdate - as.numeric(analysis_workdate - CONFIG$injury_window_start) / 365.25]
stopifnot(all(!is.na(pay$age_20150101)))
setorder(pay, employee_name, age_20150101, analysis_workdate)
stopifnot(pay[, all(round((age_20150101 - age_20150101[1]) * 52) == 0), by = employee_name][, all(V1)])
age <- unique(pay[, .(employee_name, age_20150101)], by = 'employee_name')

fornetwork <- as.data.table(read_dta(fornetwork_path))
employee_data <- as.data.table(read_dta(employee_path))
fornetwork <- merge(fornetwork, employee_data[, .(employee_name, job_class_title)], by = 'employee_name', all.x = TRUE, sort = FALSE)
stopifnot(all(!is.na(fornetwork$job_class_title)))
fornetwork <- fornetwork[job_class_title == 'TRAF OFFICER II']
fornetwork[, job_class_title := NULL]

fornetwork[, exposure := 1 / (.N - 1), by = .(geo_div, analysis_workdate)]
fornetwork[!is.finite(exposure), exposure := NA_real_]
fornetwork[, emp_num := as.integer(gsub(CONFIG$employee_name_pattern, '', employee_name))]

wide_div <- dcast(fornetwork[, .(geo_div, analysis_workdate, emp_num, exposure)],
                  geo_div + analysis_workdate ~ emp_num, value.var = 'exposure')
full_emp_nums <- seq(min(fornetwork$emp_num), max(fornetwork$emp_num))
missing_emp_cols <- setdiff(as.character(full_emp_nums), setdiff(names(wide_div), c('geo_div', 'analysis_workdate')))
for (col in missing_emp_cols) wide_div[, (col) := NA_real_]
exposure_ids <- as.character(full_emp_nums)
setcolorder(wide_div, c('geo_div', 'analysis_workdate', exposure_ids))
setnames(wide_div, exposure_ids, paste0('exposure', exposure_ids))
exposure_cols <- paste0('exposure', exposure_ids)
for (col in exposure_cols) wide_div[is.na(get(col)), (col) := 0]

exposure_panel <- merge(fornetwork[, .(geo_div, analysis_workdate, employee_name)],
                        wide_div, by = c('geo_div', 'analysis_workdate'), all.x = TRUE, sort = FALSE)
exposure_panel <- exposure_panel[, lapply(.SD, sum_na0), by = .(employee_name, analysis_workdate), .SDcols = exposure_cols]

panel_base <- merge(exposure_panel, working_expanded, by = c('employee_name', 'analysis_workdate'), all = TRUE, sort = FALSE)
panel_base <- panel_base[job_class_title == 'TRAF OFFICER II']
stopifnot(nrow(panel_base) == uniqueN(panel_base[, .(employee_name, analysis_workdate)]))
for (col in exposure_cols) panel_base[is.na(get(col)), (col) := 0]
setorder(panel_base, employee_name, analysis_workdate)

for (window in CONFIG$pre_network_windows) {
  panel <- copy(panel_base)
  roll_cols <- paste0('roll', window, '_', exposure_cols)
  panel[, (roll_cols) := {
    mat <- as.matrix(.SD)
    storage.mode(mat) <- 'double'
    out <- rolling_window_matrix(mat, window)
    lapply(seq_len(ncol(out)), function(i) out[, i])
  }, by = employee_name, .SDcols = exposure_cols]
  panel[, (exposure_cols) := NULL]

  panel[, has_work := as.numeric(tot_hours > 0 & analysis_workdate >= CONFIG$injury_window_start & analysis_workdate <= CONFIG$fiscal_year_end)]
  panel[, max_flag := max(has_work), by = employee_name]
  stopifnot(sum(panel[, .(max_flag = max(max_flag)), by = employee_name]$max_flag == 0) == 36L)
  panel <- panel[max_flag > 0]
  panel[, c('has_work', 'max_flag') := NULL]

  panel[is.na(varstandard_hours), varstandard_hours := 0]
  panel[is.na(varot_hours), varot_hours := 0]
  stopifnot(panel[matched_injury == 1, all(varstandard_hours > 0 | varot_hours > 0)])

  stopifnot(all(!is.na(panel$original_hire_date)))
  panel[, tenure := as.numeric(analysis_workdate - as.Date(original_hire_date)) / 365.25]
  stopifnot(all(panel$tenure >= 0))

  panel <- merge(panel, age, by = 'employee_name', all.x = TRUE, sort = FALSE)
  stopifnot(all(!is.na(panel$age_20150101)))
  panel[, an_age := age_20150101 + as.numeric(analysis_workdate - CONFIG$injury_window_start) / 365.25]

  setorder(panel, analysis_workdate, tenure, employee_name)
  panel[, seniority_rank := frank(-tenure, ties.method = 'min'), by = analysis_workdate]
  panel[, first_inj_date := {
    keep <- analysis_workdate[matched_injury == 1]
    if (length(keep) == 0) as.Date(NA) else keep[1]
  }, by = employee_name]

  panel[, empid := NULL]
  setorder(panel, employee_name, analysis_workdate)

  ordered_roll_cols <- roll_cols[order(as.integer(sub(paste0('roll', window, '_exposure'), '', roll_cols)))]
  setcolorder(panel, c(
    'employee_name', 'analysis_workdate', 'dept', 'yearsoldonworkdate',
    'maximum_gap_2015', 'max_rate', 'div1', 'div2', 'lhours_1', 'lhours_2',
    'shours_1', 'shours_2', 'leave_hours', 'tot_hours', 'sick_hours',
    'varstandard_hours', 'varot_hours', 'work_pay_amount', 'ot_pay_amount',
    'iod_flag', 'flag_hours_zeroed', 'doi', 'timeofinj', 'natureofinjury',
    'bodypart', 'claimcause', 'claimcausegroup', 'contribcause', 'medpd',
    'matched_injury', 'not_worked', 'cum_tot_hours', 'cum_varstandard_hours',
    'cum_varot_hours', 'job_class', 'job_class_title', 'jobclassdescription',
    'civilian_entry_date', 'original_hire_date', 'job_end_date', 'job_status',
    'totalnumberofinjuries', 'an_week', 'an_month', 'prcp', 'tmax', 'tmin',
    'holiday', 'is_holiday', 'rain', ordered_roll_cols, 'tenure',
    'age_20150101', 'an_age', 'seniority_rank', 'first_inj_date'
  ))

  export_panel <- copy(panel)
  analysis_dates <- export_panel$analysis_workdate
  export_panel[, analysis_workdate := format_stata_date(analysis_workdate)]
  export_panel[, doi := format_stata_date(doi)]
  export_panel[, first_inj_date := format_stata_date(first_inj_date)]
  export_panel[, timeofinj := format_stata_datetime(timeofinj)]
  export_panel[, civilian_entry_date := format_stata_datetime(civilian_entry_date)]
  export_panel[, original_hire_date := format_stata_datetime(original_hire_date)]
  export_panel[, an_week := format_stata_week(analysis_dates)]

  output_path <- file.path(CONFIG$data_dir, paste0('01_03_pre_network_', window, '.csv'))
  fwrite(export_panel, output_path, na = '', eol = '\r\n')
  log_message(paste('Saved', output_path))
}

log_complete(success = TRUE)

