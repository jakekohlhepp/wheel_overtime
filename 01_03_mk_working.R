library('data.table')
library('haven')

source('config.R')
source('utils/logging.R')

log_init('01_03_mk_working.R')

raw_path <- file.path(CONFIG$raw_pay_dir, 'data', 'anonymized_data_073117.dta')
info_path <- file.path(CONFIG$output_dir, 'list_var_desc.csv')
employee_path <- file.path(CONFIG$data_dir, 'employee_data.dta')
workers_path <- file.path(CONFIG$data_dir, 'workers_comp.dta')
pay_path <- file.path(CONFIG$data_dir, 'pay_data.dta')

assert_required_files(c(raw_path, info_path))
ensure_directory(CONFIG$data_dir)

constant_within_group <- function(x) {
  ref <- x[1]
  all((x == ref) | (is.na(x) & is.na(ref)))
}

raw <- as.data.table(read_dta(raw_path))
stopifnot(nrow(raw) == uniqueN(raw$v1))

stable_cols <- c('job_status', 'totalnumberofinjuries', 'original_hire_date',
                 'civilian_entry_date', 'job_end_date', 'job_class_title',
                 'job_class', 'jobclassdescription')

setorder(raw, employee_name, v1)
for (col in stable_cols) {
  stopifnot(raw[, constant_within_group(get(col)), by = employee_name][, all(V1)])
}

employee_data <- unique(raw[, .(
  employee_name,
  job_class,
  job_class_title,
  jobclassdescription,
  civilian_entry_date,
  original_hire_date,
  job_end_date,
  job_status,
  totalnumberofinjuries
)], by = 'employee_name')
setorder(employee_data, employee_name)
attr(employee_data$civilian_entry_date, 'format.stata') <- '%tc'
attr(employee_data$original_hire_date, 'format.stata') <- '%tc'
write_dta(employee_data, employee_path, version = 14)
log_message(paste('Saved', employee_path))

workers_comp <- copy(raw)
workers_comp <- workers_comp[, .(
  employee_name,
  timeofinj,
  cd,
  natureofinjury,
  cd1,
  bodypart,
  cd2,
  claimcause,
  claimcausegroup,
  cd3,
  contribcause,
  divcd,
  assigneddiv,
  iod4850pd,
  tdpd,
  pdpd,
  medpd,
  exppd,
  iodhours,
  empknowledgedt,
  howincidentoccurred,
  fms_department,
  begin_date,
  civilian_entry_date,
  original_hire_date,
  job_end_date,
  job_status,
  job_status_date,
  last_updated_by,
  last_update_date,
  doi
)]
workers_comp <- unique(workers_comp)
stopifnot(all(!is.na(workers_comp$doi[!is.na(workers_comp$timeofinj)])))
stopifnot(all(!is.na(workers_comp$timeofinj[!is.na(workers_comp$doi)])))
stopifnot(all(nzchar(workers_comp$natureofinjury[!is.na(workers_comp$doi)])))
stopifnot(all(!is.na(workers_comp$doi[nzchar(workers_comp$natureofinjury)])))
workers_comp <- workers_comp[!is.na(doi)]
workers_comp[, medpd := as.numeric(gsub(',', '', medpd, fixed = TRUE))]
attr(workers_comp$timeofinj, 'format.stata') <- '%tc'
attr(workers_comp$begin_date, 'format.stata') <- '%tc'
attr(workers_comp$civilian_entry_date, 'format.stata') <- '%tc'
attr(workers_comp$original_hire_date, 'format.stata') <- '%tc'
attr(workers_comp$job_status_date, 'format.stata') <- '%tc'
attr(workers_comp$last_update_date, 'format.stata') <- '%tc'
attr(workers_comp$doi, 'format.stata') <- '%td'
write_dta(workers_comp, workers_path, version = 14)
log_message(paste('Saved', workers_path))

info <- fread(info_path)
stopifnot('variation_description' %in% names(info))
info <- info[!is.na(variation_description) & variation_description != '']
setnames(info, 'variation_description', 'cleaned_variation_desc')
for (col in c('work', 'new_code', 'out_type', 'sick_subset')) {
  info[, (col) := as.numeric(get(col))]
}

pay <- copy(raw)
pay[, cleaned_variation_desc := gsub('"', '', variation_description, fixed = TRUE)]
setorder(pay, employee_name, work_date, variation_description, v1)
pay <- unique(pay[, .(
  employee_name,
  payroll_year,
  dept,
  div,
  pp,
  work_date,
  var_code,
  variation_description,
  totaling_group,
  hours,
  var_rate,
  pay_amount,
  yearsoldonworkdate,
  src,
  rec_date,
  cleaned_variation_desc
)])

stopifnot(all(pay$cleaned_variation_desc %chin% info$cleaned_variation_desc))
pay <- merge(pay, info, by = 'cleaned_variation_desc', all.x = TRUE, sort = FALSE)

pay_dates <- as.Date(pay$work_date)
pay[, cal_week := stata_week_num(pay_dates)]
pay[, cal_month := stata_month_num(pay_dates)]
pay[, cal_year := as.integer(format(pay_dates, '%Y'))]

pay[, temp_date := pay_dates]
pay[, temp_period := temp_date >= CONFIG$injury_window_start]
setorder(pay, employee_name, temp_period, temp_date)
pay[, temp_gap := as.numeric(temp_date - shift(temp_date)), by = .(employee_name, temp_period)]
pay[is.na(temp_gap), temp_gap := 0]
pay[temp_period == FALSE, temp_gap := 0]
pay[, maximum_gap_2015 := {
  keep <- temp_date >= CONFIG$injury_window_start
  out <- rep(0, .N)
  if (any(keep)) {
    out[keep] <- max(temp_gap[keep], na.rm = TRUE)
  }
  out
}, by = employee_name]
setorderv(pay, c('employee_name', 'maximum_gap_2015', 'temp_date'), order = c(1L, -1L, 1L))
pay[, maximum_gap_2015 := maximum_gap_2015[1], by = employee_name]
pay[, gap_end_date := as.Date(NA)]
pay[temp_gap >= CONFIG$gap_group_days, gap_end_date := temp_date]
pay[, gap_end_date := {
  if (all(is.na(gap_end_date))) {
    rep(as.Date(NA), .N)
  } else {
    rep(gap_end_date[which(!is.na(gap_end_date))[1]], .N)
  }
}, by = .(employee_name, work_date)]
pay[, gap_end := ifelse(is.na(gap_end_date), NA_real_, stata_daily_num(gap_end_date))]

pay <- pay[, .(
  employee_name,
  payroll_year,
  dept,
  div,
  pp,
  work_date,
  var_code,
  variation_description,
  totaling_group,
  hours,
  var_rate,
  pay_amount,
  yearsoldonworkdate,
  src,
  rec_date,
  cleaned_variation_desc,
  work,
  note,
  new_code,
  out_type,
  sick_subset,
  cal_week,
  cal_month,
  cal_year,
  maximum_gap_2015,
  gap_end
)]
attr(pay$work_date, 'format.stata') <- '%tc'
attr(pay$cal_week, 'format.stata') <- '%tw'
attr(pay$cal_month, 'format.stata') <- '%tm'
write_dta(pay, pay_path, version = 14)
log_message(paste('Saved', pay_path))
log_complete(success = TRUE)







