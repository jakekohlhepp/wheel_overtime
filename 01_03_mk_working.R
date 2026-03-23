#' =============================================================================
#' BUILD WORKING DATASETS FROM RAW PAY/PERSONNEL RECORDS
#' =============================================================================
#' Reads the anonymized Stata payroll file (raw data, fixed format) and splits
#' it into three typed datasets used by every downstream script:
#'
#'   employee_data   – one row per employee, demographic/status fields
#'   workers_comp    – one row per injury claim (doi non-missing)
#'   pay_data        – one row per pay line item, enriched with pay-code flags,
#'                     calendar identifiers, and leave-gap measures
#'
#' Input:  20170803_payworkers_comp/data/anonymized_data_073117.dta  (raw)
#'         out/list_var_desc.csv  (pay-code classification table)
#' Output: data/employee_data.rds
#'         data/workers_comp.rds
#'         data/pay_data.rds
#' =============================================================================

library('data.table')
library('haven')

source('config.R')
source('utils/logging.R')

log_init('01_03_mk_working.R')

raw_path      <- file.path(CONFIG$raw_pay_dir, 'data', 'anonymized_data_073117.dta')
info_path     <- file.path(CONFIG$output_dir, 'list_var_desc.csv')
employee_path <- file.path(CONFIG$data_dir, 'employee_data.rds')
workers_path  <- file.path(CONFIG$data_dir, 'workers_comp.rds')
pay_path      <- file.path(CONFIG$data_dir, 'pay_data.rds')

assert_required_files(c(raw_path, info_path))
ensure_directory(CONFIG$data_dir)

## Returns TRUE if all values in x are equal (or all NA), used to assert
## that "stable" fields never vary within an employee
constant_within_group <- function(x) {
  ref <- x[1]
  all((x == ref) | (is.na(x) & is.na(ref)))
}

####  LOAD RAW DATA  ###########################################################

raw <- as.data.table(read_dta(raw_path))
## v1 is the unique row identifier in the raw file
stopifnot(nrow(raw) == uniqueN(raw$v1))

## These fields should be constant within employee; if they vary the raw data
## has a problem we need to investigate before proceeding
stable_cols <- c('job_status', 'totalnumberofinjuries', 'original_hire_date',
                 'civilian_entry_date', 'job_end_date', 'job_class_title',
                 'job_class', 'jobclassdescription')

setorder(raw, employee_name, v1)
for (col in stable_cols) {
  stopifnot(raw[, constant_within_group(get(col)), by = employee_name][, all(V1)])
}

####  EMPLOYEE DATA  ###########################################################

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

saveRDS(employee_data, employee_path)
log_message(paste('Saved', employee_path))

####  WORKERS COMP  ############################################################

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

## doi and timeofinj are always jointly present or jointly missing
stopifnot(all(!is.na(workers_comp$doi[!is.na(workers_comp$timeofinj)])))
stopifnot(all(!is.na(workers_comp$timeofinj[!is.na(workers_comp$doi)])))
## Every actual injury claim must have a nature description and a doi
stopifnot(all(nzchar(workers_comp$natureofinjury[!is.na(workers_comp$doi)])))
stopifnot(all(!is.na(workers_comp$doi[nzchar(workers_comp$natureofinjury)])))

## Keep only actual injury claims: the raw file contains one row per pay record
## for every employee, so most rows have no doi. Rows without doi are not claims.
workers_comp <- workers_comp[!is.na(doi)]
## medpd is stored as a formatted string in the raw data; convert to numeric
workers_comp[, medpd := as.numeric(gsub(',', '', medpd, fixed = TRUE))]

saveRDS(workers_comp, workers_path)
log_message(paste('Saved', workers_path))

####  PAY DATA  ################################################################

## Load the pay-code classification table; maps variation_description to flags
## (work, new_code, out_type, sick_subset) used to categorise each pay line
info <- fread(info_path)
stopifnot('variation_description' %in% names(info))
## Drop rows with no pay-code label: they cannot be matched to pay lines and
## would cause a downstream merge failure on cleaned_variation_desc
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

## Every pay code must appear in the classification table
stopifnot(all(pay$cleaned_variation_desc %chin% info$cleaned_variation_desc))
pay <- merge(pay, info, by = 'cleaned_variation_desc', all.x = TRUE, sort = FALSE)

pay_dates <- as.Date(pay$work_date)
pay[, cal_week  := stata_week_num(pay_dates)]
pay[, cal_month := stata_month_num(pay_dates)]
pay[, cal_year  := as.integer(format(pay_dates, '%Y'))]

## Compute the largest gap between consecutive work dates after the injury
## window start (2015-01-01) for each employee. Used downstream to identify
## employees who effectively left during the analysis window.
pay[, temp_date   := pay_dates]
pay[, temp_period := temp_date >= CONFIG$injury_window_start]
setorder(pay, employee_name, temp_period, temp_date)
pay[, temp_gap := as.numeric(temp_date - shift(temp_date)), by = .(employee_name, temp_period)]
pay[is.na(temp_gap), temp_gap := 0]
pay[temp_period == FALSE, temp_gap := 0]
pay[, maximum_gap_2015 := {
  keep <- temp_date >= CONFIG$injury_window_start
  out  <- rep(0, .N)
  if (any(keep)) {
    out[keep] <- max(temp_gap[keep], na.rm = TRUE)
  }
  out
}, by = employee_name]
setorderv(pay, c('employee_name', 'maximum_gap_2015', 'temp_date'), order = c(1L, -1L, 1L))
## Broadcast the maximum gap to every row for that employee
pay[, maximum_gap_2015 := maximum_gap_2015[1], by = employee_name]

## gap_end_date is the first work date that ends a gap >= gap_group_days
## (i.e., the date the employee came back after a long absence)
pay[, gap_end_date := as.Date(NA)]
pay[temp_gap >= CONFIG$gap_group_days, gap_end_date := temp_date]
pay[, gap_end_date := {
  if (all(is.na(gap_end_date))) {
    rep(as.Date(NA), .N)
  } else {
    rep(gap_end_date[which(!is.na(gap_end_date))[1]], .N)
  }
}, by = .(employee_name, work_date)]
## Convert gap_end_date to Stata day number so downstream Stata-era merges work
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

saveRDS(pay, pay_path)
log_message(paste('Saved', pay_path))
log_complete(success = TRUE)
