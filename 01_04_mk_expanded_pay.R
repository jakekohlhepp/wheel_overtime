#' =============================================================================
#' BUILD EXPANDED DAILY OFFICER PANEL
#' =============================================================================
#' Creates the core officer-day panel used by all downstream analysis scripts.
#' Starting from the pay line items (01_03), this script:
#'   1. Computes daily-level hours, pay, and leave totals per employee
#'   2. Identifies termination dates from pay codes and status fields
#'   3. Expands each employee's record to every calendar day in their window
#'   4. Merges in injury claims, weather, and holiday indicators
#'   5. Builds running cumulative-hours totals
#'
#' A separate narrow extract (fornetwork) is saved for the network-construction
#' step (01_05/01_06): it contains only working days with a valid division code.
#'
#' Input:  data/pay_data.rds          (from 01_03)
#'         data/workers_comp.rds      (from 01_03)
#'         data/employee_data.rds     (from 01_03)
#'         data/weather_daily.rds     (from 01_01)
#'         data/holidays.rds          (from 01_02)
#'         20170803_payworkers_comp/data/anonymized_data_073117.dta  (raw)
#' Output: data/01_04_fornetwork.rds
#'         data/working_expanded.rds
#' =============================================================================

library('data.table')
library('haven')
library('zoo')

source('config.R')
source('utils/logging.R')

log_init('01_04_mk_expanded_pay.R')

pay_path       <- file.path(CONFIG$data_dir, 'pay_data.rds')
workers_path   <- file.path(CONFIG$data_dir, 'workers_comp.rds')
employee_path  <- file.path(CONFIG$data_dir, 'employee_data.rds')
weather_path   <- file.path(CONFIG$data_dir, 'weather_daily.rds')
holidays_path  <- file.path(CONFIG$data_dir, 'holidays.rds')
fornetwork_path <- file.path(CONFIG$data_dir, '01_04_fornetwork.rds')
working_path   <- file.path(CONFIG$data_dir, 'working_expanded.rds')
raw_path       <- file.path(CONFIG$raw_pay_dir, 'data', 'anonymized_data_073117.dta')

assert_required_files(c(pay_path, workers_path, employee_path, weather_path, holidays_path, raw_path))
ensure_directory(CONFIG$data_dir)
ensure_directory(CONFIG$output_dir)

####  HELPER FUNCTIONS  ########################################################

## Return the first non-NA element of x, preserving type
first_non_missing <- function(x) {
  keep <- which(!is.na(x))
  if (length(keep) == 0) {
    if (inherits(x, 'Date'))   return(as.Date(NA))
    if (inherits(x, 'POSIXct')) return(as.POSIXct(NA, origin = '1970-01-01', tz = 'UTC'))
    if (is.character(x))       return(NA_character_)
    if (is.integer(x))         return(NA_integer_)
    if (is.numeric(x))         return(NA_real_)
    return(NA)
  }
  x[keep[1]]
}

sum_na0 <- function(x) sum(x, na.rm = TRUE)

## Aggregate a data.table subset: sum over sum_cols, first-non-NA over first_cols
collapse_sum_first <- function(dt, by_cols, sum_cols, first_cols = character()) {
  sum_list   <- setNames(lapply(sum_cols,   function(col) sum_na0(dt[[col]])),         sum_cols)
  first_list <- setNames(lapply(first_cols, function(col) first_non_missing(dt[[col]])), first_cols)
  c(sum_list, first_list)
}

## Returns TRUE when all values in x are equal (or all NA); used for group-level
## assertions that a field should never vary within (employee, date)
constant_within_group <- function(x) {
  ref <- x[1]
  all((x == ref) | (is.na(x) & is.na(ref)))
}

####  SUMMARY COUNTS (diagnostic CSVs)  #######################################

pay          <- readRDS(pay_path)
workers_comp <- readRDS(workers_path)
employee_data <- readRDS(employee_path)

pay_dates <- as.Date(pay$work_date)
pay_year  <- as.integer(format(pay_dates, '%Y'))
pay_counts <- copy(pay)
pay_counts[, year            := as.character(pay_year)]
pay_counts[, unique_emps     := as.numeric(seq_len(.N) == 1), by = .(employee_name, year)]
pay_counts[, unique_codes    := as.numeric(seq_len(.N) == 1), by = .(variation_description, year)]
pay_counts[, unique_persondays := as.numeric(seq_len(.N) == 1), by = .(employee_name, work_date)]
all_counts <- copy(pay_counts)
pay_counts[, year         := 'Overall']
pay_counts[, unique_emps  := NA_real_]
pay_counts[, unique_codes := NULL]
pay_counts[, unique_codes := as.numeric(seq_len(.N) == 1), by = variation_description]
pay_counts[, unique_emps  := as.numeric(seq_len(.N) == 1), by = employee_name]
pay_counts <- rbindlist(list(pay_counts, all_counts), use.names = TRUE, fill = TRUE)
pay_counts[, recs := 1]
pay_counts <- pay_counts[, .(
  recs             = .N,
  unique_persondays = sum(unique_persondays, na.rm = TRUE),
  unique_emps      = sum(unique_emps, na.rm = TRUE),
  unique_codes     = sum(unique_codes, na.rm = TRUE)
), by = year]
fwrite(pay_counts, file.path(CONFIG$output_dir, '01_04_work_data_counts.csv'))

nature_counts <- copy(workers_comp)
nature_all    <- copy(nature_counts)
nature_all[, natureofinjury := 'All Types']
nature_counts <- rbindlist(list(nature_all, nature_counts), use.names = TRUE, fill = TRUE)
nature_counts[, emp_count    := 1]
nature_counts[, natureofinjury := gsub('Multiple', 'Mult',      natureofinjury, fixed = TRUE)]
nature_counts[, natureofinjury := gsub('Incl',     'Include',   natureofinjury, fixed = TRUE)]
nature_counts[, natureofinjury := gsub(' (e.g., ', '',          natureofinjury, fixed = TRUE)]
nature_counts[, natureofinjury := gsub(',',        '',          natureofinjury, fixed = TRUE)]
nature_counts <- nature_counts[, .(emp_count = .N), by = natureofinjury]
nature_counts[, last := natureofinjury == 'All Types']
setorder(nature_counts, last, natureofinjury)
nature_counts[, percent := emp_count / emp_count[.N]]
nature_counts[, ovrl := natureofinjury == 'All Types']
setorder(nature_counts, -ovrl, -emp_count)
nature_counts[, ovrl := NULL]
fwrite(nature_counts, file.path(CONFIG$output_dir, '01_04_nature.csv'))

cause_counts <- copy(workers_comp)
cause_all    <- copy(cause_counts)
cause_all[, claimcausegroup := 'All Types']
cause_counts <- rbindlist(list(cause_all, cause_counts), use.names = TRUE, fill = TRUE)
cause_counts[, emp_count := 1]
cause_counts <- cause_counts[, .(emp_count = .N), by = claimcausegroup]
cause_counts[, last := claimcausegroup == 'All Types']
setorder(cause_counts, last, claimcausegroup)
cause_counts[, percent := emp_count / emp_count[.N]]
cause_counts[, ovrl := claimcausegroup == 'All Types']
setorder(cause_counts, -ovrl, -emp_count)
cause_counts[, ovrl := NULL]
fwrite(cause_counts, file.path(CONFIG$output_dir, '01_04_cause.csv'))

####  TERMINATION DATES  ######################################################

## Two sources of termination: (1) job_status field in the raw .dta, and
## (2) pay lines that contain "TERM" in their variation_description.
## We take the earliest date from either source as the term date.
raw <- as.data.table(read_dta(raw_path))
stopifnot(all(is.na(raw$job_end_date)))
status_terms <- unique(raw[job_status %chin% c('Transferred Out', 'Terminated'), .(
  employee_name,
  analysis_workdate = as.Date(job_status_date)
)])

term_codes <- unique(pay[grepl('TERM', variation_description, fixed = TRUE), .(
  employee_name,
  analysis_workdate = as.Date(work_date),
  variation_description
)])
term_codes <- term_codes[, .(analysis_workdate = min(analysis_workdate)), by = employee_name]
terms <- unique(rbindlist(list(term_codes[, .(employee_name, analysis_workdate)], status_terms),
                          use.names = TRUE, fill = TRUE))
## Ignore any termination signal before the panel starts: these officers have
## already exited and are never entered in the expanded panel anyway
terms <- terms[analysis_workdate >= CONFIG$fiscal_year_start]
setorder(terms, employee_name, analysis_workdate)
## Both sources should agree to within a year; large discrepancies signal bad data
terms[, diff := as.numeric(analysis_workdate[.N] - analysis_workdate[1]), by = employee_name]
stopifnot(all(terms$diff <= 365))
terms <- terms[, .(term_date = analysis_workdate[1]), by = employee_name]

####  DAILY PAY AGGREGATION  ##################################################

## Drop all pay lines that are not actual labor supply: the raw file contains
## administrative records (retro adjustments, reimbursements, bonuses, etc.)
## that do not represent time worked. Keep only:
##   work == 1            : flagged as a working pay code in the classification table
##   IOD in description   : injury-on-duty marker (officer was working when injured)
##   out_type == 1        : paid leave drawn from the officer's leave balance
## Everything else (e.g., expense reimbursements, retroactive pay corrections) is dropped.
is_work       <- !is.na(pay$work) & pay$work == 1
is_iod_marker <- !is.na(pay$variation_description) &
  grepl(' IOD ', pay$variation_description, fixed = TRUE)
is_out_type1  <- !is.na(pay$out_type) & pay$out_type == 1

work_keep <- pay[is_work | is_iod_marker | is_out_type1]
work_keep[, test_rate        := pay_amount / hours]
work_keep[, is_work          := !is.na(work) & work == 1]
## Zero-rate work lines are data artefacts (e.g., retroactive corrections); drop them
work_keep[, drop_zero_work_rate := !is.na(test_rate) & test_rate == 0 & is_work]
work_keep <- work_keep[drop_zero_work_rate == FALSE]
work_keep[, drop_zero_work_rate := NULL]
work_keep[, iod_flag := fifelse(!is.na(variation_description) & grepl('IOD', variation_description, fixed = TRUE), 1, NA_real_)]
## dept must be constant within (employee, date); variation here means a data issue
stopifnot(work_keep[, constant_within_group(dept), by = .(employee_name, work_date)][, all(V1)])

work_keep[, is_overtime      := !is.na(variation_description) &
  grepl('overtime', tolower(variation_description), fixed = TRUE)]
work_keep[, varot_hours      := fifelse(is_overtime & is_work, hours, 0)]
work_keep[, varstandard_hours := fifelse(!is_overtime, hours, 0)]
## "types" collapses all leave flavours so we can aggregate hours correctly below
work_keep[, types := fifelse(is_work | iod_flag == 1, 'not leave', NA_character_)]
work_keep[!is.na(out_type) & out_type == 1, types := cleaned_variation_desc]
stopifnot(all(!is.na(work_keep$types)))
stopifnot(all(work_keep[!is.na(work) & work == 1, var_rate] < CONFIG$max_work_rate))
work_keep[, max_rate := max(var_rate), by = .(employee_name, work_date)]
stopifnot(all(work_keep$max_rate >= 0))
## Sentinel value -99 flags rate above max_work_rate (cleaned later with locf)
work_keep[max_rate > CONFIG$max_work_rate, max_rate := -99]
work_keep[, ot_pay_amount   := pay_amount * as.numeric(is_overtime)]
work_keep[, work_pay_amount := pay_amount * as.numeric(is_work)]
work_keep[, c('is_work', 'is_overtime') := NULL]
## Division codes below 800 identify administrative or non-geographic units
## (e.g., HQ, training, admin leave pools). Officers assigned to these codes
## cannot be placed in a physical division for network construction, so their
## division is set to NA. Their hours still count; they just won't appear in
## the fornetwork extract and thus won't contribute to exposure matrices.
setnames(work_keep, 'div', 'geo_div')
work_keep[!is.na(geo_div) & geo_div < 800, geo_div := NA_real_]

## First collapse: sum hours within (employee, date, dept, type)
work_day <- work_keep[, collapse_sum_first(.SD,
  by_cols   = c('employee_name', 'work_date', 'dept', 'yearsoldonworkdate', 'geo_div', 'types', 'sick_subset', 'maximum_gap_2015', 'max_rate'),
  sum_cols  = c('hours', 'varstandard_hours', 'varot_hours', 'work_pay_amount', 'ot_pay_amount'),
  first_cols = c('iod_flag')
), by = .(employee_name, work_date, dept, yearsoldonworkdate, geo_div, types, sick_subset, maximum_gap_2015, max_rate)]
setnames(work_day, 'hours', 'tot_hours')
## Leave hours should not be negative; zero them so summing doesn't subtract from total
work_day[tot_hours < 0 & types != 'not leave', tot_hours := 0]
work_day[types != 'not leave', types := 'leave']

## Second collapse: merge leave/work rows within (employee, date, dept)
work_day <- work_day[, collapse_sum_first(.SD,
  by_cols   = c('employee_name', 'work_date', 'dept', 'yearsoldonworkdate', 'geo_div', 'types', 'sick_subset', 'maximum_gap_2015', 'max_rate'),
  sum_cols  = c('tot_hours', 'varstandard_hours', 'varot_hours', 'work_pay_amount', 'ot_pay_amount'),
  first_cols = c('iod_flag')
), by = .(employee_name, work_date, dept, yearsoldonworkdate, geo_div, types, sick_subset, maximum_gap_2015, max_rate)]

work_day[, leave_hours := fifelse(types == 'leave', tot_hours, 0)]
work_day[, sick_hours  := fifelse(!is.na(sick_subset) & sick_subset == 1, tot_hours, 0)]
work_day[types == 'leave', tot_hours := 0]

## Third collapse: sum across leave/work within (employee, date) so we have
## one row per person-day with separate leave and work hour columns
work_day <- work_day[, collapse_sum_first(.SD,
  by_cols   = c('employee_name', 'work_date', 'dept', 'yearsoldonworkdate', 'geo_div', 'maximum_gap_2015', 'max_rate'),
  sum_cols  = c('tot_hours', 'leave_hours', 'sick_hours', 'varstandard_hours', 'varot_hours', 'work_pay_amount', 'ot_pay_amount'),
  first_cols = c('iod_flag')
), by = .(employee_name, work_date, dept, yearsoldonworkdate, geo_div, maximum_gap_2015, max_rate)]

####  FORNETWORK EXTRACT  ######################################################

## Keep only days where the officer actually worked (positive standard or OT
## hours) and was assigned to a mappable geographic division. Leave-only days
## and admin-division days are excluded because they contribute no co-worker
## exposure that can be used in the network construction in 01_05/01_06.
fornetwork <- work_day[(varstandard_hours > 0 | varot_hours > 0) & !is.na(geo_div), .(
  employee_name,
  geo_div,
  analysis_workdate = as.Date(work_date)
)]
saveRDS(fornetwork, fornetwork_path)
log_message(paste('Saved', fornetwork_path))

####  DIVISION ASSIGNMENT (up to 2 divs per employee-day)  ####################

## Some employees work in two divisions on the same day; keep at most 2,
## ordered by tot_hours ascending so the primary division (more hours) is div1
work_day[, geo_div_sort := fifelse(is.na(geo_div), Inf, geo_div)]
setorder(work_day, employee_name, work_date, tot_hours, geo_div_sort)
work_day[, geo_div_sort := NULL]
stopifnot(work_day[, max(.N), by = .(employee_name, work_date)][, max(V1)] <= 2)
work_day[, `:=`(
  div1     = geo_div[1],
  div2     = if (.N >= 2) geo_div[2]     else NA_real_,
  lhours_1 = leave_hours[1],
  lhours_2 = if (.N >= 2) leave_hours[2] else NA_real_,
  shours_1 = sick_hours[1],
  shours_2 = if (.N >= 2) sick_hours[2]  else NA_real_
), by = .(employee_name, work_date)]

work_day <- work_day[, collapse_sum_first(.SD,
  by_cols   = c('employee_name', 'work_date', 'dept', 'yearsoldonworkdate', 'div1', 'div2', 'maximum_gap_2015', 'max_rate', 'lhours_1', 'lhours_2', 'shours_1', 'shours_2'),
  sum_cols  = c('leave_hours', 'tot_hours', 'sick_hours', 'varstandard_hours', 'varot_hours', 'work_pay_amount', 'ot_pay_amount'),
  first_cols = c('iod_flag')
), by = .(employee_name, work_date, dept, yearsoldonworkdate, div1, div2, maximum_gap_2015, max_rate, lhours_1, lhours_2, shours_1, shours_2)]

setorder(work_day, employee_name, work_date)
stopifnot(nrow(work_day) == uniqueN(work_day[, .(employee_name, work_date)]))
stopifnot(all(work_day$max_rate <= CONFIG$max_work_rate))
## Replace sentinel -99 with NA, then forward-fill to propagate the last known rate
work_day[max_rate == -99, max_rate := NA_real_]
work_day[, max_rate := na.locf(max_rate, na.rm = FALSE), by = employee_name]
stopifnot(all(!is.na(work_day$max_rate)))

work_day[, flag_hours_zeroed := as.numeric(tot_hours < 0)]
work_day[tot_hours < 0, tot_hours := 0]
work_day[, analysis_workdate := as.Date(work_date)]
work_day[, doi := analysis_workdate]

####  MERGE INJURY CLAIMS  ####################################################

workers_keep <- workers_comp[, .(
  employee_name, doi, timeofinj, natureofinjury, bodypart,
  claimcause, claimcausegroup, contribcause, medpd
)]
workers_keep[, using_flag := 1L]
work_day[, master_flag := 1L]
work_merge <- merge(work_day, workers_keep, by = c('employee_name', 'doi'), all = TRUE, sort = FALSE)
work_merge[, merge_code := fifelse(!is.na(master_flag) & !is.na(using_flag), 3L,
                            fifelse(!is.na(master_flag), 1L, 2L))]
## Every employee with an injury claim must also appear in the pay data (merge_code != 2)
stopifnot(work_merge[, any(merge_code != 2L), by = employee_name][, all(V1)])
work_merge[is.na(analysis_workdate), analysis_workdate := doi]
work_merge[, matched_injury := as.numeric(merge_code == 3L)]
work_merge[, c('master_flag', 'using_flag', 'merge_code') := NULL]
## Hour columns are NA for injury-only rows; zero them for safe cumulative sums
for (col in c('tot_hours', 'varstandard_hours', 'varot_hours', 'leave_hours', 'sick_hours')) {
  work_merge[is.na(get(col)), (col) := 0]
}
setorder(work_merge, employee_name, analysis_workdate)

####  EXPAND TO CALENDAR PANEL  ###############################################

## Build the date range for each employee: from their hire date (or fiscal year
## start, whichever is later) to their term date (or fiscal year end)
employee_panel <- copy(employee_data)
employee_panel[, date_start := fifelse(as.Date(original_hire_date) > CONFIG$fiscal_year_start,
                                       as.Date(original_hire_date), CONFIG$fiscal_year_start)]
employee_panel <- unique(employee_panel[, .(employee_name, date_start)])
employee_panel <- merge(employee_panel, terms, by = 'employee_name', all.x = TRUE, sort = FALSE)
stopifnot(all(!is.na(employee_panel$date_start)))
employee_panel[, date_end := fifelse(is.na(term_date) | term_date > CONFIG$fiscal_year_end,
                                     CONFIG$fiscal_year_end, term_date)]
setorder(employee_panel, employee_name)

employee_levels <- sort(unique(employee_panel$employee_name))
emp_labels      <- stats::setNames(seq_along(employee_levels), employee_levels)

expanded_panel <- employee_panel[, .(
  analysis_workdate = seq(min(date_start, date_end), max(date_start, date_end), by = 'day')
), by = employee_name]
expanded_panel[, empid := labelled(match(employee_name, employee_levels), labels = emp_labels)]

work_merge[, data_flag := 1L]
expanded_panel <- merge(expanded_panel, work_merge,
                        by = c('employee_name', 'analysis_workdate'), all.x = TRUE, sort = FALSE)
expanded_panel[, mold := fifelse(!is.na(data_flag), 3L, 1L)]
expanded_panel[, data_flag := NULL]
## Days with no pay record get zero hours (officer was not scheduled / not present)
for (col in c('tot_hours', 'leave_hours', 'sick_hours')) {
  expanded_panel[is.na(get(col)), (col) := 0]
}
expanded_panel[is.na(matched_injury), matched_injury := 0]
setorder(expanded_panel, employee_name, analysis_workdate)
stopifnot(nrow(expanded_panel) == uniqueN(expanded_panel[, .(employee_name, analysis_workdate)]))
## Forward-fill dept and max_rate across zero-hour gap days
expanded_panel[, dept     := na.locf(dept,     na.rm = FALSE), by = employee_name]
expanded_panel[, max_rate := na.locf(max_rate, na.rm = FALSE), by = employee_name]
stopifnot(all(!is.na(expanded_panel$tot_hours)))
expanded_panel[, not_worked := as.numeric(tot_hours == 0)]

## Cumulative hours (standard, OT, total) since employee start
for (col in c('tot_hours', 'varstandard_hours', 'varot_hours')) {
  expanded_panel[is.na(get(col)), (col) := 0]
  expanded_panel[, (paste0('cum_', col)) := cumsum(get(col)), by = employee_name]
}
expanded_panel[, work_date := NULL]

## Merge stable employee-level fields from employee_data
expanded_panel <- merge(expanded_panel, employee_data, by = 'employee_name', all.x = TRUE, sort = FALSE)
stopifnot(all(!is.na(expanded_panel$original_hire_date)))
stopifnot(all(!is.na(expanded_panel$job_status)))

## Drop employees who never appear in the pay records. These are employees
## present in the personnel/HR file but with zero matching rows in the payroll
## extract — likely administrative entries, data entry errors, or employees who
## transferred before any pay records were captured. Keeping them would produce
## an expanded panel of entirely zero-hour rows with no useful signal.
## TODO: Investigate why exactly these 44 appear in the personnel file but not
## payroll; confirm none are officers who worked before the fiscal_year_start
## window (in which case they would be legitimately excluded, not artefacts).
employee_flags <- expanded_panel[, .(all_miss = !any(mold == 3L)), by = employee_name]
log_message(paste('all_miss count', sum(employee_flags$all_miss)))
log_message(paste('all_miss sample', paste(employee_flags[all_miss == TRUE, employee_name], collapse = ', ')))
stopifnot(sum(employee_flags$all_miss) == 44L)
expanded_panel <- expanded_panel[employee_name %chin% employee_flags[all_miss == FALSE, employee_name]]
expanded_panel[, mold := NULL]

expanded_panel[, an_week  := stata_week_num(analysis_workdate)]
expanded_panel[, an_month := stata_month_num(analysis_workdate)]
expanded_panel[, date     := analysis_workdate]

####  MERGE WEATHER AND HOLIDAYS  #############################################

weather  <- readRDS(weather_path)
expanded_panel <- merge(expanded_panel, weather, by = 'date', all.x = TRUE, sort = FALSE)
## Weather must be present for all days in the analysis window
stopifnot(expanded_panel[analysis_workdate >= CONFIG$injury_window_start,
                         all(!is.na(prcp) & !is.na(tmax) & !is.na(tmin))])

holidays <- readRDS(holidays_path)
expanded_panel <- merge(expanded_panel, holidays, by = 'date', all.x = TRUE, sort = FALSE)
expanded_panel[, is_holiday := as.numeric(!is.na(holiday))]
expanded_panel[, date       := NULL]
## rain indicator: NA prcp (outside weather coverage) conservatively treated as rain
expanded_panel[, rain := fifelse(is.na(prcp), 1, as.numeric(prcp > 0))]

####  FINAL COLUMN ORDER AND SAVE  ############################################

setcolorder(expanded_panel, c(
  'employee_name', 'analysis_workdate', 'empid', 'dept', 'yearsoldonworkdate',
  'maximum_gap_2015', 'max_rate', 'div1', 'div2', 'lhours_1', 'lhours_2',
  'shours_1', 'shours_2', 'leave_hours', 'tot_hours', 'sick_hours',
  'varstandard_hours', 'varot_hours', 'work_pay_amount', 'ot_pay_amount',
  'iod_flag', 'flag_hours_zeroed', 'doi', 'timeofinj', 'natureofinjury',
  'bodypart', 'claimcause', 'claimcausegroup', 'contribcause', 'medpd',
  'matched_injury', 'not_worked', 'cum_tot_hours', 'cum_varstandard_hours',
  'cum_varot_hours', 'job_class', 'job_class_title', 'jobclassdescription',
  'civilian_entry_date', 'original_hire_date', 'job_end_date', 'job_status',
  'totalnumberofinjuries', 'an_week', 'an_month', 'prcp', 'tmax', 'tmin',
  'holiday', 'is_holiday', 'rain'
))

saveRDS(expanded_panel, working_path)
log_message(paste('Saved', working_path))
log_complete(success = TRUE)
