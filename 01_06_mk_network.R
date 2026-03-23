#' =============================================================================
#' CONSTRUCT OFFICER NETWORK PANEL
#' =============================================================================
#' Builds the officer-day panel with network exposure measures, wheel-weighted
#' degree centrality, and expected earnings. Parameterized by rolling window
#' size so one script produces all variants (30, 90, 180 day).
#'
#' Input:  data/01_05_pre_network_{window}.csv (from 01_05)
#' Output: data/01_06_panel_working{_window}.rds
#'
#' Output Schema:
#'   num_emp1:                officer id (integer)
#'   analysis_workdate:       date
#'   degree:                  unweighted network degree (same-day)
#'   l_degree:                lagged degree (prior day)
#'   wheel_degree:            wheel-proximity-weighted degree (same-day)
#'   l_wheel_degree:          lagged wheel degree
#'   wheel_nothave_degree:    inverse-wheel-weighted degree (same-day)
#'   l_wheel_nothave_degree:  lagged inverse-wheel degree
#'   dist_from_med:           angular distance from leave-one-out circular median
#'   mean_circ, med_circ, med_circ_exc, mean_circ2: circular statistics
#'   max_rank_date:           max seniority rank on date
#'   avg_othours_conditional: average OT hours among those who work OT
#'   expected_earnings:       avg_othours_conditional * max_rate * ot_premium
#'   + all columns from pre_network CSV (tot_hours, leave_hours, etc.)
#'
#' Usage:
#'   source('config.R')
#'   NETWORK_WINDOW <- 90   # set before sourcing, or defaults to CONFIG value
#'   source('01_06_mk_network.R')
#' =============================================================================

library('data.table')
library('lubridate')
library('circular')
library('ClusTorus')

source('config.R')
source('utils/logging.R')

## Determine which window size to build
if (!exists("NETWORK_WINDOW")) {
  NETWORK_WINDOW <- CONFIG$network_window_default
}

input_path <- get_network_input_path(NETWORK_WINDOW)
output_path <- get_network_output_path(NETWORK_WINDOW)

log_init(paste0("01_06_mk_network_", NETWORK_WINDOW, ".R"))
log_message(paste("Building network panel for", NETWORK_WINDOW, "day window"))
log_message(paste("Input:", input_path))
log_message(paste("Output:", output_path))

#' -----------------------------------------------------------------------------
#' HELPER FUNCTIONS: Circular statistics
#' -----------------------------------------------------------------------------

## Circular mean via atan2 (returns scalar for workers only)
circ_mean <- function(x, w) {
  d <- x[w == 1] / max(x) * 2 * pi
  return(atan2(sum(sin(d)), sum(cos(d))))
}

## Circular mean via circular package
circ_mean2 <- function(x, w) {
  d <- x[w == 1] / max(x) * 2 * pi
  d <- circular(d, units = 'radians', rotation = 'clock',
                zero = 0, modulo = '2pi')
  return(mean(d))
}

## Circular median via circular package
circ_median2 <- function(x, w) {
  d <- x[w == 1] / max(x) * 2 * pi
  d <- circular(d, units = 'radians', rotation = 'clock',
                zero = 0, modulo = '2pi')
  return(median(d))
}

## Leave-one-out circular median (returns vector, one per observation)
circ_median2_exc <- function(z, w) {
  return(sapply(1:length(z), function(x) {
    d <- z[w == 1 & 1:length(z) != x] / max(z) * 2 * pi
    d <- circular(d, units = 'radians', rotation = 'clock',
                  zero = 0, modulo = '2pi')
    return(median(d))
  }))
}

## Element-wise multiplication of left and right halves of a matrix
fold_mult <- function(x) {
  x[is.na(x)] <- 0
  x[, 1:(ncol(x) / 2)] * x[, (ncol(x) / 2 + 1):ncol(x)]
}

#' -----------------------------------------------------------------------------
#' LOAD AND CLEAN DATA
#' -----------------------------------------------------------------------------

assert_required_files(input_path)
data <- fread(input_path)
data[, analysis_workdate := dmy(analysis_workdate)]
data[, num_emp1 := as.integer(gsub(CONFIG$employee_name_pattern, "", employee_name))]

log_message(paste("Loaded", nrow(data), "rows,", uniqueN(data$num_emp1), "officers"))

#' -----------------------------------------------------------------------------
#' EXCLUDE LARGE GAPS
#' -----------------------------------------------------------------------------
## Identify contiguous active spells and measure gap lengths.
## Drop observation-days within gaps >= CONFIG$gap_threshold days.

setkey(data, "num_emp1", "analysis_workdate")
data[, temp_counter := cumsum(tot_hours > 0 | leave_hours > 0), by = "num_emp1"]
data[, temp_gap_length := sum(tot_hours == 0 & leave_hours == 0), by = c("temp_counter", "num_emp1")]

rows_before <- nrow(data)
data <- data[!(tot_hours == 0 & leave_hours == 0 & temp_gap_length >= CONFIG$gap_threshold), ]
log_message(paste("Dropped", rows_before - nrow(data), "rows in gaps >=", CONFIG$gap_threshold, "days"))

data[, c("temp_counter", "temp_gap_length") := NULL]

#' -----------------------------------------------------------------------------
#' CONSTRUCT ALL-PAIRS CONTACT MATRIX
#' -----------------------------------------------------------------------------

data[, ot_work := varot_hours > 0]
data[, normal_work := varstandard_hours > 0]
data[, tot_ot := sum(ot_work), by = "analysis_workdate"]

## Melt exposure columns to long format
melt_data <- melt(data, id.vars = c("num_emp1", "analysis_workdate"),
                  measure.vars = colnames(data)[grep(CONFIG$exposure_pattern, colnames(data))])
melt_data[, num_emp2 := as.integer(gsub(CONFIG$exposure_pattern, "", variable))]

## Create all pairs (complete cross-join per date)
all_pairs <- data[, do.call(CJ, list(num_emp1, num_emp1)), by = c("analysis_workdate")]
setnames(all_pairs, old = c("V1", "V2"), new = c("num_emp1", "num_emp2"))

## Merge exposure values; missing = 0, self-links = 0
all_pairs <- merge(all_pairs, melt_data, all.x = TRUE, by = c("num_emp1", "num_emp2", "analysis_workdate"))
all_pairs[is.na(value), value := 0]
all_pairs[num_emp1 == num_emp2, value := 0]
setkey(all_pairs, "analysis_workdate", "num_emp1", "num_emp2")

## Reshape to wide: one column per potential contact
all_pairs <- dcast(all_pairs, analysis_workdate + num_emp1 ~ num_emp2, value.var = "value")

## Merge back officer-level attributes
merge_dt <- data[, .SD, .SDcols = CONFIG$merge_vars]
all_pairs <- merge(all_pairs, merge_dt, by = c("num_emp1", "analysis_workdate"), all.x = TRUE)

log_message(paste("All-pairs matrix:", nrow(all_pairs), "rows"))

#' -----------------------------------------------------------------------------
#' COMPUTE DEGREE CENTRALITY
#' -----------------------------------------------------------------------------

all_pairs[, day := weekdays(analysis_workdate)]
all_pairs[, dw := as.factor(weekdays(analysis_workdate))]
all_pairs[, month := month(analysis_workdate)]

## Identify numeric employee-ID columns (the contact matrix)
emp_cols <- colnames(all_pairs)[grep("^[0-9]+$", colnames(all_pairs))]

## Verify own-diagonal is 0
for (x in unique(all_pairs$num_emp1)) {
  y <- as.character(x)
  stopifnot(all(all_pairs[num_emp1 == x, .SD, .SDcols = y] == 0))
}

## Unweighted degree = row sum of contact matrix
check <- all_pairs[, .SD, .SDcols = emp_cols]
all_pairs[, degree := rowSums(check, na.rm = TRUE)]

## Lag degree to prior day
setkey(all_pairs, "num_emp1", "analysis_workdate")
all_pairs[, l_degree := shift(degree), by = "num_emp1"]

setorder(all_pairs, "num_emp1", "analysis_workdate")
all_pairs[, first := (1:.N) == 1, by = "num_emp1"]
stopifnot(all(all_pairs[is.na(l_degree), ]$first))
all_pairs[is.na(l_degree) & first == TRUE, l_degree := 0]
stopifnot(!is.na(all_pairs$l_degree))

log_message("Degree centrality computed and lagged")

#' -----------------------------------------------------------------------------
#' CIRCULAR STATISTICS FOR WHEEL POSITION
#' -----------------------------------------------------------------------------

all_pairs[, mean_circ := circ_mean(seniority_rank, ot_work), by = "analysis_workdate"]
all_pairs[, mean_circ := max(mean_circ, na.rm = TRUE), by = "analysis_workdate"]

all_pairs[, med_circ := circ_median2(seniority_rank, ot_work), by = "analysis_workdate"]
all_pairs[, med_circ := max(med_circ, na.rm = TRUE), by = "analysis_workdate"]

all_pairs[, med_circ_exc := circ_median2_exc(seniority_rank, ot_work), by = "analysis_workdate"]
stopifnot(!is.na(all_pairs$med_circ_exc))

all_pairs[, mean_circ2 := circ_mean2(seniority_rank, ot_work), by = "analysis_workdate"]
all_pairs[, mean_circ2 := max(mean_circ2, na.rm = TRUE), by = "analysis_workdate"]

log_message("Circular statistics computed")

#' -----------------------------------------------------------------------------
#' WHEEL-WEIGHTED DEGREE (proximity to wheel position)
#' -----------------------------------------------------------------------------

## Angular distance from leave-one-out median wheel position
all_pairs[, dist_from_med := 1 - ang.dist(seniority_rank / max(seniority_rank) * 2 * pi, med_circ_exc) / pi, by = "analysis_workdate"]

## Lead the angular distance (timing: wheel position is day-of, contact is day-before)
setkey(all_pairs, "num_emp1", "analysis_workdate")
all_pairs[, lead_dist_from_med := shift(dist_from_med, n = 1, fill = NA, type = "lead"), by = "num_emp1"]

## Construct wheel-weight matrix: one column per contact's lead_dist_from_med
temp <- dcast(all_pairs, analysis_workdate ~ num_emp1, value.var = "lead_dist_from_med")
stopifnot(colnames(temp)[-1] == emp_cols)
colnames(temp)[-1] <- paste0("wheel_", colnames(temp)[-1])
all_pairs <- merge(all_pairs, temp, by = "analysis_workdate", all.x = TRUE)

## Multiply contact matrix by wheel weights (using helper_ prefix to preserve originals)
helper_cols <- paste0("helper_", emp_cols)
wheel_temp_cols <- colnames(temp)[-1]
all_pairs[, (helper_cols) := fold_mult(.SD), .SDcols = c(emp_cols, wheel_temp_cols)]

## Verify self-links still 0 in weighted matrix
for (x in unique(all_pairs$num_emp1)) {
  y <- paste0("helper_", as.character(x))
  stopifnot(all(all_pairs[num_emp1 == x, .SD, .SDcols = y] == 0, na.rm = TRUE))
}

## Wheel-weighted degree = row sum of weighted contact matrix
check <- all_pairs[, .SD, .SDcols = helper_cols]
all_pairs[, wheel_degree := rowSums(check, na.rm = TRUE)]

## Lag wheel degree
setkey(all_pairs, "num_emp1", "analysis_workdate")
all_pairs[, l_wheel_degree := shift(wheel_degree), by = "num_emp1"]
stopifnot(all(all_pairs[is.na(l_wheel_degree), ]$first))
all_pairs[is.na(l_wheel_degree) & first == TRUE, l_wheel_degree := 0]
stopifnot(!is.na(all_pairs$l_wheel_degree))

## Clean up helper columns and wheel temp columns
all_pairs[, (helper_cols) := NULL]
all_pairs[, (wheel_temp_cols) := NULL]

log_message("Wheel-weighted degree computed")

#' -----------------------------------------------------------------------------
#' INVERSE-WHEEL-WEIGHTED DEGREE (distance FROM wheel = 1 - proximity)
#' -----------------------------------------------------------------------------

all_pairs[, nothave_dist := 1 - dist_from_med]

setkey(all_pairs, "num_emp1", "analysis_workdate")
all_pairs[, lead_nothave_dist := shift(nothave_dist, n = 1, fill = NA, type = "lead"), by = "num_emp1"]

temp <- dcast(all_pairs, analysis_workdate ~ num_emp1, value.var = "lead_nothave_dist")
stopifnot(colnames(temp)[-1] == emp_cols)
colnames(temp)[-1] <- paste0("wheel_nothave_", colnames(temp)[-1])
all_pairs <- merge(all_pairs, temp, by = "analysis_workdate", all.x = TRUE)

helper_nothave_cols <- paste0("helper_nothave_", emp_cols)
nothave_temp_cols <- colnames(temp)[-1]
all_pairs[, (helper_nothave_cols) := fold_mult(.SD), .SDcols = c(emp_cols, nothave_temp_cols)]

for (x in unique(all_pairs$num_emp1)) {
  y <- paste0("helper_nothave_", as.character(x))
  stopifnot(all(all_pairs[num_emp1 == x, .SD, .SDcols = y] == 0, na.rm = TRUE))
}

check <- all_pairs[, .SD, .SDcols = helper_nothave_cols]
all_pairs[, wheel_nothave_degree := rowSums(check, na.rm = TRUE)]

setkey(all_pairs, "num_emp1", "analysis_workdate")
all_pairs[, l_wheel_nothave_degree := shift(wheel_nothave_degree), by = "num_emp1"]
stopifnot(all(all_pairs[is.na(l_wheel_nothave_degree), ]$first))
all_pairs[is.na(l_wheel_nothave_degree) & first == TRUE, l_wheel_nothave_degree := 0]
stopifnot(!is.na(all_pairs$l_wheel_nothave_degree))

## Clean up
all_pairs[, (helper_nothave_cols) := NULL]
all_pairs[, (nothave_temp_cols) := NULL]

log_message("Inverse-wheel-weighted degree computed")

#' -----------------------------------------------------------------------------
#' EXPECTED EARNINGS
#' -----------------------------------------------------------------------------

all_pairs[, max_rank_date := max(seniority_rank), by = "analysis_workdate"]
all_pairs[, avg_othours_conditional := sum(varot_hours) / sum(ot_work), by = "analysis_workdate"]
all_pairs[, expected_earnings := avg_othours_conditional * max_rate * CONFIG$ot_premium]

#' -----------------------------------------------------------------------------
#' SAVE OUTPUT
#' -----------------------------------------------------------------------------

if (CONFIG$verbose_logging) {
  message("Output dimensions: ", nrow(all_pairs), " x ", ncol(all_pairs))
}

ensure_directory(dirname(output_path))
saveRDS(all_pairs, output_path)
log_message(paste("Saved:", output_path))
log_complete(success = TRUE)

message("01_06_mk_network complete for window = ", NETWORK_WINDOW)
