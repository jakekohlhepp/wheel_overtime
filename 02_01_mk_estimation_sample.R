#' =============================================================================
#' BUILD ESTIMATION SAMPLE
#' =============================================================================
#' Constructs the main estimation sample from the network panel by:
#'   1. Excluding post-injury days until first return to work
#'   2. Restricting to estimation date range
#'   3. Adding holiday, bereavement, and FMLA flags
#'   4. Creating derived variables (model matrices, interaction terms)
#'
#' Input:  data/01_06_panel_working.rds             (90-day network panel)
#'         20170803_payworkers_comp/anonymized_data_073117.txt (raw pay data)
#' Output: data/02_01_estimation_sample.rds
#'
#' Output Schema:
#'   All columns from network panel, plus:
#'   is_holiday:              federal holiday indicator
#'   bar_degree:              officer mean of l_degree
#'   f_dw, f_month:           factor versions of day-of-week, month
#'   f_dw*, f_month*:         model matrix dummies
#'   opp_dist:                1 - dist_from_med (distance from wheel)
#'   suppliers_interacted:    opp_dist * l_wheel_degree
#'   demanders_interacted:    dist_from_med * l_wheel_nothave_degree
#'   ot_rate:                 max_rate * ot_premium
#'   bereave, family_leave:   leave type flags from raw data
#'   injured:                 injury flag from raw data
#' =============================================================================

library('data.table')
library('lubridate')
library('stringr')
library('almanac')

source('config.R')
source('utils/logging.R')

log_init("02_01_mk_estimation_sample.R")
log_message("Building estimation sample")

#' -----------------------------------------------------------------------------
#' LOAD NETWORK PANEL
#' -----------------------------------------------------------------------------

panel_path <- get_network_output_path(CONFIG$network_window_default)
raw_pay_path <- file.path(CONFIG$raw_pay_dir, "anonymized_data_073117.txt")

assert_required_files(c(panel_path, raw_pay_path))

all_pairs <- readRDS(panel_path)
log_message(paste("Loaded panel:", nrow(all_pairs), "rows"))

#' -----------------------------------------------------------------------------
#' EXCLUDE POST-INJURY DAYS UNTIL RETURN TO WORK
#' -----------------------------------------------------------------------------

setkey(all_pairs, "num_emp1", "analysis_workdate")
all_pairs[, inj_stint := cumsum(matched_injury), by = "num_emp1"]
all_pairs[(normal_work == 1 | ot_work == 1) & matched_injury == 0 & inj_stint > 0,
          first_work := min(analysis_workdate), by = c("num_emp1", "inj_stint")]

## Injured days should always show as working (they were on the job when injured)
stopifnot(all_pairs[matched_injury == 1, ]$ot_work == 1 | all_pairs[matched_injury == 1, ]$normal_work == 1)

all_pairs[inj_stint > 0, first_work := min(first_work, na.rm = TRUE), by = c("num_emp1", "inj_stint")]
all_pairs[inj_stint == 0, first_work := min(analysis_workdate), by = c("num_emp1", "inj_stint")]
## Drop days between injury and first return to work. An officer recovering from
## injury is structurally absent from the division schedule and cannot be in the
## control condition for other officers' OT decisions. Keeping these days would
## contaminate the comparison group with observation-days where the officer is
## known to be off the wheel by design. The injury day itself is retained so we
## can study the period immediately around the event.
## NOTE: officers who never return to work after injury will have no post-injury
## observations and will drop from all regressions via the officer fixed effect.
all_pairs <- all_pairs[analysis_workdate >= first_work | matched_injury == 1, ]

## Restrict to the core estimation window (2015-01-01 to 2016-06-30). The
## broader panel (from 2014-07-01) exists only to allow rolling window exposure
## histories to accumulate before the injury window opens.
all_pairs <- all_pairs[analysis_workdate >= CONFIG$estimation_start & analysis_workdate <= CONFIG$estimation_end, ]

log_message(paste("After date/injury filter:", nrow(all_pairs), "rows,", uniqueN(all_pairs$num_emp1), "officers"))

#' -----------------------------------------------------------------------------
#' ADD HOLIDAY FLAGS
#' -----------------------------------------------------------------------------

holidays <- data.table(cal_events(cal_us_federal(), year = CONFIG$estimation_years))
setnames(holidays, "name", "holiday")
setnames(holidays, "date", "analysis_workdate")
holidays[, is_holiday := 1]
holidays[, analysis_workdate := ymd(analysis_workdate)]

all_pairs <- merge(all_pairs, holidays, all.x = TRUE, by = "analysis_workdate")
all_pairs[is.na(is_holiday), is_holiday := 0]

#' -----------------------------------------------------------------------------
#' CREATE DERIVED VARIABLES
#' -----------------------------------------------------------------------------

all_pairs[, bar_degree := mean(l_degree, na.rm = TRUE), by = "num_emp1"]
all_pairs[, f_dw := as.factor(dw)]
all_pairs[, f_month := as.factor(month)]
all_pairs[, ot_work := as.numeric(ot_work)]
all_pairs[, normal_work := as.numeric(normal_work)]

## Model matrices for month and day-of-week dummies
all_pairs <- cbind(all_pairs,
                   model.matrix(~ f_month - 1, data = all_pairs),
                   model.matrix(~ f_dw - 1, data = all_pairs))

all_pairs[, opp_dist := 1 - dist_from_med]
all_pairs[, suppliers_interacted := opp_dist * l_wheel_degree]
all_pairs[, demanders_interacted := dist_from_med * l_wheel_nothave_degree]
all_pairs[, ot_rate := max_rate * CONFIG$ot_premium]

#' -----------------------------------------------------------------------------
#' ADD BEREAVEMENT AND FMLA FLAGS FROM RAW DATA
#' -----------------------------------------------------------------------------

raw_data <- fread(raw_pay_path)

## Extract injury records
injury <- unique(raw_data[`Med Pd` > 0, c("Med Pd", "EMPLOYEE_NAME", "DOI")])
stopifnot(nrow(injury) == uniqueN(injury[, -"Med Pd"]))
injury[, analysis_workdate := ymd(DOI)]
injury[, injured := 1]

## Flag FMLA and bereavement from variation descriptions
raw_data[, family_leave := str_detect(VARIATION_DESCRIPTION, "FML") | str_detect(VARIATION_DESCRIPTION, "FAMILY")]
raw_data[, bereave := str_detect(VARIATION_DESCRIPTION, "BEREAVEMENT")]
raw_data <- raw_data[, .(family_leave = max(family_leave), bereave = max(bereave)),
                     by = c("EMPLOYEE_NAME", "WORK_DATE")]
raw_data[, analysis_workdate := ymd(WORK_DATE)]
raw_data[, num_emp1 := as.integer(gsub(CONFIG$employee_name_pattern, "", EMPLOYEE_NAME))]

## Merge injury flag onto raw data, then merge leave flags onto estimation sample
raw_data <- merge(raw_data,
                  injury[, c("EMPLOYEE_NAME", "analysis_workdate", "injured")],
                  all.x = TRUE, by = c("EMPLOYEE_NAME", "analysis_workdate"))
all_pairs <- merge(all_pairs,
                   raw_data[, c("num_emp1", "analysis_workdate", "bereave", "family_leave", "injured")],
                   by = c("num_emp1", "analysis_workdate"), all.x = TRUE)

#' -----------------------------------------------------------------------------
#' SAVE OUTPUT
#' -----------------------------------------------------------------------------

setorder(all_pairs, "num_emp1", "analysis_workdate")

## Drop contact matrix columns (numeric officer-ID columns from panel);
## scripts that need the contact matrix load it via load_contact_matrix()
contact_cols <- grep("^[0-9]+$", names(all_pairs), value = TRUE)
if (length(contact_cols) > 0) all_pairs[, (contact_cols) := NULL]

output_path <- file.path(CONFIG$data_dir, "02_01_estimation_sample.rds")
ensure_directory(dirname(output_path))
saveRDS(all_pairs, output_path)

log_message(paste("Saved:", output_path, "-", nrow(all_pairs), "rows,", uniqueN(all_pairs$num_emp1), "officers"))
log_complete(success = TRUE)
message("02_01_mk_estimation_sample complete")
