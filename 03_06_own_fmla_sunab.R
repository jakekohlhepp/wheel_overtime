#' =============================================================================
#' FAMILY MEDICAL LEAVE EVENT STUDY - Sun & Abraham (2021) Interaction-Weighted Estimator (Own Effects)
#' =============================================================================
#' Input:  data/02_01_estimation_sample.rds
#' Output: out/figures/03_06_own_fmla_sunab.png
#'         data/03_06_own_fmla_sunab_att.rds
#' =============================================================================

source('config.R')
source('utils/logging.R')
source('utils/sunab_utils.R')
log_init("03_06_own_fmla_sunab.R")

#' ---------------------------------------------------------------------------
#' LOAD PACKAGES
#' ---------------------------------------------------------------------------

library('fixest')
library('data.table')
library('ggplot2')

#' ---------------------------------------------------------------------------
#' LOAD DATA
#' ---------------------------------------------------------------------------

log_message("Loading estimation sample")
all_pairs <- readRDS(file.path(CONFIG$data_dir, "02_01_estimation_sample.rds"))

#' ---------------------------------------------------------------------------
#' IDENTIFY FAMILY LEAVE EVENTS
#' ---------------------------------------------------------------------------

log_message("Identifying family leave events for own-effect analysis")
all_pairs[is.na(family_leave), family_leave := 0]
all_pairs[family_leave == 1, first_time := min(analysis_workdate), by = "num_emp1"]
all_pairs[family_leave == 1, is_leave := first_time == analysis_workdate]
all_pairs[family_leave == 0, is_leave := 0]
all_pairs[, does_leave := max(is_leave), by = "num_emp1"]

#' ---------------------------------------------------------------------------
#' ASSIGN OWN TREATMENT
#' ---------------------------------------------------------------------------

# set treatment to be 1 at most.
all_pairs[, treat := 0]
all_pairs[is_leave == 1, treat := 1]
all_pairs[treat == 1, first_treat := analysis_workdate]
all_pairs[, first_treat := ifelse(max(treat) == 1, min(first_treat, na.rm = TRUE), Inf), by = "num_emp1"]
all_pairs[, rel_time := analysis_workdate - first_treat]

#' ---------------------------------------------------------------------------
#' PREPARE DATA FOR SUN & ABRAHAM ESTIMATOR
#' ---------------------------------------------------------------------------

log_message("Preparing data for Sun & Abraham estimator")
est_data <- copy(all_pairs)

## sunab with daily data creates a cohort x period interaction matrix that is
## computationally infeasible (547 days x many cohorts). Collapse to weekly
## frequency, which is standard practice for these estimators.
est_data[, week := as.numeric(floor((analysis_workdate - min(analysis_workdate)) / 7))]
est_data[, cohort_week := as.numeric(floor((first_treat - min(analysis_workdate)) / 7))]
est_data[is.infinite(first_treat), cohort_week := 10000]

weekly <- est_data[, .(wheel_degree = mean(wheel_degree, na.rm = TRUE),
                        treat = max(treat)),
                    by = .(num_emp1, week, cohort_week)]

#' ---------------------------------------------------------------------------
#' ESTIMATE AND PLOT
#' ---------------------------------------------------------------------------

log_message("Estimating Sun & Abraham models")

## Event study using Sun & Abraham interaction-weighted estimator
sa_es <- feols(wheel_degree ~ sunab(cohort_week, week, ref.p = c(0, .F)) | num_emp1 + week, data = weekly, cluster = ~num_emp1)

att_result <- save_sunab_att(
  sa_es,
  file.path(CONFIG$data_dir, "03_06_own_fmla_sunab_att.rds"),
  specification = "FMLA (Own)",
  effect_type = "own"
)
log_message(sprintf(
  "Estimated ATT = %.3f (SE = %.3f, p = %s)",
  att_result$estimate,
  att_result$std_error,
  format_p_value(att_result$p_value)
))

plot_sunab_event_study(
  sa_es,
  file.path(CONFIG$figures_dir, "03_06_own_fmla_sunab.png"),
  xlab = "Time to Family Medical Leave (Weeks)",
  ylab = "Connectedness"
)

log_message("03_06_own_fmla_sunab.R completed successfully")
log_complete(success = TRUE)
