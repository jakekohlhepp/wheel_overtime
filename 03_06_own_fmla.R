#' =============================================================================
#' FAMILY MEDICAL LEAVE EVENT STUDY - OWN CONNECTEDNESS
#' =============================================================================
#' Input:  data/02_01_estimation_sample.rds
#' Output: out/figures/03_06_own_fmla.png
#'         data/03_06_own_fmla_twfe_att.rds
#' =============================================================================

source('config.R')
source('utils/logging.R')
source('utils/sunab_utils.R')
log_init("03_06_own_fmla.R")

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
#' ESTIMATE AND PLOT
#' ---------------------------------------------------------------------------

log_message("Estimating TWFE models")
twfe_att <- feols(wheel_degree ~ treat | num_emp1 + analysis_workdate, data = all_pairs)
att_result <- save_twfe_att(
  twfe_att,
  file.path(CONFIG$data_dir, "03_06_own_fmla_twfe_att.rds"),
  specification = "FMLA (Own)",
  effect_type = "own"
)
log_message(sprintf(
  "Estimated TWFE ATT = %.3f (SE = %.3f, p = %s)",
  att_result$estimate,
  att_result$std_error,
  format_p_value(att_result$p_value)
))
summary(twfe_att)

twfe_es <- feols(wheel_degree ~ i(rel_time, ref = -c(0, Inf)) | num_emp1 + analysis_workdate, data = all_pairs)

ensure_directory(CONFIG$figures_dir)
png(file.path(CONFIG$figures_dir, "03_06_own_fmla.png"), width = 900, height = 500)
iplot(twfe_es, main = "", xlab = "Time to Family Medical Leave (Days)", ylab = "Connectedness",
      drop = "([3-9]\\d{1}|\\d{3})", lab.fit = "simple")
dev.off()

#' ---------------------------------------------------------------------------
#' UNCONDITIONAL WITH TREATMENT STARTING DAY PRIOR
#' ---------------------------------------------------------------------------

log_message("Estimating unconditional TWFE with prior-day treatment")
all_pairs[, first_treat := NULL]
all_pairs[, treat := 0]
all_pairs[, treat := rel_time == -1]
all_pairs[treat == 1, first_treat := analysis_workdate]
all_pairs[, first_treat := ifelse(max(treat) == 1, min(first_treat, na.rm = TRUE), Inf), by = "num_emp1"]

twfe_prior <- feols(wheel_degree ~ treat | num_emp1 + analysis_workdate, data = all_pairs)
summary(twfe_prior)

log_message("03_06_own_fmla.R completed successfully")
log_complete(success = TRUE)
