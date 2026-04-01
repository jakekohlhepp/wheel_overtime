#' =============================================================================
#' FAMILY MEDICAL LEAVE EVENT STUDY - OWN CONNECTEDNESS
#' =============================================================================
#' Input:  data/02_01_estimation_sample.rds
#' Output: out/figures/03_06_own_fmla.png
#' =============================================================================

source('config.R')
source('utils/logging.R')
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
twfe <- feols(wheel_degree ~ treat | num_emp1 + analysis_workdate, data = all_pairs)
summary(twfe)

twfe <- feols(wheel_degree ~ i(rel_time, ref = -c(0, Inf)) | num_emp1 + analysis_workdate, data = all_pairs)

ensure_directory(CONFIG$figures_dir)
png(file.path(CONFIG$figures_dir, "03_06_own_fmla.png"), width = 900, height = 500)
iplot(twfe, main = "", xlab = "Time to Family Medical Leave (Days)", ylab = "Connectedness",
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

twfe <- feols(wheel_degree ~ treat | num_emp1 + analysis_workdate, data = all_pairs)
summary(twfe)

log_message("03_06_own_fmla.R completed successfully")
log_complete(success = TRUE)
