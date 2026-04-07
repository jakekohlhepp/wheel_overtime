#' =============================================================================
#' NEW HIRE EVENT STUDY - Sun & Abraham (2021) Interaction-Weighted Estimator
#' =============================================================================
#' Input:  data/02_01_estimation_sample.rds
#'         Contact matrix via load_contact_matrix()
#' Output: out/figures/03_04_new_hire_sunab.png
#'         data/03_04_new_hire_sunab_att.rds
#' =============================================================================

source('config.R')
source('utils/logging.R')
source('utils/sunab_utils.R')
log_init("03_04_new_hire_sunab.R")

#' ---------------------------------------------------------------------------
#' LOAD PACKAGES
#' ---------------------------------------------------------------------------

library('fixest')
library('data.table')
library('ggplot2')

#' ---------------------------------------------------------------------------
#' LOAD DATA AND CONTACT MATRIX
#' ---------------------------------------------------------------------------

log_message("Loading estimation sample")
all_pairs <- readRDS(file.path(CONFIG$data_dir, "02_01_estimation_sample.rds"))

log_message("Loading contact matrix")
contact_matrix <- load_contact_matrix()
all_pairs <- merge(all_pairs, contact_matrix, by = c("num_emp1", "analysis_workdate"), all.x = TRUE)
rm(contact_matrix)

#' ---------------------------------------------------------------------------
#' IDENTIFY NEW HIRES
#' ---------------------------------------------------------------------------

log_message("Identifying new hires and assigning treatment")
all_pairs[, first_time := min(analysis_workdate), by = "num_emp1"]
## A new hire is treated as a network shock only if they enter on or after
## 2015-02-01. Hires entering in January 2015 (the first month of the
## estimation window) are excluded because they would have no pre-period
## observations in the estimation sample, making their event study uninformative.
## TODO: This cutoff should be derived from a minimum required pre-period window
## rather than hardcoded as 2015-02-01. Verify that one month of pre-period is
## sufficient for the parallel-trends assumption.
all_pairs[, is_hire := first_time == analysis_workdate & first_time >= as.Date('2015-02-01')]
all_pairs[, does_hire := max(is_hire), by = "num_emp1"]

#' ---------------------------------------------------------------------------
#' ASSIGN TREATMENT VIA CONTACT MATRIX
#' ---------------------------------------------------------------------------

newhire_list <- copy(all_pairs[is_hire == 1, ])
id_cols <- grep("^[0-9]+$", names(all_pairs), value = TRUE)
all_pairs[, treat := 0]
for (tt in 1:nrow(newhire_list)) {
  mark_list <- as.numeric(newhire_list[tt, .SD, .SDcols = id_cols])
  names(mark_list) <- id_cols
  mark_list <- names(mark_list)[which(mark_list > 0)]
  date_focal <- newhire_list[tt, ]$analysis_workdate
  all_pairs[, treat := treat + ((num_emp1 %in% as.numeric(mark_list)) & (analysis_workdate >= 1 + date_focal))]
}
# set treatment to be 1 at most.
all_pairs[treat > 1, treat := 1]
all_pairs[treat == 1, first_treat := analysis_workdate]
all_pairs[, first_treat := ifelse(max(treat) == 1, min(first_treat, na.rm = TRUE), Inf), by = "num_emp1"]
all_pairs[, rel_time := analysis_workdate - first_treat]

#' ---------------------------------------------------------------------------
#' PREPARE DATA FOR SUN & ABRAHAM ESTIMATOR
#' ---------------------------------------------------------------------------

log_message("Preparing data for Sun & Abraham estimator")
## Exclude the new hires themselves from the regression. We are estimating the
## peer effect of a new hire's arrival on incumbent officers' connectedness.
## Including the new hire (does_hire == 1) would mix the peer effect with the
## mechanical change in the new hire's own degree as they join the network.
est_data <- all_pairs[does_hire == 0]

## sunab with daily data creates a cohort x period interaction matrix that is
## computationally infeasible. Collapse to weekly frequency.
est_data[, week := as.numeric(floor((analysis_workdate - min(analysis_workdate)) / 7))]
est_data[, cohort_week := as.numeric(floor((first_treat - min(analysis_workdate)) / 7))]
est_data[is.infinite(first_treat), cohort_week := 10000]

weekly <- est_data[, .(l_degree = mean(l_degree, na.rm = TRUE),
                        treat = max(treat)),
                    by = .(num_emp1, week, cohort_week)]

#' ---------------------------------------------------------------------------
#' ESTIMATE AND PLOT
#' ---------------------------------------------------------------------------

log_message("Estimating Sun & Abraham model")

## Event study using Sun & Abraham interaction-weighted estimator
sa_es <- feols(l_degree ~ sunab(cohort_week, week, ref.p = c(-1, .F)) | num_emp1 + week, data = weekly, cluster = ~num_emp1)

att_result <- save_sunab_att(
  sa_es,
  file.path(CONFIG$data_dir, "03_04_new_hire_sunab_att.rds"),
  specification = "New Hire (Peer)",
  effect_type = "peer"
)
log_message(sprintf(
  "Estimated ATT = %.3f (SE = %.3f, p = %s)",
  att_result$estimate,
  att_result$std_error,
  format_p_value(att_result$p_value)
))

plot_sunab_event_study(
  sa_es,
  file.path(CONFIG$figures_dir, "03_04_new_hire_sunab.png"),
  xlab = "Time to Treatment (Weeks)",
  ylab = "Connectedness"
)

log_message("03_04_new_hire_sunab.R completed successfully")
log_complete(success = TRUE)
