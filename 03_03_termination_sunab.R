#' =============================================================================
#' TERMINATION EVENT STUDY - Sun & Abraham (2021) Interaction-Weighted Estimator
#' =============================================================================
#' Input:  data/02_01_estimation_sample.rds
#'         Contact matrix via load_contact_matrix()
#' Output: out/figures/03_03_termination_sunab.png
#' =============================================================================

source('config.R')
source('utils/logging.R')
log_init("03_03_termination_sunab.R")

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
#' IDENTIFY TERMINATIONS
#' ---------------------------------------------------------------------------

log_message("Identifying terminations and assigning treatment")
all_pairs[, last_time := max(analysis_workdate), by = "num_emp1"]
all_pairs[, is_term := last_time == analysis_workdate & last_time <= CONFIG$termination_cutoff]
all_pairs[, does_term := max(is_term), by = "num_emp1"]

#' ---------------------------------------------------------------------------
#' ASSIGN TREATMENT VIA CONTACT MATRIX
#' ---------------------------------------------------------------------------

term_list <- copy(all_pairs[is_term == 1, ])
id_cols <- grep("^[0-9]+$", names(all_pairs), value = TRUE)
all_pairs[, treat := 0]
for (tt in 1:nrow(term_list)) {
  mark_list <- as.numeric(term_list[tt, .SD, .SDcols = id_cols])
  names(mark_list) <- id_cols
  mark_list <- names(mark_list)[which(mark_list >= 1)]
  date_focal <- term_list[tt, ]$analysis_workdate
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
## Exclude the terminating officers themselves from the regression. The research
## question is whether a peer's departure affects *other* officers' connectedness
## (a peer effect), not whether the departing officer's own degree changes
## mechanically as they exit.
est_data <- all_pairs[does_term == 0]

## sunab with daily data creates a cohort x period interaction matrix that is
## computationally infeasible (547 days x many cohorts). Collapse to weekly
## frequency, which is standard practice for these estimators.
est_data[, week := as.numeric(floor((analysis_workdate - min(analysis_workdate)) / 7))]
est_data[, cohort_week := as.numeric(floor((first_treat - min(analysis_workdate)) / 7))]
est_data[is.infinite(first_treat), cohort_week := 10000]

weekly <- est_data[, .(degree = mean(degree, na.rm = TRUE),
                        treat = max(treat)),
                    by = .(num_emp1, week, cohort_week)]

#' ---------------------------------------------------------------------------
#' ESTIMATE AND PLOT
#' ---------------------------------------------------------------------------

log_message("Estimating Sun & Abraham models")

## Pooled estimate (on weekly data)
sa_pooled <- feols(degree ~ treat | num_emp1 + week, data = weekly, cluster = ~num_emp1)
summary(sa_pooled)

## Event study using Sun & Abraham interaction-weighted estimator
sa_es <- feols(degree ~ sunab(cohort_week, week, ref.p = c(-1, .F)) | num_emp1 + week, data = weekly, cluster = ~num_emp1)

ensure_directory(CONFIG$figures_dir)
png(file.path(CONFIG$figures_dir, "03_03_termination_sunab.png"), width = 900, height = 500)
iplot(sa_es, main = "", xlab = "Time to Treatment (Weeks)", ylab = "Connectedness",
      drop = "([5-9]\\d{0}|[1-9]\\d{1,})", lab.fit = "simple")
dev.off()

log_message("03_03_termination_sunab.R completed successfully")
log_complete(success = TRUE)
