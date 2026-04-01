#' =============================================================================
#' FAMILY MEDICAL LEAVE EVENT STUDY - Gardner (2022) did2s Imputation Estimator
#' (Peer Effects)
#' =============================================================================
#' Input:  data/02_01_estimation_sample.rds
#'         Contact matrix via load_contact_matrix()
#' Output: out/figures/03_05_fmla_did2s.png
#' =============================================================================

source('config.R')
source('utils/logging.R')
log_init("03_05_fmla_did2s.R")

#' ---------------------------------------------------------------------------
#' LOAD PACKAGES
#' ---------------------------------------------------------------------------

library('fixest')
library('did2s')
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
#' IDENTIFY FAMILY LEAVE EVENTS
#' ---------------------------------------------------------------------------

log_message("Identifying family leave events and assigning treatment")
all_pairs[is.na(family_leave), family_leave := 0]
all_pairs[family_leave == 1, first_time := min(analysis_workdate), by = "num_emp1"]
all_pairs[family_leave == 1, is_leave := first_time == analysis_workdate]
all_pairs[family_leave == 0, is_leave := 0]
all_pairs[, does_leave := max(is_leave), by = "num_emp1"]

#' ---------------------------------------------------------------------------
#' ASSIGN TREATMENT VIA CONTACT MATRIX
#' ---------------------------------------------------------------------------

term_list <- copy(all_pairs[is_leave == 1, ])
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
#' ESTIMATE AND PLOT (did2s)
#' ---------------------------------------------------------------------------

log_message("Estimating did2s imputation model")
## Exclude the officers who take FMLA leave from the regression. The estimand
## is the peer effect of a co-worker's FMLA absence on *other* officers'
## connectedness. Including the FMLA-taking officer (does_leave == 1) would
## confound the peer effect with the mechanical change in that officer's own
## degree while they are absent from the schedule.
est_data <- all_pairs[does_leave == 0]

## Event study did2s estimate
did2s_es <- did2s(est_data,
                  yname = "l_degree",
                  first_stage = ~ 0 | num_emp1 + analysis_workdate,
                  second_stage = ~ i(rel_time, ref = c(-1, Inf)),
                  treatment = "treat",
                  cluster_var = "num_emp1")

ensure_directory(CONFIG$figures_dir)
png(file.path(CONFIG$figures_dir, "03_05_fmla_did2s.png"), width = 900, height = 500)
iplot(did2s_es, main = "", xlab = "Time to Treatment (Days)", ylab = "Connectedness",
      drop = "([3-9]\\d{1}|\\d{3})", lab.fit = "simple")
dev.off()

log_message("03_05_fmla_did2s.R completed successfully")
log_complete(success = TRUE)
