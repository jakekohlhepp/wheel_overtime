#' =============================================================================
#' TERMINATION EVENT STUDY (DiD)
#' =============================================================================
#' Input:  data/00_02_estimation_sample.rds
#'         Contact matrix via load_contact_matrix()
#' Output: out/figures/01_04_termination_twfe.png
#' =============================================================================

source('config.R')
source('utils/logging.R')
log_init("01_04_termination_did.R")

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
all_pairs <- readRDS(file.path(CONFIG$data_dir, "00_02_estimation_sample.rds"))

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
#' ESTIMATE AND PLOT
#' ---------------------------------------------------------------------------

log_message("Estimating TWFE models")
twfe <- feols(degree ~ treat | num_emp1 + analysis_workdate, data = all_pairs[does_term == 0])
summary(twfe)

twfe <- feols(degree ~ i(rel_time, ref = -c(1, Inf)) | num_emp1 + analysis_workdate, data = all_pairs[does_term == 0])

ensure_directory(CONFIG$figures_dir)
png(file.path(CONFIG$figures_dir, "01_04_termination_twfe.png"), width = 900, height = 500)
iplot(twfe, main = "", xlab = "Time to Treatment (Days)", ylab = "Connectedness",
      drop = "([3-9]\\d{1}|\\d{3})", lab.fit = "simple")
dev.off()

log_message("01_04_termination_did.R completed successfully")
log_complete(success = TRUE)
