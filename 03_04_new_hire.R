#' =============================================================================
#' NEW HIRE EVENT STUDY (DiD)
#' =============================================================================
#' Input:  data/02_01_estimation_sample.rds
#'         Contact matrix via load_contact_matrix()
#' Output: out/figures/03_04_new_hire_twfe.png
#'         data/03_04_new_hire_twfe_att.rds
#' =============================================================================

source('config.R')
source('utils/logging.R')
source('utils/sunab_utils.R')
log_init("03_04_new_hire.R")

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
#' ESTIMATE AND PLOT
#' ---------------------------------------------------------------------------

log_message("Estimating TWFE model")
twfe_att <- feols(
  l_degree ~ treat | num_emp1 + analysis_workdate,
  data = all_pairs[does_hire == 0],
  cluster = ~num_emp1
)
att_result <- save_twfe_att(
  twfe_att,
  file.path(CONFIG$data_dir, "03_04_new_hire_twfe_att.rds"),
  specification = "New Hire (Peer)",
  effect_type = "peer"
)
log_message(sprintf(
  "Estimated TWFE ATT = %.3f (SE = %.3f, p = %s)",
  att_result$estimate,
  att_result$std_error,
  format_p_value(att_result$p_value)
))

## Exclude the new hires themselves from the regression. We are estimating the
## peer effect of a new hire's arrival on incumbent officers' connectedness.
## Including the new hire (does_hire == 1) would mix the peer effect with the
## mechanical change in the new hire's own degree as they join the network.
twfe_es <- feols(l_degree ~ i(rel_time, ref = -c(1, Inf)) | num_emp1 + analysis_workdate, data = all_pairs[does_hire == 0], cluster = ~num_emp1)

ensure_directory(CONFIG$figures_dir)
png(file.path(CONFIG$figures_dir, "03_04_new_hire_twfe.png"), width = 900, height = 500)
iplot(twfe_es, main = "", xlab = "Time to Treatment (Days)", ylab = "Connectedness",
      drop = "([3-9]\\d{1}|\\d{3})", lab.fit = "simple")
dev.off()

log_message("03_04_new_hire.R completed successfully")
log_complete(success = TRUE)
