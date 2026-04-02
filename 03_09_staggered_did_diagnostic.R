#' =============================================================================
#' STAGGERED DiD DIAGNOSTIC REPORT
#' =============================================================================
#' Compares TWFE and Sun & Abraham (2021) event study estimates for each of the
#' six event studies in this project. Sun & Abraham uses weekly aggregation to
#' keep the cohort x period matrix feasible with daily panel data.
#'
#' Produces a side-by-side comparison figure and a summary table of pooled ATT
#' estimates so that researchers can assess whether naive TWFE is biased by
#' staggered treatment timing.
#'
#' Input:  data/02_01_estimation_sample.rds
#'         Contact matrix via load_contact_matrix()
#' Output: out/figures/03_09_diagnostic_*.png  (one per event)
#'         out/tables/03_09_staggered_diagnostic.tex
#' =============================================================================

source('config.R')
source('utils/logging.R')
log_init("03_09_staggered_did_diagnostic.R")

#' ---------------------------------------------------------------------------
#' LOAD PACKAGES
#' ---------------------------------------------------------------------------

library('fixest')
library('data.table')
library('ggplot2')

#' ---------------------------------------------------------------------------
#' HELPER: EXTRACT EVENT STUDY COEFFICIENTS
#' ---------------------------------------------------------------------------

#' Extract coefficients from a fixest object (TWFE or sunab)
extract_fixest_es <- function(fit, estimator_label, drop_regex = "([3-9]\\d{1}|\\d{3})") {
  ct <- as.data.table(coeftable(fit), keep.rownames = "term")
  setnames(ct, c("term", "estimate", "se", "tval", "pval"))
  # Extract relative time from term names
  ct[, rel_time := as.numeric(gsub(".*::(-?[0-9]+).*", "\\1", term))]
  ct <- ct[!is.na(rel_time)]
  ct[, `:=`(lower = estimate - 1.96 * se, upper = estimate + 1.96 * se)]
  ct[, estimator := estimator_label]
  ct[abs(rel_time) < 30]  # keep window of interest
}

#' ---------------------------------------------------------------------------
#' HELPER: PLOT COMPARISON
#' ---------------------------------------------------------------------------

plot_comparison <- function(combined_dt, title, ylab, filename) {
  p <- ggplot(combined_dt, aes(x = rel_time, y = estimate, color = estimator, fill = estimator)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = -0.5, linetype = "dotted", color = "gray70") +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, color = NA) +
    geom_line(linewidth = 0.6) +
    geom_point(size = 1.2) +
    scale_color_manual(values = c("TWFE" = "#E41A1C", "Sun & Abraham" = "#4DAF4A")) +
    scale_fill_manual(values = c("TWFE" = "#E41A1C", "Sun & Abraham" = "#4DAF4A")) +
    labs(x = "Time to Treatment (Days)", y = ylab, color = "Estimator", fill = "Estimator") +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom",
          panel.grid.minor = element_blank())

  ensure_directory(CONFIG$figures_dir)
  png(file.path(CONFIG$figures_dir, filename), width = 1100, height = 600)
  print(p)
  dev.off()
  log_message(paste("Saved", filename))
}

#' ---------------------------------------------------------------------------
#' LOAD DATA
#' ---------------------------------------------------------------------------

log_message("Loading estimation sample")
full_data <- readRDS(file.path(CONFIG$data_dir, "02_01_estimation_sample.rds"))

log_message("Loading contact matrix")
contact_matrix <- load_contact_matrix()
full_data_cm <- merge(copy(full_data), contact_matrix, by = c("num_emp1", "analysis_workdate"), all.x = TRUE)
rm(contact_matrix)

#' ---------------------------------------------------------------------------
#' HELPER: PREPARE TREATMENT VARIABLES (peer-effect scripts)
#' ---------------------------------------------------------------------------

#' Assign peer treatment via contact matrix loop.
#' Returns data with treat, first_treat, rel_time columns added.
assign_peer_treatment <- function(dt, event_rows, positive_contact = FALSE) {
  id_cols <- grep("^[0-9]+$", names(dt), value = TRUE)
  dt[, treat := 0]
  for (tt in 1:nrow(event_rows)) {
    mark_list <- as.numeric(event_rows[tt, .SD, .SDcols = id_cols])
    names(mark_list) <- id_cols
    if (positive_contact) {
      mark_list <- names(mark_list)[which(mark_list > 0)]
    } else {
      mark_list <- names(mark_list)[which(mark_list >= 1)]
    }
    date_focal <- event_rows[tt, ]$analysis_workdate
    dt[, treat := treat + ((num_emp1 %in% as.numeric(mark_list)) & (analysis_workdate >= 1 + date_focal))]
  }
  dt[treat > 1, treat := 1]
  dt[treat == 1, first_treat := analysis_workdate]
  dt[, first_treat := ifelse(max(treat) == 1, min(first_treat, na.rm = TRUE), Inf), by = "num_emp1"]
  dt[, rel_time := analysis_workdate - first_treat]
  dt
}

#' ---------------------------------------------------------------------------
#' RUN TWFE AND SUN & ABRAHAM FOR ONE EVENT
#' ---------------------------------------------------------------------------

run_estimators <- function(est_data, outcome, ref_period, event_label, ylab, filename) {
  log_message(paste("Running estimators for:", event_label))

  results <- list()

  # --- 1. TWFE ---
  log_message(paste(" ", event_label, "- TWFE"))
  twfe_ref <- abs(ref_period)
  fml_twfe <- as.formula(paste0(outcome, " ~ i(rel_time, ref = -c(", twfe_ref, ", Inf)) | num_emp1 + analysis_workdate"))
  twfe_fit <- feols(fml_twfe, data = est_data, cluster = ~num_emp1)
  results[["twfe"]] <- extract_fixest_es(twfe_fit, "TWFE")

  # --- 2. Sun & Abraham (2021) ---
  ## sunab with daily data is infeasible (cohort x period matrix too large).
  ## Collapse to weekly frequency before running.
  log_message(paste(" ", event_label, "- Sun & Abraham (weekly aggregation)"))
  tryCatch({
    min_date <- min(est_data$analysis_workdate)
    est_data[, sa_week := as.numeric(floor((analysis_workdate - min_date) / 7))]
    est_data[, sa_cohort_week := as.numeric(floor((first_treat - min_date) / 7))]
    est_data[is.infinite(first_treat), sa_cohort_week := 10000]
    weekly_sa <- est_data[, .(outcome_val = mean(get(outcome), na.rm = TRUE),
                               treat = max(treat)),
                           by = .(num_emp1, sa_week, sa_cohort_week)]
    setnames(weekly_sa, "outcome_val", outcome)
    fml_sa_rhs <- paste0("sunab(sa_cohort_week, sa_week, ref.p = c(", ref_period, ", .F))")
    fml_sa <- as.formula(paste0(outcome, " ~ ", fml_sa_rhs, " | num_emp1 + sa_week"))
    sa_fit <- feols(fml_sa, data = weekly_sa, cluster = ~num_emp1)
    results[["sunab"]] <- extract_fixest_es(sa_fit, "Sun & Abraham")
  }, error = function(e) {
    log_message(paste("  Sun & Abraham failed:", conditionMessage(e)))
  })

  # --- Combine and plot ---
  combined <- rbindlist(results, use.names = TRUE, fill = TRUE)
  if (nrow(combined) > 0) {
    plot_comparison(combined, event_label, ylab, filename)
  }

  # --- Return pooled ATTs for summary table ---
  pooled <- list()

  # TWFE pooled
  fml_pool <- as.formula(paste0(outcome, " ~ treat | num_emp1 + analysis_workdate"))
  twfe_pool <- feols(fml_pool, data = est_data, cluster = ~num_emp1)
  twfe_ct <- coeftable(twfe_pool)
  pooled[["TWFE"]] <- data.table(estimator = "TWFE", event = event_label,
                                  att = twfe_ct[1, 1], se = twfe_ct[1, 2])

  rbindlist(pooled, use.names = TRUE, fill = TRUE)
}

#' =============================================================================
#' EVENT 1: TERMINATIONS (Peer Effect)
#' =============================================================================

log_message("=== Event 1: Terminations ===")
dt1 <- copy(full_data_cm)
dt1[, last_time := max(analysis_workdate), by = "num_emp1"]
dt1[, is_term := last_time == analysis_workdate & last_time <= CONFIG$termination_cutoff]
dt1[, does_term := max(is_term), by = "num_emp1"]
term_events <- copy(dt1[is_term == 1, ])
dt1 <- assign_peer_treatment(dt1, term_events)
est1 <- dt1[does_term == 0]
pool1 <- run_estimators(est1, "degree", -1, "Termination (Peer)", "Connectedness",
                         "03_09_diagnostic_termination.png")

#' =============================================================================
#' EVENT 2: NEW HIRES (Peer Effect)
#' =============================================================================

log_message("=== Event 2: New Hires ===")
dt2 <- copy(full_data_cm)
dt2[, first_time := min(analysis_workdate), by = "num_emp1"]
dt2[, is_hire := first_time == analysis_workdate & first_time >= as.Date('2015-02-01')]
dt2[, does_hire := max(is_hire), by = "num_emp1"]
hire_events <- copy(dt2[is_hire == 1, ])
dt2 <- assign_peer_treatment(dt2, hire_events, positive_contact = TRUE)
est2 <- dt2[does_hire == 0]
pool2 <- run_estimators(est2, "l_degree", -1, "New Hire (Peer)", "Connectedness",
                         "03_09_diagnostic_new_hire.png")

#' =============================================================================
#' EVENT 3: FMLA (Peer Effect)
#' =============================================================================

log_message("=== Event 3: FMLA (Peer) ===")
dt3 <- copy(full_data_cm)
dt3[is.na(family_leave), family_leave := 0]
dt3[family_leave == 1, first_time := min(analysis_workdate), by = "num_emp1"]
dt3[family_leave == 1, is_leave := first_time == analysis_workdate]
dt3[family_leave == 0, is_leave := 0]
dt3[, does_leave := max(is_leave), by = "num_emp1"]
fmla_events <- copy(dt3[is_leave == 1, ])
dt3 <- assign_peer_treatment(dt3, fmla_events)
est3 <- dt3[does_leave == 0]
pool3 <- run_estimators(est3, "l_degree", -1, "FMLA (Peer)", "Connectedness",
                         "03_09_diagnostic_fmla_peer.png")

#' =============================================================================
#' EVENT 4: FMLA (Own Effect)
#' =============================================================================

log_message("=== Event 4: FMLA (Own) ===")
dt4 <- copy(full_data)
dt4[is.na(family_leave), family_leave := 0]
dt4[family_leave == 1, first_time := min(analysis_workdate), by = "num_emp1"]
dt4[family_leave == 1, is_leave := first_time == analysis_workdate]
dt4[family_leave == 0, is_leave := 0]
dt4[, does_leave := max(is_leave), by = "num_emp1"]
dt4[, treat := 0]
dt4[is_leave == 1, treat := 1]
dt4[treat == 1, first_treat := analysis_workdate]
dt4[, first_treat := ifelse(max(treat) == 1, min(first_treat, na.rm = TRUE), Inf), by = "num_emp1"]
dt4[, rel_time := analysis_workdate - first_treat]
pool4 <- run_estimators(dt4, "wheel_degree", 0, "FMLA (Own)", "Connectedness",
                         "03_09_diagnostic_fmla_own.png")

#' =============================================================================
#' EVENT 5: BEREAVEMENT (Peer Effect)
#' =============================================================================

log_message("=== Event 5: Bereavement (Peer) ===")
dt5 <- copy(full_data_cm)
dt5[is.na(bereave), bereave := 0]
dt5[bereave == 1, first_time := min(analysis_workdate), by = "num_emp1"]
dt5[bereave == 1, is_leave := first_time == analysis_workdate]
dt5[bereave == 0, is_leave := 0]
dt5[, does_leave := max(is_leave), by = "num_emp1"]
bereave_events <- copy(dt5[is_leave == 1, ])
dt5 <- assign_peer_treatment(dt5, bereave_events)
est5 <- dt5[does_leave == 0]
pool5 <- run_estimators(est5, "l_degree", -1, "Bereavement (Peer)", "Connectedness",
                         "03_09_diagnostic_bereave_peer.png")

#' =============================================================================
#' EVENT 6: BEREAVEMENT (Own Effect)
#' =============================================================================

log_message("=== Event 6: Bereavement (Own) ===")
dt6 <- copy(full_data)
dt6[is.na(bereave), bereave := 0]
dt6[bereave == 1, first_time := min(analysis_workdate), by = "num_emp1"]
dt6[bereave == 1, is_leave := first_time == analysis_workdate]
dt6[bereave == 0, is_leave := 0]
dt6[, does_leave := max(is_leave), by = "num_emp1"]
dt6[, treat := 0]
dt6[is_leave == 1, treat := 1]
dt6[treat == 1, first_treat := analysis_workdate]
dt6[, first_treat := ifelse(max(treat) == 1, min(first_treat, na.rm = TRUE), Inf), by = "num_emp1"]
dt6[, rel_time := analysis_workdate - first_treat]
pool6 <- run_estimators(dt6, "wheel_degree", 0, "Bereavement (Own)", "Potential Supplier Count",
                         "03_09_diagnostic_bereave_own.png")

#' =============================================================================
#' SUMMARY TABLE
#' =============================================================================

log_message("Assembling summary table")
summary_table <- rbindlist(list(pool1, pool2, pool3, pool4, pool5, pool6), use.names = TRUE, fill = TRUE)
summary_table[, stars := ifelse(abs(att / se) > 2.576, "***",
                         ifelse(abs(att / se) > 1.96, "**",
                         ifelse(abs(att / se) > 1.645, "*", "")))]
summary_table[, display := paste0(sprintf("%.4f", att), stars, "\n(", sprintf("%.4f", se), ")")]

# Reshape to wide for a clean comparison table
wide <- dcast(summary_table, event ~ estimator, value.var = "display")
cat("\n===== STAGGERED DiD DIAGNOSTIC SUMMARY =====\n\n")
print(wide, row.names = FALSE)

# Save as LaTeX
ensure_directory(CONFIG$tables_dir)
if (requireNamespace("xtable", quietly = TRUE)) {
  library('xtable')
  wide_tex <- dcast(summary_table, event ~ estimator, value.var = "display")
  xt <- xtable(wide_tex, caption = "Pooled ATT Estimates: TWFE vs.\\ Sun \\& Abraham (2021)",
               label = "tab:staggered_diagnostic")
  print(xt, file = file.path(CONFIG$tables_dir, "03_09_staggered_diagnostic.tex"),
        include.rownames = FALSE, sanitize.text.function = identity,
        floating = TRUE, booktabs = TRUE)
  log_message("Saved LaTeX table")
}

#' =============================================================================
#' DIAGNOSTIC NOTES (printed to console)
#' =============================================================================

cat("\n===== DIAGNOSTIC INTERPRETATION GUIDE =====\n\n")
cat("What to look for in the comparison plots:\n\n")
cat("1. PRE-TRENDS: Both estimators should show coefficients near zero before\n")
cat("   treatment. If TWFE shows significant pre-trends but Sun & Abraham\n")
cat("   does not (or vice versa), this signals TWFE contamination from\n")
cat("   negative weighting of already-treated units.\n\n")
cat("2. POST-TREATMENT DIVERGENCE: If TWFE and Sun & Abraham diverge after\n")
cat("   treatment, the TWFE estimate is likely biased by heterogeneous\n")
cat("   treatment effects across cohorts. The direction of bias depends on\n")
cat("   whether early vs. late cohorts have larger effects.\n\n")
cat("3. CONFIDENCE INTERVALS: Sun & Abraham typically has wider CIs because\n")
cat("   it avoids forbidden comparisons (already-treated as controls). Wider\n")
cat("   CIs are honest, not a weakness.\n\n")
cat("4. NOTE ON WEEKLY AGGREGATION: Sun & Abraham results use weekly-averaged\n")
cat("   data to keep the cohort x period interaction matrix feasible. The\n")
cat("   x-axis for sunab is in weeks, while TWFE uses daily observations.\n")
cat("   This means sunab coefficients are smoothed relative to TWFE.\n\n")
cat("5. POOLED ATT TABLE: Large differences in the pooled ATT between TWFE\n")
cat("   and Sun & Abraham indicate meaningful staggered-timing bias.\n")
cat("   Small differences suggest TWFE may be adequate for this setting.\n\n")

log_message("03_09_staggered_did_diagnostic.R completed successfully")
log_complete(success = TRUE)
