#' =============================================================================
#' MASTER SCRIPT: Analysis Pipeline
#' =============================================================================
#' Runs all analysis scripts in dependency order.
#' Assumes data preparation (run_prep_data.R) has already completed.
#'
#' Usage: source("run_analysis.R")
#' =============================================================================

rm(list = ls())
pipeline_start <- Sys.time()

source("config.R")
source("utils/logging.R")

ensure_directory(CONFIG$log_dir)
ensure_directory(CONFIG$tables_dir)
ensure_directory(CONFIG$figures_dir)

# Pipeline step toggles
RUN_EST_SAMPLE    <- TRUE
RUN_FACTS         <- TRUE
RUN_EVENT_STUDIES <- TRUE
RUN_MODERN_DID    <- TRUE
RUN_ESTIMATION    <- TRUE
RUN_EST_ANALYSIS  <- TRUE
RUN_SIMULATIONS   <- TRUE
RUN_SIM_COMPARE   <- TRUE

pipeline_results <- list()
pipeline_success <- FALSE

emit_status <- function(msg, level = "INFO") {
  message(msg)
  log_message(msg, level = level)
}

log_init("run_analysis.R")
on.exit({
  total_time <- as.numeric(difftime(Sys.time(), pipeline_start, units = "mins"))
  log_pipeline_summary(pipeline_results, pipeline_start,
                       title = "ANALYSIS PIPELINE SUMMARY")
  log_complete(success = pipeline_success, duration = total_time)
}, add = TRUE)

message("\n", strrep("=", 70))
message("ANALYSIS PIPELINE")
message(strrep("=", 70))
message("Start time: ", pipeline_start)
log_message("ANALYSIS PIPELINE")
log_message(paste("Start time:", format(pipeline_start, "%Y-%m-%d %H:%M:%S")))

#' -----------------------------------------------------------------------------
#' Helper: run a single step
#' -----------------------------------------------------------------------------

run_step <- function(step_name, script, deps, outputs = NULL,
                     env = new.env(parent = globalenv())) {
  message("\n", strrep("-", 70))
  message("STEP: ", step_name)
  message(strrep("-", 70))
  log_message(paste("STEP:", step_name))

  if (needs_rerun(step_name, deps, outputs = outputs)) {
    step_start <- Sys.time()
    emit_status(paste("Running", step_name))

    tryCatch({
      source(script, local = env)
      if (!is.null(outputs)) assert_required_files(outputs)

      step_time <- difftime(Sys.time(), step_start, units = "mins")
      pipeline_results[[step_name]] <<- list(
        ran = TRUE, success = TRUE, duration = as.numeric(step_time),
        error = NULL, skipped = FALSE
      )
      emit_status(sprintf("%s complete (%.2f minutes)", step_name, as.numeric(step_time)))
    }, error = function(e) {
      pipeline_results[[step_name]] <<- list(
        ran = TRUE, success = FALSE, duration = 0,
        error = e$message, skipped = FALSE
      )
      emit_status(paste(step_name, "failed:", e$message), level = "ERROR")
      stop(step_name, " failed: ", e$message)
    })
  } else {
    pipeline_results[[step_name]] <<- list(
      ran = FALSE, success = TRUE, duration = 0, error = NULL, skipped = TRUE
    )
    emit_status(paste(step_name, "skipped (no changes detected)"))
  }
}

#' -----------------------------------------------------------------------------
#' TIER 2: Build estimation sample
#' -----------------------------------------------------------------------------

est_sample_path <- file.path(CONFIG$data_dir, "02_01_estimation_sample.rds")

if (RUN_EST_SAMPLE) {
  run_step("02_01_mk_estimation_sample",
           "02_01_mk_estimation_sample.R",
           deps = c("config.R", "02_01_mk_estimation_sample.R",
                    get_network_output_path(CONFIG$network_window_default),
                    file.path(CONFIG$raw_pay_dir, "anonymized_data_073117.txt")),
           outputs = est_sample_path)
}

#' -----------------------------------------------------------------------------
#' TIER 3: Descriptive facts and event studies
#' -----------------------------------------------------------------------------

facts_outputs <- c(
  file.path(CONFIG$tables_dir, "03_01_summary_stats.tex"),
  file.path(CONFIG$tables_dir, "03_01_claim.tex"),
  file.path(CONFIG$tables_dir, "03_01_nature.tex"),
  file.path(CONFIG$figures_dir, "03_01_age_dispersion.png"),
  file.path(CONFIG$figures_dir, "03_01_wheel_turning_oct.png"),
  file.path(CONFIG$figures_dir, "03_01_wheel_turning_july.png"),
  file.path(CONFIG$figures_dir, "03_01_wheel_predicts_ot.png"),
  file.path(CONFIG$figures_dir, "03_01_ot_dipersion_frac.png"),
  file.path(CONFIG$figures_dir, "03_01_ot_dipersion_count.png"),
  file.path(CONFIG$figures_dir, "03_01_buyers_sellers_connectedness.png"),
  file.path(CONFIG$figures_dir, "03_01_zoomed_out_network.png"),
  file.path(CONFIG$figures_dir, "03_01_zoomed_in_network.png"),
  file.path(CONFIG$figures_dir, "03_01_zoomed_out_network_centrality.png"),
  file.path(CONFIG$figures_dir, "03_01_zoomed_in_network_centrality.png"),
  file.path(CONFIG$figures_dir, "03_01_zoomed_in_network_centrality_18.png"),
  file.path(CONFIG$figures_dir, "03_01_timevarying_230_20150101.png"),
  file.path(CONFIG$figures_dir, "03_01_timevarying_230_20150701.png"),
  file.path(CONFIG$figures_dir, "03_01_timevarying_230_20160101.png"),
  file.path(CONFIG$figures_dir, "03_01_potential_supplier_hist.png"),
  file.path(CONFIG$figures_dir, "03_01_potential_supplier_hist_idiosyncratic.png"),
  file.path(CONFIG$figures_dir, "03_01_potential_supplier_count.png"),
  file.path(CONFIG$figures_dir, "03_01_relevance_resid.png")
)

if (RUN_FACTS) {
  run_step("03_01_facts",
           "03_01_facts.R",
           deps = c("config.R", "03_01_facts.R", est_sample_path),
           outputs = facts_outputs)
}

if (RUN_FACTS) {
  run_step("03_02_lag_check",
           "03_02_lag_check.R",
           deps = c("config.R", "03_02_lag_check.R", est_sample_path))
}

if (RUN_EVENT_STUDIES) {
  event_scripts <- c(
    "03_03_termination_did",
    "03_04_new_hire",
    "03_05_fmla",
    "03_06_own_fmla",
    "03_07_bereave",
    "03_08_own_bereave"
  )

  event_outputs <- list(
    "03_03_termination_did" = c(
      file.path(CONFIG$figures_dir, "03_03_termination_twfe.png"),
      file.path(CONFIG$data_dir, "03_03_termination_twfe_att.rds")
    ),
    "03_04_new_hire" = c(
      file.path(CONFIG$figures_dir, "03_04_new_hire_twfe.png"),
      file.path(CONFIG$data_dir, "03_04_new_hire_twfe_att.rds")
    ),
    "03_05_fmla" = c(
      file.path(CONFIG$figures_dir, "03_05_fmla_twfe.png"),
      file.path(CONFIG$data_dir, "03_05_fmla_twfe_att.rds")
    ),
    "03_06_own_fmla" = c(
      file.path(CONFIG$figures_dir, "03_06_own_fmla.png"),
      file.path(CONFIG$data_dir, "03_06_own_fmla_twfe_att.rds")
    ),
    "03_07_bereave" = c(
      file.path(CONFIG$figures_dir, "03_07_bereave_twfe.png"),
      file.path(CONFIG$data_dir, "03_07_bereave_twfe_att.rds")
    ),
    "03_08_own_bereave" = c(
      file.path(CONFIG$figures_dir, "03_08_own_bereave_twfe.png"),
      file.path(CONFIG$data_dir, "03_08_own_bereave_twfe_att.rds")
    )
  )

  for (script_name in event_scripts) {
    script_file <- paste0(script_name, ".R")
    run_step(script_name, script_file,
             deps = c("config.R", script_file, est_sample_path),
             outputs = event_outputs[[script_name]])
  }
}

#' -----------------------------------------------------------------------------
#' TIER 3b: Modern staggered DiD estimators (Sun & Abraham, Callaway & Sant'Anna)
#' -----------------------------------------------------------------------------

if (RUN_MODERN_DID) {
  modern_suffixes <- c("did2s", "sunab", "cs")
  modern_bases <- c(
    "03_03_termination",
    "03_04_new_hire",
    "03_05_fmla",
    "03_06_own_fmla",
    "03_07_bereave",
    "03_08_own_bereave"
  )

  for (base in modern_bases) {
    for (suffix in modern_suffixes) {
      script_name <- paste0(base, "_", suffix)
      script_file <- paste0(script_name, ".R")
      if (!file.exists(script_file)) next

      output_file <- file.path(CONFIG$figures_dir, paste0(script_name, ".png"))
      if (suffix == "sunab") {
        output_file <- c(
          output_file,
          file.path(CONFIG$data_dir, paste0(script_name, "_att.rds"))
        )
      }

      ## did2s scripts are known to fail with did2s 1.0.2 + fixest 0.10.0
      ## (vcov dimension mismatch). Wrap in tryCatch so they do not halt the
      ## pipeline; sunab and CS scripts should still run.
      if (suffix == "did2s") {
        tryCatch(
          run_step(script_name, script_file,
                   deps = c("config.R", script_file, est_sample_path),
                   outputs = output_file),
          error = function(e) {
            emit_status(paste(script_name, "failed (non-fatal):", e$message),
                        level = "WARN")
            emit_status("did2s requires did2s >= 1.1.0 and fixest >= 0.11.0",
                        level = "WARN")
          }
        )
      } else {
        run_step(script_name, script_file,
                 deps = c("config.R", script_file, est_sample_path),
                 outputs = output_file)
      }
    }
  }

  sunab_att_files <- file.path(CONFIG$data_dir, paste0(modern_bases, "_sunab_att.rds"))
  twfe_att_files <- file.path(CONFIG$data_dir, c(
    "03_03_termination_twfe_att.rds",
    "03_04_new_hire_twfe_att.rds",
    "03_05_fmla_twfe_att.rds",
    "03_06_own_fmla_twfe_att.rds",
    "03_07_bereave_twfe_att.rds",
    "03_08_own_bereave_twfe_att.rds"
  ))
  run_step("03_10_sunab_summary",
           "03_10_sunab_summary.R",
           deps = c("config.R", "utils/sunab_utils.R", "03_10_sunab_summary.R", sunab_att_files, twfe_att_files),
           outputs = c(
             file.path(CONFIG$tables_dir, "03_10_staggered_att_peer.tex"),
             file.path(CONFIG$tables_dir, "03_10_staggered_att_own.tex")
           ))
}

#' -----------------------------------------------------------------------------
#' TIER 4: Main estimation
#' -----------------------------------------------------------------------------

estimate_path <- file.path(CONFIG$data_dir, "04_01_estimate.Rdata")
estimate_probit_path <- file.path(CONFIG$data_dir, "04_01_estimate_probit.Rdata")

if (RUN_ESTIMATION) {
  run_step("04_01_estimate",
           "04_01_estimate.R",
           deps = c("config.R", "04_01_estimate.R", est_sample_path),
           outputs = c(estimate_path, estimate_probit_path))
}

if (RUN_ESTIMATION) {
  run_step("04_02_estimate_many",
           "04_02_estimate_many.R",
           deps = c("config.R", "04_02_estimate_many.R", est_sample_path))
}

#' -----------------------------------------------------------------------------
#' TIER 5: Estimation analysis
#' -----------------------------------------------------------------------------

if (RUN_EST_ANALYSIS) {
  est_analysis_scripts <- c(
    "05_01_display",
    "05_02_validate_valuations",
    "05_03_cartel_demographics",
    "05_04_decomp_pref_network",
    "05_05_labor_supply"
  )

  est_analysis_outputs <- list(
    "05_01_display" = c(
      file.path(CONFIG$figures_dir, "05_01_officer_fe.png"),
      file.path(CONFIG$figures_dir, "05_01_date_fe.png")
    ),
    "05_02_validate_valuations" = c(
      file.path(CONFIG$data_dir, "05_02_val_special_events_blank.csv"),
      file.path(CONFIG$tables_dir, "05_02_top10_fixed_effects.tex"),
      file.path(CONFIG$tables_dir, "05_02_rain_dow_fe.tex")
    ),
    "05_03_cartel_demographics" = c(
      file.path(CONFIG$tables_dir, "05_03_cartel.tex"),
      file.path(CONFIG$figures_dir, "05_03_age_valuation.png"),
      file.path(CONFIG$figures_dir, "05_03_family_leave_valuation.png")
    ),
    "05_04_decomp_pref_network" = file.path(CONFIG$tables_dir, "05_04_decomp.tex")
  )

  for (script_name in est_analysis_scripts) {
    script_file <- paste0(script_name, ".R")
    run_step(script_name, script_file,
             deps = c("config.R", script_file, est_sample_path, estimate_path),
             outputs = est_analysis_outputs[[script_name]])
  }
}

#' -----------------------------------------------------------------------------
#' TIER 6: Counterfactual simulations
#' -----------------------------------------------------------------------------

sim_scripts <- c(
  "06_01_sim_random",
  "06_02_auction_sim",
  "06_03_sim_informal",
  "06_04_sim_informal_reverse",
  "06_05_sim_informal_perfect"
)

sim_outputs <- list(
  "06_01_sim_random" = c(
    file.path(CONFIG$data_dir, "06_01_sim_random.rds"),
    file.path(CONFIG$data_dir, "06_01_sim_random_byworker.rds")
  ),
  "06_02_auction_sim" = c(
    file.path(CONFIG$data_dir, "06_02_sim_auction_dev.rds"),
    file.path(CONFIG$data_dir, "06_02_sim_auction_dev_markdown.rds"),
    file.path(CONFIG$data_dir, "06_02_sim_auction_dev_byworker.rds"),
    file.path(CONFIG$data_dir, "06_02_sim_auction_straight.rds"),
    file.path(CONFIG$data_dir, "06_02_sim_auction_straight_wage.rds"),
    file.path(CONFIG$data_dir, "06_02_sim_auction_straight_byworker.rds")
  ),
  "06_03_sim_informal" = c(
    file.path(CONFIG$data_dir, "06_03_sim_informal.rds"),
    file.path(CONFIG$data_dir, "06_03_sim_informal_byworker.rds")
  ),
  "06_04_sim_informal_reverse" = c(
    file.path(CONFIG$data_dir, "06_04_sim_informal_reverse.rds"),
    file.path(CONFIG$data_dir, "06_04_sim_informal_reverse_byworker.rds")
  ),
  "06_05_sim_informal_perfect" = c(
    file.path(CONFIG$data_dir, "06_05_sim_informal_perfect.rds"),
    file.path(CONFIG$data_dir, "06_05_sim_informal_perfect_byworker.rds")
  )
)

sim_output_files <- unlist(sim_outputs, use.names = FALSE)
optional_frontier_path <- file.path(CONFIG$data_dir, "06_99_sim_frontier.rds")

if (RUN_SIMULATIONS) {
  ## Legacy frontier script 06_99_sim_frontier.R is kept for manual use,
  ## but the numbered analysis pipeline now runs the active 06_01-06_05 block only.
  for (script_name in sim_scripts) {
    script_file <- paste0(script_name, ".R")
    run_step(script_name, script_file,
             deps = c("config.R", script_file, est_sample_path, estimate_path),
             outputs = sim_outputs[[script_name]])
  }
}

#' -----------------------------------------------------------------------------
#' TIER 7: Simulation comparison
#' -----------------------------------------------------------------------------

tier7_sim_deps <- sim_output_files
if (file.exists(optional_frontier_path)) {
  tier7_sim_deps <- c(tier7_sim_deps, optional_frontier_path)
}

if (RUN_SIM_COMPARE) {
  heatmap_outputs <- c(
    file.path(CONFIG$figures_dir, "07_01_heatmap_continuous.png"),
    file.path(CONFIG$figures_dir, "07_01_heatmap.png"),
    file.path(CONFIG$figures_dir, "07_01_heatmap_less_granular.png")
  )

  run_step("07_01_heatmap",
           "07_01_heatmap.R",
           deps = c("config.R", "07_01_heatmap.R", est_sample_path, estimate_path, tier7_sim_deps),
           outputs = heatmap_outputs)

  run_step("07_02_compare_sims",
           "07_02_compare_sims.R",
           deps = c("config.R", "07_02_compare_sims.R", est_sample_path, estimate_path, tier7_sim_deps))
}

#' -----------------------------------------------------------------------------
#' SUMMARY
#' -----------------------------------------------------------------------------

pipeline_end <- Sys.time()
total_time <- difftime(pipeline_end, pipeline_start, units = "mins")
pipeline_success <- all(vapply(pipeline_results, function(r) isTRUE(r$success), logical(1)))

message("\n", strrep("=", 70))
message("ANALYSIS PIPELINE COMPLETE")
message(strrep("=", 70))
message("Total time: ", round(total_time, 2), " minutes")

for (name in names(pipeline_results)) {
  r <- pipeline_results[[name]]
  status_str <- if (isTRUE(r$skipped)) {
    "SKIPPED (up to date)"
  } else {
    format_pipeline_result(r)
  }
  message(sprintf("  %s: %s", name, status_str))
}

