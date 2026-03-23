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
RUN_ESTIMATION    <- TRUE
RUN_EST_ANALYSIS  <- TRUE
RUN_SIMULATIONS   <- TRUE
RUN_SIM_COMPARE   <- TRUE

pipeline_results <- list()

message("\n", strrep("=", 70))
message("ANALYSIS PIPELINE")
message(strrep("=", 70))
message("Start time: ", pipeline_start)

#' -----------------------------------------------------------------------------
#' Helper: run a single step
#' -----------------------------------------------------------------------------

run_step <- function(step_name, script, deps, outputs = NULL,
                     env = new.env(parent = globalenv())) {
  message("\n", strrep("-", 70))
  message("STEP: ", step_name)
  message(strrep("-", 70))

  if (needs_rerun(step_name, deps, outputs = outputs)) {
    step_start <- Sys.time()

    tryCatch({
      source(script, local = env)

      step_time <- difftime(Sys.time(), step_start, units = "mins")
      message(step_name, " complete (", round(step_time, 2), " minutes)")
      pipeline_results[[step_name]] <<- list(
        ran = TRUE, success = TRUE, duration = as.numeric(step_time),
        error = NULL, skipped = FALSE
      )

    }, error = function(e) {
      pipeline_results[[step_name]] <<- list(
        ran = TRUE, success = FALSE, duration = 0,
        error = e$message, skipped = FALSE
      )
      stop(step_name, " failed: ", e$message)
    })
  } else {
    message(step_name, " skipped (no changes detected)")
    pipeline_results[[step_name]] <<- list(
      ran = FALSE, success = TRUE, duration = 0, error = NULL, skipped = TRUE
    )
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

if (RUN_FACTS) {
  run_step("03_01_facts",
           "03_01_facts.R",
           deps = c("config.R", "03_01_facts.R", est_sample_path))
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

  for (script_name in event_scripts) {
    script_file <- paste0(script_name, ".R")
    run_step(script_name, script_file,
             deps = c("config.R", script_file, est_sample_path))
  }
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
    "05_03_cartel_age",
    "05_04_decomp_pref_network",
    "05_05_labor_supply"
  )

  for (script_name in est_analysis_scripts) {
    script_file <- paste0(script_name, ".R")
    run_step(script_name, script_file,
             deps = c("config.R", script_file, est_sample_path, estimate_path))
  }
}

#' -----------------------------------------------------------------------------
#' TIER 6: Counterfactual simulations
#' -----------------------------------------------------------------------------

if (RUN_SIMULATIONS) {
  run_step("06_01_sim_frontier",
           "06_01_sim_frontier.R",
           deps = c("config.R", "06_01_sim_frontier.R", est_sample_path, estimate_path),
           outputs = file.path(CONFIG$data_dir, "06_01_sim_frontier.rds"))

  sim_scripts <- c(
    "06_02_sim_random",
    "06_03_auction_sim",
    "06_04_sim_informal",
    "06_05_sim_informal_reverse",
    "06_06_sim_informal_perfect"
  )

  for (script_name in sim_scripts) {
    script_file <- paste0(script_name, ".R")
    run_step(script_name, script_file,
             deps = c("config.R", script_file, est_sample_path, estimate_path))
  }
}

#' -----------------------------------------------------------------------------
#' TIER 7: Simulation comparison
#' -----------------------------------------------------------------------------

if (RUN_SIM_COMPARE) {
  run_step("07_01_heatmap",
           "07_01_heatmap.R",
           deps = c("config.R", "07_01_heatmap.R", est_sample_path, estimate_path))

  run_step("07_02_compare_sims",
           "07_02_compare_sims.R",
           deps = c("config.R", "07_02_compare_sims.R", est_sample_path, estimate_path))
}

#' -----------------------------------------------------------------------------
#' SUMMARY
#' -----------------------------------------------------------------------------

pipeline_end <- Sys.time()
total_time <- difftime(pipeline_end, pipeline_start, units = "mins")
write_pipeline_summary(pipeline_results, pipeline_start,
                       summary_name = "run_analysis.log",
                       title = "ANALYSIS PIPELINE SUMMARY")

message("\n", strrep("=", 70))
message("ANALYSIS PIPELINE COMPLETE")
message(strrep("=", 70))
message("Total time: ", round(total_time, 2), " minutes")

for (name in names(pipeline_results)) {
  r <- pipeline_results[[name]]
  if (r$skipped) {
    status_str <- "SKIPPED (up to date)"
  } else if (r$success) {
    status_str <- sprintf("SUCCESS (%.2f min)", r$duration)
  } else {
    status_str <- sprintf("FAILURE: %s", r$error)
  }
  message(sprintf("  %s: %s", name, status_str))
}
