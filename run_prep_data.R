#' =============================================================================
#' MASTER SCRIPT: Data Preparation
#' =============================================================================
#' Builds all intermediate data files from Stata outputs.
#' Stata scripts (01_01 through 01_03) are assumed to have already run.
#' Their outputs in data/ are taken as given.
#'
#' Usage: source("run_prep_data.R")
#' =============================================================================

rm(list = ls())
pipeline_start <- Sys.time()

source("config.R")
source("utils/logging.R")

ensure_directory(CONFIG$log_dir)

# Pipeline step toggles
RUN_NETWORK       <- TRUE
RUN_MAP           <- TRUE   ## requires internet (OSM + geocoding); set TRUE when needed

pipeline_results <- list()

message("\n", strrep("=", 70))
message("DATA PREPARATION PIPELINE")
message(strrep("=", 70))
message("Start time: ", pipeline_start)

#' -----------------------------------------------------------------------------
#' STEP 1: Network panels (30, 90, 180 day windows)
#' -----------------------------------------------------------------------------

if (RUN_NETWORK) {
  for (window in CONFIG$network_windows) {
    step_name <- paste0("prep_01_mk_network_", window)

    message("\n", strrep("-", 70))
    message("STEP: ", step_name, " (", window, "-day window)")
    message(strrep("-", 70))

    input_path <- get_network_input_path(window)
    step_deps <- c("config.R", "prep_01_mk_network.R", input_path)
    step_outputs <- get_network_output_path(window)

    if (needs_rerun(step_name, step_deps, outputs = step_outputs)) {
      step_start <- Sys.time()

      tryCatch({
        env <- new.env(parent = globalenv())
        env$NETWORK_WINDOW <- window
        source("prep_01_mk_network.R", local = env)

        step_time <- difftime(Sys.time(), step_start, units = "mins")
        message(step_name, " complete (", round(step_time, 2), " minutes)")
        pipeline_results[[step_name]] <- list(
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
      pipeline_results[[step_name]] <- list(
        ran = FALSE, success = TRUE, duration = 0, error = NULL, skipped = TRUE
      )
    }
  }
}

#' -----------------------------------------------------------------------------
#' STEP 2: Enforcement districts map
#' -----------------------------------------------------------------------------

if (RUN_MAP) {
  step_name <- "prep_02_mk_map"

  message("\n", strrep("-", 70))
  message("STEP: ", step_name)
  message(strrep("-", 70))

  step_deps <- c("config.R", "prep_02_mk_map.R",
                 file.path(CONFIG$data_dir, "01_05_list_complete.csv"))
  step_outputs <- CONFIG$map_output

  if (needs_rerun(step_name, step_deps, outputs = step_outputs)) {
    step_start <- Sys.time()

    tryCatch({
      source("prep_02_mk_map.R", local = new.env(parent = globalenv()))

      step_time <- difftime(Sys.time(), step_start, units = "mins")
      message(step_name, " complete (", round(step_time, 2), " minutes)")
      pipeline_results[[step_name]] <- list(
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
    pipeline_results[[step_name]] <- list(
      ran = FALSE, success = TRUE, duration = 0, error = NULL, skipped = TRUE
    )
  }
}

#' -----------------------------------------------------------------------------
#' SUMMARY
#' -----------------------------------------------------------------------------

pipeline_end <- Sys.time()
total_time <- difftime(pipeline_end, pipeline_start, units = "mins")
write_pipeline_summary(pipeline_results, pipeline_start,
                       summary_name = "run_prep_data.log",
                       title = "DATA PREPARATION SUMMARY")

message("\n", strrep("=", 70))
message("DATA PREPARATION COMPLETE")
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
