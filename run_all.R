#' =============================================================================
#' MASTER SCRIPT: Full Pipeline
#' =============================================================================
#' Runs the two master pipeline scripts in order:
#'   1. run_prep_data.R
#'   2. run_analysis.R
#'
#' Usage: source("run_all.R")
#' =============================================================================

rm(list = ls())
pipeline_start <- Sys.time()

source("config.R")
source("utils/logging.R")

ensure_directory(CONFIG$log_dir)

pipeline_results <- list()
pipeline_success <- FALSE

emit_status <- function(msg, level = "INFO") {
  message(msg)
  log_message(msg, level = level)
}

log_init("run_all.R")
on.exit({
  total_time <- as.numeric(difftime(Sys.time(), pipeline_start, units = "mins"))
  log_pipeline_summary(pipeline_results, pipeline_start,
                       title = "FULL PIPELINE SUMMARY")
  log_complete(success = pipeline_success, duration = total_time)
}, add = TRUE)

message("\n", strrep("=", 70))
message("FULL PIPELINE")
message(strrep("=", 70))
message("Start time: ", pipeline_start)
log_message("FULL PIPELINE")
log_message(paste("Start time:", format(pipeline_start, "%Y-%m-%d %H:%M:%S")))

run_master_step <- function(step_name, script,
                            env = new.env(parent = globalenv())) {
  message("\n", strrep("-", 70))
  message("STEP: ", step_name)
  message(strrep("-", 70))
  log_message(paste("STEP:", step_name))

  step_start <- Sys.time()
  emit_status(paste("Running", step_name))

  tryCatch({
    source(script, local = env)
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
}

run_master_step("run_prep_data", "run_prep_data.R")
run_master_step("run_analysis", "run_analysis.R")

pipeline_end <- Sys.time()
total_time <- difftime(pipeline_end, pipeline_start, units = "mins")
pipeline_success <- all(vapply(pipeline_results, function(r) isTRUE(r$success), logical(1)))

message("\n", strrep("=", 70))
message("FULL PIPELINE COMPLETE")
message(strrep("=", 70))
message("Total time: ", round(total_time, 2), " minutes")

for (name in names(pipeline_results)) {
  message(sprintf("  %s: %s", name, format_pipeline_result(pipeline_results[[name]])))
}
