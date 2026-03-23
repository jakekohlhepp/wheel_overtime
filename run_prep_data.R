#' =============================================================================
#' MASTER SCRIPT: Data Preparation
#' =============================================================================
#' Builds all intermediate data files from raw inputs through network panels.
#'
#' Phase A: Raw data processing (weather, holidays, payroll, exposure matrices)
#' Phase B: Network panel construction + map
#'
#' Usage: source("run_prep_data.R")
#' =============================================================================

rm(list = ls())
pipeline_start <- Sys.time()

source("config.R")
source("utils/logging.R")

ensure_directory(CONFIG$log_dir)
ensure_directory(CONFIG$data_dir)
ensure_directory(CONFIG$output_dir)

# Pipeline step toggles
RUN_WEATHER       <- TRUE
RUN_HOLIDAYS      <- TRUE
RUN_RAW_SPLIT     <- TRUE   ## 01_01: split raw .dta into employee/workers_comp/pay
RUN_EXPAND        <- TRUE   ## 01_02: expand pay to daily panel + merge weather/holidays
RUN_PRE_NETWORK   <- TRUE   ## 01_03: build exposure matrices for all windows
RUN_NETWORK       <- TRUE   ## prep_01: network panels (30, 90, 180 day windows)
RUN_MAP           <- TRUE   ## prep_02: enforcement districts map (requires internet)

pipeline_results <- list()

message("\n", strrep("=", 70))
message("DATA PREPARATION PIPELINE")
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

#' =============================================================================
#' PHASE A: Raw data processing
#' =============================================================================

# -- Paths used across Phase A steps --
raw_dta_path     <- file.path(CONFIG$raw_pay_dir, "data", "anonymized_data_073117.dta")
info_path        <- file.path(CONFIG$output_dir, "list_var_desc.csv")
weather_csv_path <- file.path(CONFIG$raw_weather_dir, "data", "1834210.csv")
holidays_csv_path <- file.path(CONFIG$raw_holidays_dir, "data",
                               "us-federal-holidays-2011-2020.csv")

weather_dta      <- file.path(CONFIG$data_dir, "weather_daily.dta")
holidays_dta     <- file.path(CONFIG$data_dir, "holidays.dta")
employee_dta     <- file.path(CONFIG$data_dir, "employee_data.dta")
workers_dta      <- file.path(CONFIG$data_dir, "workers_comp.dta")
pay_dta          <- file.path(CONFIG$data_dir, "pay_data.dta")
working_dta      <- file.path(CONFIG$data_dir, "working_expanded.dta")
fornetwork_dta   <- file.path(CONFIG$data_dir, "01_02_fornetwork.dta")

#' -----------------------------------------------------------------------------
#' A1: Process weather data
#' -----------------------------------------------------------------------------

if (RUN_WEATHER) {
  run_step("process_weather",
           "process_weather.R",
           deps = c("config.R", "process_weather.R", weather_csv_path),
           outputs = weather_dta)
}

#' -----------------------------------------------------------------------------
#' A2: Process holidays
#' -----------------------------------------------------------------------------

if (RUN_HOLIDAYS) {
  run_step("process_holidays",
           "process_holidays.R",
           deps = c("config.R", "process_holidays.R", holidays_csv_path),
           outputs = holidays_dta)
}

#' -----------------------------------------------------------------------------
#' A3: Split raw data into employee, workers_comp, pay tables (01_01)
#' -----------------------------------------------------------------------------

if (RUN_RAW_SPLIT) {
  run_step("01_01_mk_working",
           "01_01_mk_working.R",
           deps = c("config.R", "01_01_mk_working.R", raw_dta_path, info_path),
           outputs = c(employee_dta, workers_dta, pay_dta))
}

#' -----------------------------------------------------------------------------
#' A4: Expand pay to daily panel + merge weather/holidays (01_02)
#' -----------------------------------------------------------------------------

if (RUN_EXPAND) {
  run_step("01_02_mk_expanded_pay",
           "01_02_mk_expanded_pay.R",
           deps = c("config.R", "01_02_mk_expanded_pay.R",
                    pay_dta, workers_dta, employee_dta,
                    weather_dta, holidays_dta, raw_dta_path),
           outputs = c(working_dta, fornetwork_dta))
}

#' -----------------------------------------------------------------------------
#' A5: Build exposure matrices for all windows (01_03)
#' -----------------------------------------------------------------------------

if (RUN_PRE_NETWORK) {
  pre_net_outputs <- sapply(CONFIG$pre_network_windows, function(w) {
    file.path(CONFIG$data_dir, paste0(CONFIG$pre_network_prefix, w, ".csv"))
  })

  run_step("01_03_mk_pre_network",
           "01_03_mk_pre_network.R",
           deps = c("config.R", "01_03_mk_pre_network.R",
                    working_dta, pay_dta, fornetwork_dta, employee_dta),
           outputs = pre_net_outputs)
}

#' =============================================================================
#' PHASE B: Network panel construction
#' =============================================================================

#' -----------------------------------------------------------------------------
#' B1: Network panels (30, 90, 180 day windows)
#' -----------------------------------------------------------------------------

if (RUN_NETWORK) {
  for (window in CONFIG$network_windows) {
    step_name <- paste0("prep_01_mk_network_", window)

    input_path <- get_network_input_path(window)
    step_deps <- c("config.R", "prep_01_mk_network.R", input_path)
    step_outputs <- get_network_output_path(window)

    env <- new.env(parent = globalenv())
    env$NETWORK_WINDOW <- window

    run_step(step_name, "prep_01_mk_network.R",
             deps = step_deps, outputs = step_outputs, env = env)
  }
}

#' -----------------------------------------------------------------------------
#' B2: Enforcement districts map
#' -----------------------------------------------------------------------------

if (RUN_MAP) {
  raw_pay_txt <- file.path(CONFIG$raw_pay_dir, "anonymized_data_073117.txt")
  list_path   <- file.path(CONFIG$raw_office_dir, "list_complete.csv")

  run_step("prep_02_mk_map",
           "prep_02_mk_map.R",
           deps = c("config.R", "prep_02_mk_map.R", raw_pay_txt, list_path),
           outputs = CONFIG$map_output)
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
