rm(list = ls())
pipeline_start <- Sys.time()

find_project_root <- function(start_dir = getwd()) {
  current_dir <- normalizePath(start_dir, winslash = "/", mustWork = FALSE)
  repeat {
    if (file.exists(file.path(current_dir, "config.R")) &&
        dir.exists(file.path(current_dir, ".git"))) {
      return(current_dir)
    }
    parent_dir <- dirname(current_dir)
    if (identical(parent_dir, current_dir)) {
      stop("Could not locate project root from: ", start_dir)
    }
    current_dir <- parent_dir
  }
}

project_root <- find_project_root()
setwd(project_root)

source('config.R')
source('utils/logging.R')

steps <- c(
  '01_01_process_weather.R',
  '01_02_process_holidays.R',
  '01_03_mk_working.R',
  '01_04_mk_expanded_pay.R',
  '01_05_mk_pre_network.R'
)

step_outputs <- list(
  '01_01_process_weather.R' = file.path(CONFIG$data_dir, 'weather_daily.rds'),
  '01_02_process_holidays.R' = file.path(CONFIG$data_dir, 'holidays.rds'),
  '01_03_mk_working.R' = c(
    file.path(CONFIG$data_dir, 'employee_data.rds'),
    file.path(CONFIG$data_dir, 'workers_comp.rds'),
    file.path(CONFIG$data_dir, 'pay_data.rds')
  ),
  '01_04_mk_expanded_pay.R' = c(
    file.path(CONFIG$data_dir, '01_04_fornetwork.rds'),
    file.path(CONFIG$data_dir, 'working_expanded.rds')
  ),
  '01_05_mk_pre_network.R' = sapply(CONFIG$pre_network_windows, function(window) {
    file.path(CONFIG$data_dir, paste0(CONFIG$pre_network_prefix, window, '.csv'))
  })
)

results <- list()
for (step in steps) {
  start_time <- Sys.time()
  success <- FALSE
  error_msg <- NULL

  tryCatch({
    source(step, local = new.env(parent = globalenv()))
    assert_required_files(step_outputs[[step]])
    success <- TRUE
  }, error = function(e) {
    error_msg <<- conditionMessage(e)
  })

  duration <- as.numeric(difftime(Sys.time(), start_time, units = 'mins'))
  results[[step]] <- list(
    ran = TRUE,
    success = success,
    duration = duration,
    error = error_msg,
    skipped = FALSE
  )

  if (!success) {
    write_pipeline_summary(results, pipeline_start,
                           summary_name = 'run_stata_rewrites.log',
                           title = 'STATA REWRITE SUMMARY')
    stop(step, ' failed: ', error_msg)
  }
}

write_pipeline_summary(results, pipeline_start,
                       summary_name = 'run_stata_rewrites.log',
                       title = 'STATA REWRITE SUMMARY')