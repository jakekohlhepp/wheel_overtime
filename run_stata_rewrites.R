rm(list = ls())
pipeline_start <- Sys.time()

source('config.R')
source('utils/logging.R')

steps <- c(
  'process_weather.R',
  'process_holidays.R',
  '01_01_mk_working.R',
  '01_02_mk_expanded_pay.R',
  '01_03_mk_pre_network.R'
)

results <- list()
for (step in steps) {
  start_time <- Sys.time()
  success <- FALSE
  error_msg <- NULL

  tryCatch({
    source(step, local = new.env(parent = globalenv()))
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
