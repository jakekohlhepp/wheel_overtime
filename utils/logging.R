#' =============================================================================
#' LOGGING UTILITIES
#' =============================================================================
#' Provides logging infrastructure for pipeline scripts.
#' Tracks script execution with timestamps to enable conditional re-runs.
#'
#' Usage:
#'   source("utils/logging.R")
#'   log_init("my_script.R")
#'   log_message("Processing data...", "INFO")
#'   log_complete(success = TRUE)
#' =============================================================================

.log_env <- new.env(parent = emptyenv())
.log_env$current_script <- NULL
.log_env$current_log_path <- NULL
.log_env$start_time <- NULL

get_log_dir <- function() {
  if (exists("CONFIG") && !is.null(CONFIG$log_dir)) return(CONFIG$log_dir)
  return("logs")
}

get_log_path <- function(script_name) {
  base <- sub("\\.R$", "", basename(script_name))
  file.path(get_log_dir(), paste0(base, ".log"))
}

log_init <- function(script_name) {
  log_dir <- get_log_dir()
  if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)

  .log_env$current_script <- script_name
  .log_env$current_log_path <- get_log_path(script_name)
  .log_env$start_time <- Sys.time()

  header <- paste0(
    strrep("=", 60), "\n",
    "Script: ", script_name, "\n",
    "Started: ", format(.log_env$start_time, "%Y-%m-%d %H:%M:%S"), "\n",
    "Completed: RUNNING\n",
    "Status: RUNNING\n",
    "Duration: RUNNING\n",
    strrep("=", 60), "\n"
  )
  writeLines(header, .log_env$current_log_path)
  invisible(NULL)
}

log_message <- function(msg, level = "INFO") {
  if (is.null(.log_env$current_log_path)) {
    warning("log_message called before log_init")
    return(invisible(NULL))
  }
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_line <- sprintf("[%s] %s: %s\n", timestamp, level, msg)
  cat(log_line, file = .log_env$current_log_path, append = TRUE)

  if (exists("CONFIG") && isTRUE(CONFIG$verbose_logging)) {
    message(sprintf("[%s] %s", level, msg))
  }
  invisible(NULL)
}

log_complete <- function(success, duration = NULL) {
  if (is.null(.log_env$current_log_path)) {
    warning("log_complete called before log_init")
    return(invisible(NULL))
  }

  end_time <- Sys.time()
  if (is.null(duration)) {
    duration <- as.numeric(difftime(end_time, .log_env$start_time, units = "mins"))
  }

  status <- if (success) "SUCCESS" else "FAILURE"
  log_content <- readLines(.log_env$current_log_path)
  header_end <- which(log_content == strrep("=", 60))[2]
  if (is.na(header_end)) header_end <- length(log_content)
  messages <- if (header_end < length(log_content)) {
    log_content[(header_end + 1):length(log_content)]
  } else { character(0) }

  header <- paste0(
    strrep("=", 60), "\n",
    "Script: ", .log_env$current_script, "\n",
    "Started: ", format(.log_env$start_time, "%Y-%m-%d %H:%M:%S"), "\n",
    "Completed: ", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n",
    "Status: ", status, "\n",
    "Duration: ", sprintf("%.2f minutes", duration), "\n",
    strrep("=", 60)
  )
  writeLines(c(header, messages), .log_env$current_log_path)

  .log_env$current_script <- NULL
  .log_env$current_log_path <- NULL
  .log_env$start_time <- NULL
  invisible(NULL)
}

parse_log_metadata <- function(script_name) {
  log_path <- get_log_path(script_name)
  if (!file.exists(log_path)) return(list(exists = FALSE))

  log_lines <- readLines(log_path, n = 10)
  completed_line <- grep("^Completed:", log_lines, value = TRUE)
  status_line <- grep("^Status:", log_lines, value = TRUE)

  if (length(completed_line) == 0 || length(status_line) == 0) {
    return(list(exists = TRUE, status = "UNKNOWN", completed_time = NULL))
  }

  status <- trimws(sub("^Status:", "", status_line[1]))
  completed_str <- trimws(sub("^Completed:", "", completed_line[1]))
  completed_time <- if (completed_str == "RUNNING") NULL else {
    tryCatch(as.POSIXct(completed_str, format = "%Y-%m-%d %H:%M:%S"),
             error = function(e) NULL)
  }

  list(exists = TRUE, status = status, completed_time = completed_time)
}

needs_rerun <- function(script_name, dependencies = NULL, outputs = NULL) {
  if (exists("CONFIG") && isTRUE(CONFIG$force_rerun)) {
    message("  -> Force rerun enabled, will run ", script_name)
    return(TRUE)
  }

  meta <- parse_log_metadata(script_name)
  if (!meta$exists) { message("  -> No log for ", script_name); return(TRUE) }
  if (meta$status != "SUCCESS") { message("  -> Previous: ", meta$status); return(TRUE) }
  if (is.null(meta$completed_time)) { message("  -> No timestamp"); return(TRUE) }

  ## Check that all expected outputs exist
  if (!is.null(outputs)) {
    missing <- outputs[!file.exists(outputs)]
    if (length(missing) > 0) {
      message("  -> Missing output(s): ", paste(missing, collapse = ", "))
      return(TRUE)
    }
  }

  log_time <- meta$completed_time

  if (file.exists(script_name) && file.mtime(script_name) > log_time) {
    message("  -> ", script_name, " modified"); return(TRUE)
  }

  if (!is.null(dependencies)) {
    for (dep in dependencies) {
      if (file.exists(dep) && file.mtime(dep) > log_time) {
        message("  -> Dependency ", dep, " modified"); return(TRUE)
      }
    }
  }

  message("  -> ", script_name, " is up to date")
  return(FALSE)
}

run_with_logging <- function(script_name, dependencies = NULL, outputs = NULL, force = FALSE) {
  if (!force && !needs_rerun(script_name, dependencies, outputs)) {
    return(list(ran = FALSE, success = TRUE, duration = 0, error = NULL, skipped = TRUE))
  }

  log_init(script_name)
  log_message(paste("Starting", script_name))
  start_time <- Sys.time()
  success <- FALSE
  error_msg <- NULL

  tryCatch({
    withCallingHandlers({
      source(script_name, local = new.env(parent = globalenv()))
      success <- TRUE
      log_message(paste("Completed", script_name))
    }, warning = function(w) {
      log_message(paste("WARNING:", conditionMessage(w)), "WARN")
      invokeRestart("muffleWarning")
    })
  }, error = function(e) {
    error_msg <<- conditionMessage(e)
    log_message(paste("ERROR:", error_msg), "ERROR")
  })

  duration <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
  log_complete(success, duration)
  list(ran = TRUE, success = success, duration = duration,
       error = error_msg, skipped = FALSE)
}

write_pipeline_summary <- function(results, pipeline_start,
                                   summary_name = "run_all.log",
                                   title = "PIPELINE SUMMARY") {
  summary_path <- file.path(get_log_dir(), summary_name)
  end_time <- Sys.time()
  total_duration <- as.numeric(difftime(end_time, pipeline_start, units = "mins"))
  all_success <- all(sapply(results, function(r) r$success))
  overall_status <- if (all_success) "SUCCESS" else "FAILURE"

  lines <- c(
    strrep("=", 60), title,
    paste("Started:", format(pipeline_start, "%Y-%m-%d %H:%M:%S")),
    paste("Completed:", format(end_time, "%Y-%m-%d %H:%M:%S")),
    paste("Status:", overall_status),
    paste("Total Duration:", sprintf("%.2f minutes", total_duration)),
    strrep("=", 60), "", "Step Results:", strrep("-", 40)
  )
  for (name in names(results)) {
    r <- results[[name]]
    if (r$skipped) status_str <- "SKIPPED"
    else if (r$success) status_str <- sprintf("SUCCESS (%.2f min)", r$duration)
    else status_str <- sprintf("FAILURE: %s", r$error)
    lines <- c(lines, sprintf("  %s: %s", name, status_str))
  }
  lines <- c(lines, "", strrep("=", 60))
  writeLines(lines, summary_path)
  invisible(NULL)
}
