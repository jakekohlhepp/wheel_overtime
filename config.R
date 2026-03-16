#' =============================================================================
#' MODEL CONFIGURATION
#' =============================================================================
#' Centralized configuration for the LADOT Traffic Officer OT project.
#' Every script sources this file: source('config.R')
#' =============================================================================

CONFIG <- list(

  # ---------------------------------------------------------------------------
  # Logging and execution control
  # ---------------------------------------------------------------------------
  log_dir = Sys.getenv("WHEEL_LOG_DIR", unset = "logs"),
  force_rerun = FALSE,
  verbose_logging = TRUE,

  # ---------------------------------------------------------------------------
  # Data paths
  # ---------------------------------------------------------------------------
  data_dir = Sys.getenv("WHEEL_DATA_DIR", unset = "data"),
  raw_pay_dir = "20170803_payworkers_comp",
  raw_weather_dir = "20190811_weather",
  raw_holidays_dir = "20190814_fed_holidays",
  raw_districts_dir = "20250311_ladot_enforcement_districts",
  raw_office_dir="20250320_division_names_offices",
  raw_special_events_dir = "20250207_bss_special_events",
  output_dir = Sys.getenv("WHEEL_OUT_DIR", unset = "out"),

  # ---------------------------------------------------------------------------
  # Core dates and thresholds from the original Stata pipeline
  # ---------------------------------------------------------------------------
  fiscal_year_start = as.Date("2014-07-01"),
  fiscal_year_end = as.Date("2016-06-30"),
  injury_window_start = as.Date("2015-01-01"),
  gap_group_days = 31,
  max_work_rate = 34,
  weather_station_name = "LOS ANGELES DOWNTOWN USC, CA US",
  pre_network_windows = c(30, 90, 180, 1000),
  stata_daily_origin = as.Date("1960-01-01"),
  stata_week_origin = as.Date("1960-01-03"),

  # ---------------------------------------------------------------------------
  # Network construction parameters
  # ---------------------------------------------------------------------------
  ## Rolling window sizes (days) for exposure matrices
  ## The Stata script 01_03 creates pre_network CSVs for each window size.
  ## The main estimation uses 90; 30 and 180 are robustness checks.
  network_windows = c(30, 90, 180),
  network_window_default = 90,

  ## Gap threshold: drop observation-days in gaps >= this many days
  gap_threshold = 30,

  ## Employee name pattern in raw data (anonymized)
  employee_name_pattern = "EMPLOYEE ",

  ## OT premium multiplier for expected earnings calculation
  ot_premium = 1.5,

  ## Exposure column pattern in pre_network CSVs
  exposure_pattern = "roll[0-9]+_exposure",

  # ---------------------------------------------------------------------------
  # Network input/output file patterns
  # ---------------------------------------------------------------------------
  ## Input: data/{pre_network_prefix}{window}.csv
  pre_network_prefix = "01_03_pre_network_",
  ## Output: data/{panel_output_prefix}{suffix}.rds
  ## suffix is "" for 90-day (default), "_30" for 30-day, "_180" for 180-day
  panel_output_prefix = "prep_01_panel_working",

  # ---------------------------------------------------------------------------
  # Map script parameters
  # ---------------------------------------------------------------------------
  districts_layer = "LADOT_Parking_Enforcement_Districts",
  map_div_code_min = 808,
  map_width = 12,
  map_height = 12,
  map_output = "out/figures/prep_02_la_street_map.png",

  # ---------------------------------------------------------------------------
  # Estimation sample parameters
  # ---------------------------------------------------------------------------
  estimation_start = as.Date("2015-01-01"),
  estimation_end = as.Date("2016-06-30"),
  estimation_years = c(2015, 2016),

  ## Expected estimation sample size (assertion)
  expected_estimation_rows = 274908,
  expected_estimation_officers = 535,

  # ---------------------------------------------------------------------------
  # Analysis parameters
  # ---------------------------------------------------------------------------
  ## Termination date cutoff for event studies
  termination_cutoff = as.Date("2016-06-01"),

  # ---------------------------------------------------------------------------
  # Output directories
  # ---------------------------------------------------------------------------
  tables_dir = "out/tables",
  figures_dir = "out/figures",

  # ---------------------------------------------------------------------------
  # Column names (standardized references)
  # ---------------------------------------------------------------------------
  merge_vars = c("num_emp1", "analysis_workdate", "tot_hours", "leave_hours",
                 "matched_injury", "normal_work", "tot_ot", "ot_work",
                 "seniority_rank", "an_age", "max_rate", "medpd",
                 "claimcause", "prcp", "tmax", "tmin", "varot_hours")
)

#' Helper: build path relative to project root
project_path <- function(...) {
  file.path(...)
}

#' Helper: ensure directory exists
ensure_directory <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  invisible(path)
}

#' Helper: assert files exist or stop with informative error
assert_required_files <- function(paths) {
  missing <- paths[!file.exists(paths)]
  if (length(missing) > 0) {
    stop("Missing required file(s):\n  ", paste(missing, collapse = "\n  "))
  }
  invisible(paths)
}

#' Helper: assert required columns exist in a data.table
assert_required_columns <- function(dt, required_cols, object_name = "data.table") {
  missing <- setdiff(required_cols, names(dt))
  if (length(missing) > 0) {
    stop(object_name, " is missing required columns: ", paste(missing, collapse = ", "))
  }
  invisible(TRUE)
}

#' Helper: get network input path for a given window size
get_network_input_path <- function(window, config = CONFIG) {
  file.path(config$data_dir, paste0(config$pre_network_prefix, window, ".csv"))
}

#' Helper: get network output path for a given window size
get_network_output_path <- function(window, config = CONFIG) {
  suffix <- if (window == config$network_window_default) "" else paste0("_", window)
  file.path(config$data_dir, paste0(config$panel_output_prefix, suffix, ".rds"))
}

#' Helper: load the contact matrix from a network panel
#' Returns a data.table with num_emp1, analysis_workdate, and numeric officer-ID columns
load_contact_matrix <- function(window = CONFIG$network_window_default) {
  panel_path <- get_network_output_path(window)
  if (!file.exists(panel_path)) stop("Panel not found: ", panel_path)
  dt <- readRDS(panel_path)
  id_cols <- grep("^[0-9]+$", names(dt), value = TRUE)
  if (length(id_cols) == 0) {
    ## Contact matrix was cleaned from panel; load from pre-network CSV
    input_path <- get_network_input_path(window)
    if (!file.exists(input_path)) stop("Pre-network CSV not found: ", input_path)
    raw <- data.table::fread(input_path)
    raw[, analysis_workdate := lubridate::dmy(analysis_workdate)]
    raw[, num_emp1 := as.integer(gsub(CONFIG$employee_name_pattern, "", employee_name))]
    exposure_cols <- grep(CONFIG$exposure_pattern, names(raw), value = TRUE)
    ## Pivot exposure columns to officer-ID columns (same format as old panel)
    ## Actually, the pre-network has roll{window}_exposure_{id} columns
    ## Extract officer IDs from exposure column names
    id_pattern <- paste0("roll", window, "_exposure_")
    officer_ids <- gsub(id_pattern, "", exposure_cols)
    raw_sub <- raw[, .SD, .SDcols = c("num_emp1", "analysis_workdate", exposure_cols)]
    setnames(raw_sub, exposure_cols, officer_ids)
    return(raw_sub)
  }
  dt[, .SD, .SDcols = c("num_emp1", "analysis_workdate", id_cols)]
}

stata_daily_num <- function(x, config = CONFIG) {
  as.numeric(as.Date(x) - config$stata_daily_origin)
}

stata_month_num <- function(x) {
  x <- as.Date(x)
  (as.integer(format(x, "%Y")) - 1960L) * 12L + as.integer(format(x, "%m")) - 1L
}

stata_week_num <- function(x, config = CONFIG) {
  x <- as.Date(x)
  week_start <- x - as.POSIXlt(x, tz = "UTC")$wday
  as.integer((week_start - config$stata_week_origin) / 7)
}

format_stata_date <- function(x) {
  out <- rep("", length(x))
  keep <- !is.na(x)
  out[keep] <- tolower(format(as.Date(x[keep]), "%d%b%Y"))
  out
}

format_stata_datetime <- function(x) {
  out <- rep("", length(x))
  keep <- !is.na(x)
  out[keep] <- tolower(format(as.POSIXct(x[keep], tz = "UTC"), tz = "UTC", "%d%b%Y %H:%M:%S"))
  out
}

format_stata_week <- function(x) {
  out <- rep("", length(x))
  keep <- !is.na(x)
  out[keep] <- paste0(format(as.Date(x[keep]), "%Y"), "w", format(as.Date(x[keep]), "%U"))
  out
}
