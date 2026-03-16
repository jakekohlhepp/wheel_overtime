#' =============================================================================
#' VALIDATE VALUATIONS
#' =============================================================================
#' Input:  data/02_00_estimate.Rdata
#'         data/00_02_estimation_sample.rds
#'         20250207_bss_special_events/Building_and_Safety_...csv
#'         data/02_02_val_special_events_filled.csv
#' Output: data/02_02_val_special_events_blank.csv
#'         out/tables/02_02_top10_fixed_effects.tex
#'         out/tables/02_02_rain_dow_fe.tex
#' =============================================================================

library('data.table')
library('lubridate')
library('stringr')
library('alpaca')
library('kableExtra')
library('fixest')

source('config.R')
source('utils/logging.R')

log_init("02_02_validate_valuations.R")
log_message("Starting valuation validation")

#' ---------------------------------------------------------------------------
#' LOAD DATA AND COMPUTE DATE FIXED EFFECTS
#' ---------------------------------------------------------------------------

load(file.path(CONFIG$data_dir, "02_00_estimate.Rdata"))
all_pairs <- readRDS(file.path(CONFIG$data_dir, "00_02_estimation_sample.rds"))
## compute average ot hours.
avg_othours <- mean(all_pairs[ot_work == 1]$varot_hours)

date_fe <- data.table(val_date = getFEs(mod_mod)$analysis_workdate / coef(mod_mod)['ot_rate'] * avg_othours, analysis_workdate = as.Date(names(getFEs(mod_mod)$analysis_workdate)))

## just date vars.
date_fe <- merge(date_fe, unique(all_pairs[, c("analysis_workdate", "prcp", "tmin", "tmax", "tot_ot", "is_holiday", "dw")]), by = "analysis_workdate", all.x = TRUE)
date_fe[, rain := prcp > 0]
date_fe[, r_date_fe := resid(feols(val_date ~ 1 | tot_ot, data = date_fe))]

#' ---------------------------------------------------------------------------
#' SPECIAL EVENTS ANALYSIS
#' ---------------------------------------------------------------------------

log_message("Processing special events data")

special <- fread(file.path(CONFIG$raw_special_events_dir, "Building_and_Safety_Temporary_Special_Event__TSE__Permits_20250207.csv"))

special[, start_date := mdy(`Event Start Date`)]
special[, end_date := mdy(`Event End Date`)]
special <- special[, .(analysis_workdate = as.Date(start_date:end_date)), by = eval(colnames(special))]
special[, `Event Name` := str_to_upper(`Event Name`)]
special <- unique(special[, c("start_date", "end_date", "PCIS Permit #", "analysis_workdate", "Event Name")])
special <- unique(special[, c("analysis_workdate", "Event Name")])
## collapse to unique by date, concat all events
special <- special[, .(event_list = paste0(`Event Name`, collapse = "", sep = " || ")), by = "analysis_workdate"]

special <- merge(special, date_fe, by = "analysis_workdate", all.x = TRUE)
setorder(special, -"r_date_fe")

ensure_directory(CONFIG$data_dir)
write.csv(special[!is.na(r_date_fe), c("r_date_fe", "analysis_workdate", "event_list")], file = file.path(CONFIG$data_dir, "02_02_val_special_events_blank.csv"))
log_message("Wrote blank special events CSV for manual fill-in")

#' ---------------------------------------------------------------------------
#' TOP 10 FIXED EFFECTS TABLE
#' ---------------------------------------------------------------------------

log_message("Building top 10 fixed effects table")

withnames <- fread(file.path(CONFIG$data_dir, "02_02_val_special_events_filled.csv"))
withnames[, analysis_workdate := mdy(analysis_workdate)]
setorder(withnames, -"r_date_fe")
withnames <- merge(withnames[, -"r_date_fe"], special[!is.na(r_date_fe)][1:10, c("analysis_workdate", "tot_ot", "r_date_fe")], by = "analysis_workdate", all.y = TRUE)
withnames[, analysis_workdate := format(analysis_workdate, "%B %d, %Y")]
setorder(withnames, -"r_date_fe")
withnames <- withnames[, c("analysis_workdate", "notable_event", "r_date_fe", "tot_ot")]
withnames[, r_date_fe := paste0("\\$", as.character(sprintf("%.2f", round(r_date_fe, 2))))]
colnames(withnames) <- c("Date", "Notable Event", "Fixed Effect Earnings Equivalent", "Total Overtime")

ensure_directory(CONFIG$tables_dir)

## do ot_count, valuation, name.
kable(withnames, "latex", align = "c", booktabs = TRUE, linesep = c(""), escape = F, caption = NA, label = NA) %>%
  cat(., file = file.path(CONFIG$tables_dir, "02_02_top10_fixed_effects.tex"))
log_message("Saved top 10 fixed effects table")

#' ---------------------------------------------------------------------------
#' RAIN AND DAY-OF-WEEK TABLE
#' ---------------------------------------------------------------------------

log_message("Building rain and day-of-week table")

## do rain, day of the week.
## do count, mean, sd.

rain_table <- date_fe[, .(mean_fe = mean(r_date_fe, na.rm = TRUE), mean_count = mean(tot_ot, na.rm = TRUE), count = .N), by = "rain"]
rain_table[rain == TRUE, name := "Rain"]
rain_table[rain == FALSE, name := "No Rain"]

dayofweek <- date_fe[, .(mean_fe = mean(r_date_fe, na.rm = TRUE), mean_count = mean(tot_ot, na.rm = TRUE), count = .N), by = "dw"]
setnames(dayofweek, "dw", "name")
fortable <- rbind(rain_table[, -"rain"], dayofweek)
fortable[, mean_fe := paste0("\\$", as.character(sprintf("%.2f", round(mean_fe, 2))))]
fortable[, mean_count := paste0(as.character(sprintf("%.2f", round(mean_count, 2))))]
colnames(fortable) <- c("Avg. Fixed Effect", "Avg. Overtime Shifts", "Number of Dates", "Type of Day")
kable(fortable[, c("Type of Day", "Number of Dates", "Avg. Fixed Effect", "Avg. Overtime Shifts")], "latex", align = "c", booktabs = TRUE, linesep = c(""), escape = F, caption = NA, label = NA) %>%
  cat(., file = file.path(CONFIG$tables_dir, "02_02_rain_dow_fe.tex"))

log_message("Saved rain and day-of-week table")
log_complete(success = TRUE)
