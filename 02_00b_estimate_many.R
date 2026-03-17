#' =============================================================================
#' ESTIMATE TIME-VARYING MODEL
#' =============================================================================
#' Input:  data/00_01_estimation_sample.rds
#'         data/{pre_network_prefix}{network_window_default}.csv
#' Output: (plots displayed interactively)
#' =============================================================================

library('data.table')
library('alpaca')
library('lubridate')
library('stringr')

source('config.R')
source('utils/logging.R')

log_init("02_00b_estimate_many.R")
log_message("Starting time-varying estimation")

#' ---------------------------------------------------------------------------
#' LOAD DATA
#' ---------------------------------------------------------------------------

set.seed(633491)
all_pairs <- readRDS(file.path(CONFIG$data_dir, "00_01_estimation_sample.rds"))

# original hire date
rawer_data <- fread(file.path(CONFIG$data_dir, paste0(CONFIG$pre_network_prefix, CONFIG$network_window_default, ".csv")))
rawer_data[, analysis_workdate := dmy(analysis_workdate)]
rawer_data[, num_emp1 := as.integer(gsub(CONFIG$employee_name_pattern, "", employee_name))]
rawer_data <- unique(rawer_data[, c("original_hire_date", "num_emp1")])
stopifnot(uniqueN(rawer_data$num_emp1) == nrow(rawer_data))
all_pairs <- merge(all_pairs, rawer_data, by = "num_emp1", all.x = TRUE)

#' ---------------------------------------------------------------------------
#' CONSTRUCT TIME INDICATORS
#' ---------------------------------------------------------------------------

log_message("Constructing month indicators")

# month indicators
all_pairs[, original_hire_date := date(dmy_hms(original_hire_date))]
all_pairs[, month_since := pmin(floor(as.numeric(analysis_workdate - original_hire_date) / 90), 12)]
all_pairs[, month_since := floor_date(analysis_workdate, unit = "month")]

#' ---------------------------------------------------------------------------
#' ESTIMATE TIME-VARYING MODEL
#' ---------------------------------------------------------------------------

log_message("Estimating logit with time-varying coefficients")

## estimate logit with two-way fixed effects
mod <- alpaca::feglm(ot_work ~ as.factor(month_since):opp_dist + factor(month_since):suppliers_interacted + ot_rate + seniority_rank + normal_work | num_emp1 + analysis_workdate,
                     all_pairs, family = binomial("logit"))

res <- data.table(val = coef(mod)[str_detect(names(coef(mod)), "^as.factor")],
                  month_since = names(coef(mod))[str_detect(names(coef(mod)), "^as.factor")])
res[, month_since := str_replace(month_since, "as.factor\\(month_since\\)", "")]
res[, month_since := str_replace(month_since, ":opp_dist", "")]
res[, month_since := as.Date(month_since)]
plot(res[, c("month_since", "val")])

#' ---------------------------------------------------------------------------
#' MERGE WITH CENTRALITY
#' ---------------------------------------------------------------------------

log_message("Merging with centrality measures")

res <- merge(res, all_pairs[, .(avg_centrality = mean(l_degree)), by = "month_since"], by = "month_since", all.x = TRUE)
plot(res[, c("avg_centrality", "val")])

log_message("Time-varying estimation complete")
log_complete(success = TRUE)
