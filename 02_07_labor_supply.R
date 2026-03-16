#' =============================================================================
#' LABOR SUPPLY ANALYSIS
#' =============================================================================
#' Analyzes labor supply elasticity by computing dollar-equivalent valuations
#' and examining family leave and age heterogeneity.
#'
#' Input:  data/02_00_estimate.Rdata        (logit model)
#'         data/00_02_estimation_sample.rds  (estimation sample)
#' Output: (analysis only, no saved files)
#' =============================================================================

library('data.table')
library('alpaca')
library('lubridate')

source('config.R')
source('utils/logging.R')

log_init("02_07_labor_supply.R")
log_message("Running labor supply analysis")

set.seed(660062)

#' -----------------------------------------------------------------------------
#' LOAD DATA AND MODEL
#' -----------------------------------------------------------------------------

load(file.path(CONFIG$data_dir, "02_00_estimate.Rdata"))

all_pairs <- readRDS(file.path(CONFIG$data_dir, "00_02_estimation_sample.rds"))

#' -----------------------------------------------------------------------------
#' COMPUTE VALUATIONS
#' -----------------------------------------------------------------------------

## Average OT hours per shift
avg_ot_hours <- sum(all_pairs$varot_hours) / sum(all_pairs$ot_work)

officer_fe <- data.table(officer_fe = getFEs(mod_mod)$num_emp1, num_emp1 = as.numeric(names(getFEs(mod_mod)$num_emp1)))
all_pairs <- merge(all_pairs, officer_fe, by = "num_emp1", all.x = TRUE)
date_fe <- data.table(date_fe = getFEs(mod_mod)$analysis_workdate, analysis_workdate = as.Date(names(getFEs(mod_mod)$analysis_workdate)))
all_pairs <- merge(all_pairs, date_fe, by = "analysis_workdate", all.x = TRUE)

## Exclude officers without fixed effects
print(uniqueN(all_pairs[is.na(officer_fe)]$num_emp1) / uniqueN(all_pairs$num_emp1))
all_pairs <- all_pairs[!is.na(officer_fe), ]

#' -----------------------------------------------------------------------------
#' COMPUTE DETERMINISTIC VALUATION
#' -----------------------------------------------------------------------------

all_pairs[, det_val := ((date_fe + officer_fe + seniority_rank * coef(mod_mod)["seniority_rank"] +
            normal_work * coef(mod_mod)["normal_work"]) / coef(mod_mod)["ot_rate"] + ot_rate) * avg_ot_hours]

## Family leave indicator
all_pairs[, has_family := max(family_leave, na.rm = TRUE), by = "num_emp1"]

## Age on estimation start
all_pairs[, age_on_start := an_age - (analysis_workdate - CONFIG$estimation_start) / 365.25, by = "num_emp1"]
stopifnot(all_pairs[, .(check = uniqueN(round(age_on_start, digits = 8))), by = "num_emp1"]$check == 1)
all_pairs[, age_on_start := as.numeric(max(age_on_start)), by = "num_emp1"]

#' -----------------------------------------------------------------------------
#' SUMMARIZE BY OFFICER
#' -----------------------------------------------------------------------------

by_emp <- all_pairs[, .(observed_ot_count = sum(ot_work), valuation = mean(det_val),
                        avg_degree = mean(l_degree),
                        avg_suppliers = mean(l_wheel_degree),
                        barrier_avg = mean(coef(mod_mod)['opp_dist'] * opp_dist + coef(mod_mod)['suppliers_interacted'] * suppliers_interacted),
                        avg_seniority = mean(seniority_rank)),
                    by = c("num_emp1", "has_family", "age_on_start")]

log_message(paste("Officer summary:", nrow(by_emp), "officers"))
log_complete(success = TRUE)
message("02_07_labor_supply complete")
