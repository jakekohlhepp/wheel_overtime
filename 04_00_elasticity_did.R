#' =============================================================================
#' ELASTICITY DIFFERENCE-IN-DIFFERENCES
#' =============================================================================
#' Uses salary step increases as quasi-exogenous variation in overtime wage
#' rates to estimate the labor supply elasticity via Callaway & Sant'Anna DiD.
#' Estimates overall effect and heterogeneity by network connectedness.
#'
#' Input:  20170803_payworkers_comp/anonymized_data_073117.txt (raw pay data)
#'         data/00_02_estimation_sample.rds (estimation sample)
#' Output: out/figures/04_00_*.png (event study plots)
#' =============================================================================

library('data.table')
library('stringr')
library('lubridate')
library('did')
library('ggplot2')

source('config.R')
source('utils/logging.R')

log_init("04_00_elasticity_did.R")
log_message("Running elasticity DiD analysis")

#' -----------------------------------------------------------------------------
#' CONSTRUCT PAY PERIOD PANEL
#' -----------------------------------------------------------------------------

raw_pay_path <- file.path(CONFIG$raw_pay_dir, "anonymized_data_073117.txt")
assert_required_files(raw_pay_path)

pay_recs <- fread(raw_pay_path)
pay_recs <- pay_recs[, .SD, .SDcols = colnames(pay_recs)[1:which(colnames(pay_recs) == "PAY_AMOUNT")]]
pay_recs[, dups := .N > 1, by = eval(colnames(pay_recs)[2:which(colnames(pay_recs) == "PAY_AMOUNT")])]
pay_recs[, dups := NULL]
pay_recs[, V1 := NULL]
pay_recs <- unique(pay_recs)
colnames(pay_recs) <- str_to_lower(colnames(pay_recs))
pay_recs <- pay_recs[job_class_title == 'TRAF OFFICER II']
pay_recs[, analysis_workdate := ymd(work_date)]

## Create pay period dates
pay_recs[, helper_end_date := max(analysis_workdate), by = c("payroll_year", "pp")]

hold <- as.Date(sapply(0:100, function(x) { return(14 * x + min(pay_recs$helper_end_date)) }))
stopifnot(weekdays(hold) == "Saturday")
hold <- data.table(end_date = hold, num_payperiod = 1:length(hold))
hold[, analysis_workdate := end_date]
pp_xwalk <- data.table()
for (i in 0:13) {
  hold[, analysis_workdate := end_date - i]
  pp_xwalk <- rbind(pp_xwalk, hold)
}
pay_recs <- merge(pay_recs, pp_xwalk, by = "analysis_workdate", all.x = TRUE)
stopifnot(!is.na(pay_recs[analysis_workdate >= CONFIG$fiscal_year_start]$end_date))

## Pay periods only well defined for 2014 fiscal year onwards
pay_recs <- pay_recs[analysis_workdate >= CONFIG$fiscal_year_start]

#' -----------------------------------------------------------------------------
#' ATTACH TENURE AND IDENTIFY STEP INCREASES
#' -----------------------------------------------------------------------------

tenure <- fread(raw_pay_path)
tenure <- unique(tenure[, c("EMPLOYEE_NAME", "WORK_DATE", "Years at Job on Workdate", "ORIGINAL_HIRE_DATE")])
stopifnot(uniqueN(tenure[, c("EMPLOYEE_NAME", "WORK_DATE")]) == nrow(tenure))
setnames(tenure, "WORK_DATE", "work_date")
setnames(tenure, "EMPLOYEE_NAME", "employee_name")
pay_recs <- merge(pay_recs, tenure, by = c("employee_name", "work_date"), all.x = TRUE)

## Create one record per pay period with pay rate
base_rate_codes <- c("CURRENT ACTUAL HOURS WORKED ONLY", "100% SICK TIME (CREDIT OR CHARGE)",
                     "HOLIDAY HOURS (CREDIT OR CHARGE)", "OVERTIME (1.5) WORKED AND PAID")

pay_recs <- pay_recs[variation_description %in% base_rate_codes, ]
pay_periods <- pay_recs[, .(max_rate = max(unique(var_rate)), min_rate = (unique(var_rate))),
                        by = c("end_date", "employee_name")]
setorder(pay_periods, "employee_name", "end_date")
pay_periods[, next_min := shift(min_rate, type = "lead"), by = "employee_name"]
## All within-period conflicts are resolved by a change in the next period
pay_periods[!(abs(max_rate - min_rate) <= 1e-05 | abs(next_min - max_rate) <= 1e-05), ]
## Set base rate to be maximum in period
pay_periods[, base_rate := max_rate]

## Examine all changes
setorder(pay_periods, "employee_name", "end_date")
pay_periods[, delta_base := base_rate - shift(base_rate), by = "employee_name"]
pay_periods[, delta_base_forward := shift(base_rate, type = "lead") - base_rate, by = "employee_name"]
pay_periods[, delta_time := end_date - shift(end_date), by = "employee_name"]

## Step increases: positive, above 0.5, and last period was 14 days prior
pay_periods[, step_increase := delta_base > 0.5 & delta_time == 14]
pay_periods[, num_emp1 := as.integer(gsub(CONFIG$employee_name_pattern, "", employee_name))]

#' -----------------------------------------------------------------------------
#' MERGE WITH ESTIMATION SAMPLE
#' -----------------------------------------------------------------------------

est_sample_path <- file.path(CONFIG$data_dir, "00_02_estimation_sample.rds")
assert_required_files(est_sample_path)

all_pairs <- readRDS(est_sample_path)
all_pairs <- merge(all_pairs, pp_xwalk, by = c("analysis_workdate"), all.x = TRUE)
stopifnot(!is.na(all_pairs$end_date))

all_pairs[, avg_wheel := mean(l_wheel_degree), by = "num_emp1"]
all_pairs <- merge(all_pairs, pay_periods[, c("step_increase", "end_date", "num_emp1", "delta_base")],
                   by = c("end_date", "num_emp1"), all.x = TRUE)
all_pairs[is.na(step_increase), step_increase := 0]

#' -----------------------------------------------------------------------------
#' DIFFERENCE-IN-DIFFERENCES: FULL SAMPLE
#' -----------------------------------------------------------------------------

for_did <- all_pairs[, .(tot_ot = sum(ot_work), step_increase = unique(step_increase),
                         size_increase = unique(delta_base)),
                     by = c("avg_wheel", "end_date", "num_payperiod", "num_emp1")]

log_message("Running Callaway-Sant'Anna DiD: full sample")

for_did[, when_treat := min(ifelse(step_increase, num_payperiod, Inf)), by = "num_emp1"]
for_did[is.infinite(when_treat), when_treat := 0]

example_attgt <- att_gt(yname = "tot_ot",
                        tname = "num_payperiod",
                        gname = "when_treat",
                        idname = "num_emp1",
                        data = for_did)

agg.es <- aggte(example_attgt, type = "dynamic", na.rm = TRUE)
summary(agg.es)

ensure_directory(CONFIG$figures_dir)
png(file.path(CONFIG$figures_dir, "04_00_elasticity_es_full.png"), width = 12, height = 8, units = "in", res = 150)
ggdid(agg.es) + scale_x_continuous(limits = c(-5, 5))
dev.off()

agg.simple <- aggte(example_attgt, type = "simple", na.rm = TRUE)
summary(agg.simple)

#' -----------------------------------------------------------------------------
#' DIFFERENCE-IN-DIFFERENCES: LOW CONNECTEDNESS
#' -----------------------------------------------------------------------------

log_message("Running Callaway-Sant'Anna DiD: low connectedness (avg_wheel <= 25)")

example_attgt <- att_gt(yname = "tot_ot",
                        tname = "num_payperiod",
                        gname = "when_treat",
                        idname = "num_emp1",
                        data = for_did[avg_wheel <= 25])

agg.simple <- aggte(example_attgt, type = "simple", na.rm = TRUE)
summary(agg.simple)

agg.es <- aggte(example_attgt, type = "dynamic", na.rm = TRUE)
summary(agg.es)

png(file.path(CONFIG$figures_dir, "04_00_elasticity_es_low_connect.png"), width = 12, height = 8, units = "in", res = 150)
ggdid(agg.es) + scale_x_continuous(limits = c(-5, 5))
dev.off()

#' -----------------------------------------------------------------------------
#' DIFFERENCE-IN-DIFFERENCES: HIGH CONNECTEDNESS
#' -----------------------------------------------------------------------------

log_message("Running Callaway-Sant'Anna DiD: high connectedness (avg_wheel >= 35)")

example_attgt <- att_gt(yname = "tot_ot",
                        tname = "num_payperiod",
                        gname = "when_treat",
                        idname = "num_emp1",
                        data = for_did[avg_wheel >= 35])

agg.simple <- aggte(example_attgt, type = "simple", na.rm = TRUE)
summary(agg.simple)

agg.es <- aggte(example_attgt, type = "dynamic", na.rm = TRUE)
summary(agg.es)

png(file.path(CONFIG$figures_dir, "04_00_elasticity_es_high_connect.png"), width = 12, height = 8, units = "in", res = 150)
ggdid(agg.es) + scale_x_continuous(limits = c(-5, 5))
dev.off()

#' -----------------------------------------------------------------------------
#' DIFFERENCE-IN-DIFFERENCES: RESTRICTED COHORTS
#' -----------------------------------------------------------------------------

log_message("Running Callaway-Sant'Anna DiD: restricted cohorts (low connectedness)")

example_attgt <- att_gt(yname = "tot_ot",
                        tname = "num_payperiod",
                        gname = "when_treat",
                        idname = "num_emp1",
                        data = for_did[when_treat %in% c(25, 22, 30, 0) & avg_wheel < 30, ])

summary(example_attgt)
agg.es <- aggte(example_attgt, type = "dynamic", na.rm = TRUE)
summary(agg.es)

log_complete(success = TRUE)
message("04_00_elasticity_did complete")
