#' =============================================================================
#' LAG CHECK REGRESSIONS
#' =============================================================================
#' Input:  data/02_01_estimation_sample.rds
#' Output: out/tables/01_01_lag_regs_suppliers.tex
#'         out/tables/01_01_lag_regs.tex
#' =============================================================================

source('config.R')
source('utils/logging.R')
log_init("03_02_lag_check.R")

#' ---------------------------------------------------------------------------
#' LOAD PACKAGES
#' ---------------------------------------------------------------------------

library('fixest')
library('data.table')
set.seed(633491)

#' ---------------------------------------------------------------------------
#' LOAD DATA
#' ---------------------------------------------------------------------------

log_message("Loading estimation sample")
all_pairs <- readRDS(file.path(CONFIG$data_dir, "02_01_estimation_sample.rds"))

#' ---------------------------------------------------------------------------
#' LAGGED SUPPLIER REGRESSIONS
#' ---------------------------------------------------------------------------

log_message("Running lagged supplier regressions")
setorder(all_pairs, "num_emp1", "analysis_workdate")
all_pairs[, s_suppliers := suppliers_interacted / sd(suppliers_interacted, na.rm = TRUE)]
all_pairs[, l_suppliers_7 := shift(suppliers_interacted, 7), by = "num_emp1"]
all_pairs[, s_suppliers_7 := l_suppliers_7 / sd(l_suppliers_7, na.rm = TRUE)]
all_pairs[, l_suppliers_14 := shift(suppliers_interacted, 14), by = "num_emp1"]
all_pairs[, s_suppliers_14 := l_suppliers_14 / sd(l_suppliers_14, na.rm = TRUE)]
all_pairs[, l_suppliers_21 := shift(suppliers_interacted, 21), by = "num_emp1"]
all_pairs[, s_suppliers_21 := l_suppliers_21 / sd(l_suppliers_21, na.rm = TRUE)]
all_pairs[, l_suppliers_28 := shift(suppliers_interacted, 28), by = "num_emp1"]
all_pairs[, s_suppliers_28 := l_suppliers_28 / sd(l_suppliers_28, na.rm = TRUE)]

res1 <- feols(ot_work ~ opp_dist + s_suppliers + normal_work + seniority_rank + ot_rate | num_emp1 + analysis_workdate, data = all_pairs, cluster = "num_emp1")
res2 <- feols(ot_work ~ opp_dist + s_suppliers_7 + normal_work + seniority_rank + ot_rate | num_emp1 + analysis_workdate, data = all_pairs, cluster = "num_emp1")
res3 <- feols(ot_work ~ opp_dist + s_suppliers_14 + normal_work + seniority_rank + ot_rate | num_emp1 + analysis_workdate, data = all_pairs, cluster = "num_emp1")
res4 <- feols(ot_work ~ opp_dist + s_suppliers_21 + normal_work + seniority_rank + ot_rate | num_emp1 + analysis_workdate, data = all_pairs, cluster = "num_emp1")
res5 <- feols(ot_work ~ opp_dist + s_suppliers_28 + normal_work + seniority_rank + ot_rate | num_emp1 + analysis_workdate, data = all_pairs, cluster = "num_emp1")

ensure_directory(CONFIG$tables_dir)
etable(res1, res2, res3, res4, res5, fitstat = ~r2, keep = "!Constant", order = "Supplier",
       dict = c(s_suppliers = "Std. Supplier Count",
                s_suppliers_7 = "Std. Supplier Count (Lag 7)",
                s_supplier_14 = "Std. Supplier Count (Lag 14)",
                s_supplier_21 = "Std. Supplier Count (Lag 21)",
                s_supplier_28 = "Std. Supplier Count (Lag 28)",
                opp_dist = "Distance from Wheel Median",
                normal_work = "Normal Work", an_age = "Age", seniority_rank = "Seniority Rank", ot_work = "Overtime",
                expected_earnings = "Expected Earnings", analysis_workdate = "Date", num_emp1 = "Officer"),
       file = file.path(CONFIG$tables_dir, "01_01_lag_regs_suppliers.tex"), replace = TRUE,
       signifCode = c(`***` = 0.001, `**` = 0.01, `*` = 0.05))

#' ---------------------------------------------------------------------------
#' LAGGED CONNECTEDNESS REGRESSIONS
#' ---------------------------------------------------------------------------

log_message("Running lagged connectedness regressions")
setorder(all_pairs, "num_emp1", "analysis_workdate")
all_pairs[, s_degree := l_degree / sd(l_degree, na.rm = TRUE)]
all_pairs[, l_degree_7 := shift(l_degree, 7), by = "num_emp1"]
all_pairs[, s_degree_7 := l_degree_7 / sd(l_degree_7, na.rm = TRUE)]
all_pairs[, l_degree_14 := shift(l_degree, 14), by = "num_emp1"]
all_pairs[, s_degree_14 := l_degree_14 / sd(l_degree_14, na.rm = TRUE)]
all_pairs[, l_degree_21 := shift(l_degree, 21), by = "num_emp1"]
all_pairs[, s_degree_21 := l_degree_21 / sd(l_degree_21, na.rm = TRUE)]
all_pairs[, l_degree_28 := shift(l_degree, 28), by = "num_emp1"]
all_pairs[, s_degree_28 := l_degree_28 / sd(l_degree_28, na.rm = TRUE)]

res1 <- feols(ot_work ~ opp_dist + s_degree + normal_work + seniority_rank + ot_rate | num_emp1 + analysis_workdate, data = all_pairs, cluster = "num_emp1")
res2 <- feols(ot_work ~ opp_dist + s_degree_7 + normal_work + seniority_rank + ot_rate | num_emp1 + analysis_workdate, data = all_pairs, cluster = "num_emp1")
res3 <- feols(ot_work ~ opp_dist + s_degree_14 + normal_work + seniority_rank + ot_rate | num_emp1 + analysis_workdate, data = all_pairs, cluster = "num_emp1")
res4 <- feols(ot_work ~ opp_dist + s_degree_21 + normal_work + seniority_rank + ot_rate | num_emp1 + analysis_workdate, data = all_pairs, cluster = "num_emp1")
res5 <- feols(ot_work ~ opp_dist + s_degree_28 + normal_work + seniority_rank + ot_rate | num_emp1 + analysis_workdate, data = all_pairs, cluster = "num_emp1")

etable(res1, res2, res3, res4, res5, fitstat = ~r2, keep = "!Constant", order = "Connectedness",
       dict = c(s_degree = "Std. Connectedness",
                s_degree_7 = "Std. Connectedness (Lag 7)",
                s_degree_14 = "Std. Connectedness (Lag 14)",
                s_degree_21 = "Std. Connectedness (Lag 21)",
                s_degree_28 = "Std. Connectedness (Lag 28)",
                opp_dist = "Distance from Wheel Median",
                normal_work = "Normal Work", an_age = "Age", seniority_rank = "Seniority Rank", ot_work = "Overtime",
                expected_earnings = "Expected Earnings", analysis_workdate = "Date", num_emp1 = "Officer"),
       file = file.path(CONFIG$tables_dir, "01_01_lag_regs.tex"), replace = TRUE,
       signifCode = c(`***` = 0.001, `**` = 0.01, `*` = 0.05))

log_message("03_02_lag_check.R completed successfully")
log_complete(success = TRUE)
