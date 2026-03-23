#' =============================================================================
#' ESTIMATE MAIN MODEL
#' =============================================================================
#' Input:  data/02_01_estimation_sample.rds
#' Output: data/04_01_estimate.Rdata, data/04_01_estimate_probit.Rdata
#' =============================================================================

library('data.table')
library('alpaca')

source('config.R')
source('utils/logging.R')

log_init("04_01_estimate.R")
log_message("Starting main estimation")

#' ---------------------------------------------------------------------------
#' LOAD DATA
#' ---------------------------------------------------------------------------

set.seed(633491)
all_pairs <- readRDS(file.path(CONFIG$data_dir, "02_01_estimation_sample.rds"))

#' ---------------------------------------------------------------------------
#' LOGIT ESTIMATION
#' ---------------------------------------------------------------------------

log_message("Estimating logit with two-way fixed effects")

## estimate logit with two-way fixed effects
mod <- alpaca::feglm(ot_work ~ opp_dist + suppliers_interacted + ot_rate + seniority_rank + normal_work | num_emp1 + analysis_workdate,
                     all_pairs, family = binomial("logit"))
mod_mod <- alpaca::biasCorr(mod)
summary(mod_mod, type = "clustered", cluster = ~num_emp1)

ensure_directory(CONFIG$data_dir)
save(mod_mod, file = file.path(CONFIG$data_dir, "04_01_estimate.Rdata"))
log_message("Saved logit estimates")

#' ---------------------------------------------------------------------------
#' PROBIT ESTIMATION
#' ---------------------------------------------------------------------------

log_message("Estimating probit with two-way fixed effects")

## to be comprehensive we also include probit
mod_probit <- alpaca::feglm(ot_work ~ opp_dist + suppliers_interacted + ot_rate + seniority_rank + normal_work | num_emp1 + analysis_workdate,
                     all_pairs, family = binomial("probit"))
mod_probit_mod <- alpaca::biasCorr(mod_probit)
summary(mod_probit_mod, type = "clustered", cluster = ~num_emp1)

save(mod_probit_mod, file = file.path(CONFIG$data_dir, "04_01_estimate_probit.Rdata"))
log_message("Saved probit estimates")

log_complete(success = TRUE)
