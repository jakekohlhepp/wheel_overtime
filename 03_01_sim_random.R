#' =============================================================================
#' RANDOM ASSIGNMENT SIMULATION
#' =============================================================================
#' Input:  file.path(CONFIG$data_dir, "02_00_estimate.Rdata")
#'         file.path(CONFIG$data_dir, "00_01_estimation_sample.rds")
#' Output: file.path(CONFIG$data_dir, "03_01_sim_random.rds")
#'         file.path(CONFIG$data_dir, "03_01_sim_random_byworker.rds")
#' =============================================================================

library('alpaca')
library('data.table')
library('parallel')
library('lubridate')
library('stats')

source('config.R')
source('utils/logging.R')
log_init("03_01_sim_random.R")

#' ---------------------------------------------------------------------------
#' LOAD DATA
#' ---------------------------------------------------------------------------

set.seed(477812)
log_message("Loading estimation data and sample")
load(file.path(CONFIG$data_dir, "02_00_estimate.Rdata"))

all_pairs <- readRDS(file.path(CONFIG$data_dir, "00_01_estimation_sample.rds"))
officer_fe <- data.table(officer_fe = getFEs(mod_mod)$num_emp1, num_emp1 = as.numeric(names(getFEs(mod_mod)$num_emp1)))
all_pairs <- merge(all_pairs, officer_fe, by = "num_emp1", all.x = TRUE)
date_fe <- data.table(date_fe = getFEs(mod_mod)$analysis_workdate, analysis_workdate = as.Date(names(getFEs(mod_mod)$analysis_workdate)))
all_pairs <- merge(all_pairs, date_fe, by = "analysis_workdate", all.x = TRUE)

#' ---------------------------------------------------------------------------
#' PREPARE DATA
#' ---------------------------------------------------------------------------

## exclude 7 officers without fixed effects
log_message(paste0("Fraction officers without FE: ", uniqueN(all_pairs[is.na(officer_fe)]$num_emp1) / uniqueN(all_pairs$num_emp1)))
all_pairs <- all_pairs[!is.na(officer_fe), ]

## Extract coefficients once
beta_sr  <- coef(mod_mod)["seniority_rank"]
beta_nw  <- coef(mod_mod)["normal_work"]
beta_ot  <- coef(mod_mod)["ot_rate"]
beta_od  <- coef(mod_mod)["opp_dist"]
beta_si  <- coef(mod_mod)["suppliers_interacted"]

all_pairs[, tot_ot_among := sum(ot_work), by = "analysis_workdate"]
## utility is sum of all components including wages and degrees.
all_pairs[, det_util := opp_dist * beta_od + suppliers_interacted * beta_si + date_fe + officer_fe +
            seniority_rank * beta_sr + normal_work * beta_nw + ot_rate * beta_ot]

### for each date, compute the total ot hours and total ot instances.
## assign based on number of instances, assume even hours distribution
all_pairs[, all_othours := sum(varot_hours), by = "analysis_workdate"]

## total ot on each date
all_pairs[, tot_ot_among := sum(ot_work), by = "analysis_workdate"]
setkey(all_pairs, "analysis_workdate", "num_emp1")

#' ---------------------------------------------------------------------------
#' RUN SIMULATION (parallel)
#' ---------------------------------------------------------------------------

max_iter <- 2500

## Slim down to needed columns
keep_cols <- c("num_emp1", "analysis_workdate", "ot_rate", "det_util",
               "opp_dist", "suppliers_interacted",
               "tot_ot_among", "all_othours", "ot_work")
ap_slim <- all_pairs[, ..keep_cols]

## Generate per-iteration seeds
iter_seeds <- sample.int(.Machine$integer.max, max_iter)

run_one_random_iter <- function(iter, ap, beta_ot, beta_od, beta_si) {
  library(data.table)
  dt <- copy(ap)
  n <- nrow(dt)

  ## those who work are just those randomly with the highest value
  dt[, rand_assign := runif(n)]
  dt[, sim_work := frank(rand_assign, ties.method = "random") <= tot_ot_among, by = "analysis_workdate"]

  ## draw logit shock
  dt[, true_utility := det_util + rlogis(n)]
  dt[, sim_win_wage := ot_rate]

  ## non-wage utility delivered
  dt[, true_valuation := (true_utility -
                           (ot_rate * beta_ot + opp_dist * beta_od + suppliers_interacted * beta_si)) / beta_ot]

  dt[, sim_value := sim_work * true_valuation]
  dt[, sim_payment := fifelse(tot_ot_among > 0L, sim_win_wage * sim_work * all_othours / tot_ot_among, 0)]
  dt[, worker_surplus := sim_work * true_valuation]

  byemp <- dt[, .(ot_tot = sum(sim_work)), by = "num_emp1"]
  setorder(byemp, "ot_tot", "num_emp1")
  byemp[, position := (1:.N) / .N]
  byemp[, cum_ot := cumsum(ot_tot) / sum(ot_tot)]
  byemp[, is_90th := position >= 0.9 & shift(position) < 0.9]
  stopifnot(nrow(byemp[is_90th == TRUE]) == 1)

  res <- data.table(sim_num = iter,
                    worker_value = sum(dt$sim_value),
                    worker_surplus = sum(dt$worker_surplus),
                    wage_bill = sum(dt$sim_payment),
                    share_top10 = 1 - byemp[is_90th == TRUE]$cum_ot[1])

  res_bw <- dt[sim_work == TRUE, .(sim_num = iter,
                                    total_ot = sum(sim_work),
                                    worker_surplus = sum(worker_surplus),
                                    total_ot_pay = sum(sim_payment),
                                    max_win_wage = max(sim_win_wage),
                                    min_win_wage = min(sim_win_wage),
                                    avg_win_wage = mean(sim_win_wage)),
               by = "num_emp1"]

  list(res = res, res_bw = res_bw)
}

n_cores <- min(detectCores() - 1, 10)
log_message(paste0("Starting random assignment simulation with ", max_iter, " iterations on ", n_cores, " cores"))

cl <- makeCluster(n_cores)
clusterExport(cl, c("ap_slim", "beta_ot", "beta_od", "beta_si", "run_one_random_iter", "iter_seeds"), envir = environment())

out <- parLapply(cl, seq_len(max_iter), function(iter) {
  set.seed(iter_seeds[iter])
  run_one_random_iter(iter, ap_slim, beta_ot, beta_od, beta_si)
})

stopCluster(cl)
log_message("Parallel run complete, collecting results")

results         <- rbindlist(lapply(out, `[[`, "res"))
results_byworker <- rbindlist(lapply(out, `[[`, "res_bw"))
rm(out); gc()

setorder(results, "sim_num")
setorder(results_byworker, "sim_num", "num_emp1")

#' ---------------------------------------------------------------------------
#' SAVE RESULTS
#' ---------------------------------------------------------------------------

ensure_directory(CONFIG$data_dir)
log_message("Saving simulation results")
saveRDS(results, file.path(CONFIG$data_dir, "03_01_sim_random.rds"))
saveRDS(results_byworker, file.path(CONFIG$data_dir, "03_01_sim_random_byworker.rds"))

log_complete(success = TRUE)
