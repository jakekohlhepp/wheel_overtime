#' =============================================================================
#' AUCTION SIMULATION
#' =============================================================================
#' Simulate a k-wage auction, where officers bid for shifts each day.
#' Includes both deviation-from-base and straight shift auction variants.
#' Input:  file.path(CONFIG$data_dir, "04_01_estimate.Rdata")
#'         file.path(CONFIG$data_dir, "02_01_estimation_sample.rds")
#' Output: file.path(CONFIG$data_dir, "06_03_sim_auction_dev.rds")
#'         file.path(CONFIG$data_dir, "06_03_sim_auction_dev_markdown.rds")
#'         file.path(CONFIG$data_dir, "06_03_sim_auction_dev_byworker.rds")
#'         file.path(CONFIG$data_dir, "06_03_sim_auction_straight.rds")
#'         file.path(CONFIG$data_dir, "06_03_sim_auction_straight_wage.rds")
#'         file.path(CONFIG$data_dir, "06_03_sim_auction_straight_byworker.rds")
#' =============================================================================

library('data.table')
library('alpaca')
library('lubridate')
library('parallel')

source('config.R')
source('utils/logging.R')
log_init("06_03_auction_sim.R")

#' ---------------------------------------------------------------------------
#' LOAD DATA
#' ---------------------------------------------------------------------------

set.seed(660062)
log_message("Loading estimation data and sample")
load(file.path(CONFIG$data_dir, "04_01_estimate.Rdata"))

all_pairs <- readRDS(file.path(CONFIG$data_dir, "02_01_estimation_sample.rds"))

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

### for each date, compute the total ot hours and total ot instances.
## assign based on number of instances, assume even hours distribution
all_pairs[, all_othours := sum(varot_hours), by = "analysis_workdate"]
all_pairs[, tot_ot_among := sum(ot_work), by = "analysis_workdate"]

## set number of iterations
max_iter <- 2500

## Extract coefficients once (avoid repeated coef() calls in loop)
beta_sr  <- coef(mod_mod)["seniority_rank"]
beta_nw  <- coef(mod_mod)["normal_work"]
beta_ot  <- coef(mod_mod)["ot_rate"]

## deterministic portion of valuation is seniority rank part plus normal work plus person fe.
all_pairs[, det_val := date_fe + officer_fe + seniority_rank * beta_sr +
            normal_work * beta_nw]

#' ---------------------------------------------------------------------------
#' HELPER: run a single iteration of an auction simulation
#' ---------------------------------------------------------------------------

run_one_dev_iter <- function(iter, ap, beta_ot) {
  library(data.table)
  dt <- copy(ap)
  n <- nrow(dt)

  dt[, true_valuation := (det_val + rlogis(n)) / beta_ot]
  dt[, valuation := true_valuation + ot_rate]

  setorder(dt, "analysis_workdate", -"valuation")
  dt[, sim_work := (1:.N <= tot_ot_among), by = "analysis_workdate"]
  dt[, sim_value := sim_work * true_valuation]
  dt[, sim_markdown := max(ifelse(((1:.N) == (tot_ot_among + 1)), valuation, NA), na.rm = TRUE), by = "analysis_workdate"]
  dt[sim_markdown == -Inf, sim_markdown := NA_real_]
  dt[, sim_win_wage := ot_rate - sim_markdown]
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

  res_wage <- unique(dt[tot_ot_among > 0, c("analysis_workdate", "sim_markdown")])

  list(res = res, res_bw = res_bw, res_wage = res_wage)
}

run_one_straight_iter <- function(iter, ap, beta_ot) {
  library(data.table)
  dt <- copy(ap)
  n <- nrow(dt)

  dt[, true_valuation := (det_val + rlogis(n)) / beta_ot]
  dt[, valuation := true_valuation]

  setorder(dt, "analysis_workdate", -"valuation")
  dt[, sim_work := (1:.N <= tot_ot_among), by = "analysis_workdate"]
  dt[, sim_value := sim_work * true_valuation]
  dt[, sim_win_wage := max(ifelse(((1:.N) == (tot_ot_among + 1)), valuation, NA), na.rm = TRUE), by = "analysis_workdate"]
  dt[sim_win_wage == -Inf, sim_win_wage := NA_real_]
  dt[, sim_payment := fifelse(tot_ot_among > 0L, -(sim_win_wage) * sim_work * all_othours / tot_ot_among, 0)]
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

  res_wage <- unique(dt[tot_ot_among > 0, c("analysis_workdate", "sim_win_wage")])

  list(res = res, res_bw = res_bw, res_wage = res_wage)
}

#' ---------------------------------------------------------------------------
#' SETUP PARALLEL CLUSTER
#' ---------------------------------------------------------------------------

n_cores <- min(detectCores() - 1, 10)
log_message(paste0("Setting up parallel cluster with ", n_cores, " cores"))

## Slim down all_pairs to only needed columns before sending to workers
keep_cols <- c("num_emp1", "analysis_workdate", "ot_rate", "det_val",
               "tot_ot_among", "all_othours")
ap_slim <- all_pairs[, ..keep_cols]

## Generate per-iteration seeds from the master RNG for reproducibility
dev_seeds <- sample.int(.Machine$integer.max, max_iter)
straight_seeds <- sample.int(.Machine$integer.max, max_iter)

#' ---------------------------------------------------------------------------
#' AUCTION 1: DEVIATION-FROM-BASE RATE (parallel)
#' ---------------------------------------------------------------------------

log_message("Starting deviation-from-base auction simulation")

cl <- makeCluster(n_cores)
clusterExport(cl, c("ap_slim", "beta_ot", "run_one_dev_iter", "dev_seeds"), envir = environment())

dev_out <- parLapply(cl, seq_len(max_iter), function(iter) {
  set.seed(dev_seeds[iter])
  run_one_dev_iter(iter, ap_slim, beta_ot)
})

stopCluster(cl)
log_message("Deviation auction parallel run complete, collecting results")

results         <- rbindlist(lapply(dev_out, `[[`, "res"))
results_byworker <- rbindlist(lapply(dev_out, `[[`, "res_bw"))
results_wage     <- rbindlist(lapply(dev_out, `[[`, "res_wage"))
rm(dev_out); gc()

setorder(results, "sim_num")
setorder(results_byworker, "sim_num", "num_emp1")
setorder(results_wage, "analysis_workdate")

ensure_directory(CONFIG$data_dir)
log_message("Saving deviation auction results")
saveRDS(results, file.path(CONFIG$data_dir, "06_03_sim_auction_dev.rds"))
saveRDS(results_wage, file.path(CONFIG$data_dir, "06_03_sim_auction_dev_markdown.rds"))
saveRDS(results_byworker, file.path(CONFIG$data_dir, "06_03_sim_auction_dev_byworker.rds"))

#' ---------------------------------------------------------------------------
#' AUCTION 2: STRAIGHT SHIFT AUCTION (parallel)
#' ---------------------------------------------------------------------------

log_message("Starting straight shift auction simulation")

cl <- makeCluster(n_cores)
clusterExport(cl, c("ap_slim", "beta_ot", "run_one_straight_iter", "straight_seeds"), envir = environment())

straight_out <- parLapply(cl, seq_len(max_iter), function(iter) {
  set.seed(straight_seeds[iter])
  run_one_straight_iter(iter, ap_slim, beta_ot)
})

stopCluster(cl)
log_message("Straight auction parallel run complete, collecting results")

results         <- rbindlist(lapply(straight_out, `[[`, "res"))
results_byworker <- rbindlist(lapply(straight_out, `[[`, "res_bw"))
results_wage     <- rbindlist(lapply(straight_out, `[[`, "res_wage"))
rm(straight_out); gc()

setorder(results, "sim_num")
setorder(results_byworker, "sim_num", "num_emp1")
setorder(results_wage, "analysis_workdate")

log_message("Saving straight auction results")
saveRDS(results, file.path(CONFIG$data_dir, "06_03_sim_auction_straight.rds"))
saveRDS(results_wage, file.path(CONFIG$data_dir, "06_03_sim_auction_straight_wage.rds"))
saveRDS(results_byworker, file.path(CONFIG$data_dir, "06_03_sim_auction_straight_byworker.rds"))

log_complete(success = TRUE)
