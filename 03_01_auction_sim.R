#' =============================================================================
#' AUCTION SIMULATION
#' =============================================================================
#' Simulate a k-wage auction, where officers bid for shifts each day.
#' Includes both deviation-from-base and straight shift auction variants.
#' Input:  file.path(CONFIG$data_dir, "02_00_estimate.Rdata")
#'         file.path(CONFIG$data_dir, "00_02_estimation_sample.rds")
#' Output: file.path(CONFIG$data_dir, "03_01_sim_auction_dev.rds")
#'         file.path(CONFIG$data_dir, "03_01_sim_auction_dev_markdown.rds")
#'         file.path(CONFIG$data_dir, "03_01_sim_auction_dev_byworker.rds")
#'         file.path(CONFIG$data_dir, "03_01_sim_auction_straight.rds")
#'         file.path(CONFIG$data_dir, "03_01_sim_auction_straight_wage.rds")
#'         file.path(CONFIG$data_dir, "03_01_sim_auction_straight_byworker.rds")
#' =============================================================================

library('data.table')
library('alpaca')
library('lubridate')

source('config.R')
source('utils/logging.R')
log_init("03_01_auction_sim.R")

#' ---------------------------------------------------------------------------
#' LOAD DATA
#' ---------------------------------------------------------------------------

set.seed(660062)
log_message("Loading estimation data and sample")
load(file.path(CONFIG$data_dir, "02_00_estimate.Rdata"))

all_pairs <- readRDS(file.path(CONFIG$data_dir, "00_02_estimation_sample.rds"))

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

## deterministic portion of valuation is seniority rank part plus normal work plus person fe.
all_pairs[, det_val := date_fe + officer_fe + seniority_rank * coef(mod_mod)["seniority_rank"] +
            normal_work * coef(mod_mod)["normal_work"]]

#' ---------------------------------------------------------------------------
#' AUCTION 1: DEVIATION-FROM-BASE RATE
#' ---------------------------------------------------------------------------

log_message("Starting deviation-from-base auction simulation")
results <- data.table()
results_wage <- data.table()
results_byworker <- data.table()

for (iter in 1:max_iter) {
  if (iter %% 100 == 0) log_message(paste0("Deviation auction iteration: ", iter))

  all_pairs[, true_valuation := (det_val + rlogis(.N)) / (coef(mod_mod)["ot_rate"])]
  all_pairs[, valuation := true_valuation + ot_rate]

  setorder(all_pairs, "analysis_workdate", -"valuation")
  # for each date, take the first tot_ot_among based on available officers
  all_pairs[, sim_work := (1:.N <= tot_ot_among), by = "analysis_workdate"]
  all_pairs[, sim_value := sim_work * true_valuation, by = "analysis_workdate"]
  # the wage reduction is equal to the k+1 highest valuation
  all_pairs[, sim_markdown := max(ifelse(((1:.N) == (tot_ot_among + 1)), valuation, NA), na.rm = TRUE), by = "analysis_workdate"]
  all_pairs[, sim_win_wage := ot_rate - sim_markdown]

  all_pairs[, sim_payment := (sim_win_wage) * sim_work * all_othours / tot_ot_among]
  # worker surplus is then true valuation plus actual wage
  all_pairs[, worker_surplus := sim_work * (true_valuation + sim_win_wage)]

  ## no auction winner does not want to refuse the shift.
  stopifnot(all_pairs[sim_work == 1]$worker_surplus > 0)

  byemp <- all_pairs[, .(ot_tot = sum(sim_work)), by = "num_emp1"]
  setorder(byemp, "ot_tot", "num_emp1")
  byemp[, position := (1:.N) / .N]
  byemp[, cum_ot := cumsum(ot_tot) / sum(ot_tot)]
  byemp[, is_90th := position >= 0.9 & shift(position) < 0.9]
  stopifnot(nrow(byemp[is_90th == 1]) == 1)

  results <- rbind(results, data.table(sim_num = iter,
                                       worker_value = sum(all_pairs$sim_value),
                                       worker_surplus = sum(all_pairs$worker_surplus),
                                       wage_bill = sum(all_pairs$sim_payment),
                                       share_top10 = 1 - byemp[is_90th == 1]$cum_ot[1]))
  results_byworker <- rbind(results_byworker, all_pairs[sim_work == 1, .(sim_num = iter, total_ot = sum(sim_work), worker_surplus = sum(worker_surplus), total_ot_pay = sum(sim_payment), max_win_wage = max(sim_win_wage), min_win_wage = min(sim_win_wage),
                                                                          avg_win_wage = mean(sim_win_wage)), by = "num_emp1"])

  results_wage <- rbind(results_wage, unique(all_pairs[tot_ot_among > 0, c("analysis_workdate", "sim_markdown")]))
}

ensure_directory(CONFIG$data_dir)
log_message("Saving deviation auction results")
saveRDS(results, file.path(CONFIG$data_dir, "03_01_sim_auction_dev.rds"))
saveRDS(results_wage, file.path(CONFIG$data_dir, "03_01_sim_auction_dev_markdown.rds"))
saveRDS(results_byworker, file.path(CONFIG$data_dir, "03_01_sim_auction_dev_byworker.rds"))

#' ---------------------------------------------------------------------------
#' AUCTION 2: STRAIGHT SHIFT AUCTION
#' ---------------------------------------------------------------------------

log_message("Starting straight shift auction simulation")
results_byworker <- data.table()
results_wage <- data.table()
results <- data.table()

for (iter in 1:max_iter) {
  if (iter %% 100 == 0) log_message(paste0("Straight auction iteration: ", iter))

  all_pairs[, true_valuation := (det_val + rlogis(.N)) / (coef(mod_mod)["ot_rate"])]
  all_pairs[, valuation := true_valuation]

  setorder(all_pairs, "analysis_workdate", -"valuation")
  # for each date, take the first tot_ot_among based on available officers
  all_pairs[, sim_work := (1:.N <= tot_ot_among), by = "analysis_workdate"]
  all_pairs[, sim_value := sim_work * true_valuation, by = "analysis_workdate"]
  # the wage is equal to the negative of the k+1 highest valuation
  all_pairs[, sim_win_wage := max(ifelse(((1:.N) == (tot_ot_among + 1)), valuation, NA), na.rm = TRUE), by = "analysis_workdate"]
  all_pairs[, sim_payment := -(sim_win_wage) * sim_work * all_othours / tot_ot_among]
  # worker surplus is then true valuation plus actual wage
  all_pairs[, worker_surplus := sim_work * (true_valuation - sim_win_wage)]

  ## no auction winner does not want to refuse the shift.
  stopifnot(all_pairs[sim_work == 1]$worker_surplus > 0)

  byemp <- all_pairs[, .(ot_tot = sum(sim_work)), by = "num_emp1"]
  setorder(byemp, "ot_tot", "num_emp1")
  byemp[, position := (1:.N) / .N]
  byemp[, cum_ot := cumsum(ot_tot) / sum(ot_tot)]
  byemp[, is_90th := position >= 0.9 & shift(position) < 0.9]
  stopifnot(nrow(byemp[is_90th == 1]) == 1)

  results <- rbind(results, data.table(sim_num = iter,
                                       worker_value = sum(all_pairs$sim_value),
                                       worker_surplus = sum(all_pairs$worker_surplus),
                                       wage_bill = sum(all_pairs$sim_payment),
                                       share_top10 = 1 - byemp[is_90th == 1]$cum_ot[1]))
  results_byworker <- rbind(results_byworker, all_pairs[sim_work == 1, .(sim_num = iter, total_ot = sum(sim_work), worker_surplus = sum(worker_surplus), total_ot_pay = sum(sim_payment), max_win_wage = max(sim_win_wage), min_win_wage = min(sim_win_wage),
                                                                          avg_win_wage = mean(sim_win_wage)), by = "num_emp1"])
  results_wage <- rbind(results_wage, unique(all_pairs[tot_ot_among > 0, c("analysis_workdate", "sim_win_wage")]))
}

log_message("Saving straight auction results")
saveRDS(results, file.path(CONFIG$data_dir, "03_01_sim_auction_straight.rds"))
saveRDS(results_wage, file.path(CONFIG$data_dir, "03_01_sim_auction_straight_wage.rds"))
saveRDS(results_byworker, file.path(CONFIG$data_dir, "03_01_sim_auction_straight_byworker.rds"))

log_complete(success = TRUE)
