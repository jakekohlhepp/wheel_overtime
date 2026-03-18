#' =============================================================================
#' INFORMAL TRADE SIMULATION - PERFECT (POSITIVE CORRELATION)
#' =============================================================================
#' Sort for exact alignment - most connections for the person who wants most.
#' Vary access costs and network reduction parameters.
#' Input:  file.path(CONFIG$data_dir, "02_00_estimate.Rdata")
#'         file.path(CONFIG$data_dir, "00_01_estimation_sample.rds")
#' Output: file.path(CONFIG$data_dir, "03_05_sim_informal_perfect.rds")
#'         file.path(CONFIG$data_dir, "03_05_sim_informal_perfect_byworker.rds")
#' =============================================================================

library('data.table')
library('alpaca')
library('lubridate')
library('parallel')

source('config.R')
source('utils/logging.R')
log_init("03_05_sim_informal_perfect.R")

#' ---------------------------------------------------------------------------
#' LOAD DATA
#' ---------------------------------------------------------------------------

set.seed(455723)
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

## exclude officers without fixed effects
log_message(paste0("Excluding ", uniqueN(all_pairs[is.na(officer_fe)]$num_emp1), " officers without FE"))
all_pairs <- all_pairs[!is.na(officer_fe), ]

### for each date, compute the total ot hours and total ot instances.
## assign based on number of instances, assume even hours distribution
all_pairs[, all_othours := sum(varot_hours), by = "analysis_workdate"]
all_pairs[, tot_ot_among := sum(ot_work), by = "analysis_workdate"]

## Extract coefficients once
beta_sr  <- coef(mod_mod)["seniority_rank"]
beta_nw  <- coef(mod_mod)["normal_work"]
beta_ot  <- coef(mod_mod)["ot_rate"]

#' ---------------------------------------------------------------------------
#' RUN SIMULATION (parallel across grid cells)
#' ---------------------------------------------------------------------------

## set number of iterations
max_iter <- 200

## Build grid of (net, cost) pairs
net_vals  <- c(seq(from = 0, to = 20 * coef(mod_mod)["suppliers_interacted"], length = 20), coef(mod_mod)["suppliers_interacted"])
cost_vals <- c(seq(from = 0, to = 20 * coef(mod_mod)["opp_dist"], length = 20), coef(mod_mod)["opp_dist"])
grid <- expand.grid(net = net_vals, cost = cost_vals)
log_message(paste0("Starting perfect alignment informal trade simulation: ", nrow(grid), " grid cells x ", max_iter, " iterations"))

## Slim down to needed columns
keep_cols <- c("num_emp1", "analysis_workdate", "ot_rate", "opp_dist",
               "suppliers_interacted", "l_wheel_degree",
               "date_fe", "officer_fe",
               "seniority_rank", "normal_work",
               "tot_ot_among", "all_othours")
ap_slim <- all_pairs[, ..keep_cols]

## Generate per-cell seeds
cell_seeds <- sample.int(.Machine$integer.max, nrow(grid))

run_one_cell_perfect <- function(cell_idx, ap, grid, max_iter, beta_sr, beta_nw, beta_ot) {
  library(data.table)
  dt <- copy(ap)
  n <- nrow(dt)
  net  <- grid$net[cell_idx]
  cost <- grid$cost[cell_idx]

  ## Base deterministic utility (without network/access — used for sorting)
  dt[, det_util_base := date_fe + officer_fe + seniority_rank * beta_sr +
              normal_work * beta_nw]

  res_list <- vector("list", max_iter)
  bw_list  <- vector("list", max_iter)

  for (iter in seq_len(max_iter)) {
    ## draw logit shocks
    dt[, true_utility := det_util_base + rlogis(n)]

    ## Sort by base utility (descending) within each date
    setorder(dt, "analysis_workdate", -"true_utility", "num_emp1")
    ## PERFECT: reassign the MOST connections to those who most value overtime
    dt[, mod_l_wheel_degree := sort(l_wheel_degree, decreasing = TRUE), by = "analysis_workdate"]
    dt[, mod_suppliers_interacted := opp_dist * mod_l_wheel_degree]

    ## Add network/access/wage terms to get full utility
    dt[, true_utility := true_utility + ot_rate * beta_ot + opp_dist * cost + mod_suppliers_interacted * net]
    dt[, sim_work := frank(-true_utility, ties.method = "random") <= tot_ot_among, by = "analysis_workdate"]
    dt[, sim_win_wage := ot_rate]

    ## non-wage utility delivered
    dt[, true_valuation := (true_utility -
                               (ot_rate * beta_ot + opp_dist * cost + mod_suppliers_interacted * net)) / beta_ot]
    dt[, sim_value := sim_work * true_valuation]
    dt[, sim_payment := fifelse(tot_ot_among > 0L, sim_win_wage * sim_work * all_othours / tot_ot_among, 0)]
    dt[, worker_surplus := sim_work * (true_utility / beta_ot)]

    byemp <- dt[, .(ot_tot = sum(sim_work)), by = "num_emp1"]
    setorder(byemp, "ot_tot", "num_emp1")
    byemp[, position := (1:.N) / .N]
    byemp[, cum_ot := cumsum(ot_tot) / sum(ot_tot)]
    byemp[, is_90th := position >= 0.9 & shift(position) < 0.9]
    stopifnot(nrow(byemp[is_90th == TRUE]) == 1)

    res_list[[iter]] <- data.table(network_reduction = net, access_cost = cost, sim_num = iter,
                                   worker_value = sum(dt$sim_value),
                                   worker_surplus = sum(dt$worker_surplus),
                                   wage_bill = sum(dt$sim_payment),
                                   share_top10 = 1 - byemp[is_90th == TRUE]$cum_ot[1])
    bw_list[[iter]] <- dt[sim_work == TRUE, .(network_reduction = net, access_cost = cost, sim_num = iter,
                                               total_ot = sum(sim_work),
                                               worker_surplus = sum(worker_surplus),
                                               total_ot_pay = sum(sim_payment),
                                               max_win_wage = max(sim_win_wage),
                                               min_win_wage = min(sim_win_wage),
                                               avg_win_wage = mean(sim_win_wage)),
                          by = "num_emp1"]
  }

  list(res = rbindlist(res_list), res_bw = rbindlist(bw_list))
}

n_cores <- min(detectCores() - 1, 10)
log_message(paste0("Running on ", n_cores, " cores"))

cl <- makeCluster(n_cores)
clusterExport(cl, c("ap_slim", "grid", "max_iter", "beta_sr", "beta_nw", "beta_ot",
                     "run_one_cell_perfect", "cell_seeds"), envir = environment())

out <- parLapply(cl, seq_len(nrow(grid)), function(i) {
  set.seed(cell_seeds[i])
  run_one_cell_perfect(i, ap_slim, grid, max_iter, beta_sr, beta_nw, beta_ot)
})

stopCluster(cl)
log_message("Parallel run complete, collecting results")

results         <- rbindlist(lapply(out, `[[`, "res"))
results_byworker <- rbindlist(lapply(out, `[[`, "res_bw"))
rm(out); gc()

#' ---------------------------------------------------------------------------
#' SAVE RESULTS
#' ---------------------------------------------------------------------------

ensure_directory(CONFIG$data_dir)
log_message("Saving perfect alignment informal trade simulation results")
saveRDS(results, file.path(CONFIG$data_dir, "03_05_sim_informal_perfect.rds"))
saveRDS(results_byworker, file.path(CONFIG$data_dir, "03_05_sim_informal_perfect_byworker.rds"))

log_complete(success = TRUE)
