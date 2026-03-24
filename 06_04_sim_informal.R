#' =============================================================================
#' INFORMAL TRADE SIMULATION
#' =============================================================================
#' Hold fix the number of officers who work each day at the observed level
#' but choose based on the model index, varying access costs and network.
#' Input:  file.path(CONFIG$data_dir, "04_01_estimate.Rdata")
#'         file.path(CONFIG$data_dir, "02_01_estimation_sample.rds")
#' Output: file.path(CONFIG$data_dir, "06_04_sim_informal.rds")
#'         file.path(CONFIG$data_dir, "06_04_sim_informal_byworker.rds")
#' =============================================================================

library('data.table')
library('alpaca')
library('lubridate')
library('parallel')

source('config.R')
source('utils/logging.R')
log_init("06_04_sim_informal.R")

#' ---------------------------------------------------------------------------
#' LOAD DATA
#' ---------------------------------------------------------------------------

set.seed(54323)
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
#' PREPARE SIMULATION DATA (pre-split by date for speed)
#' ---------------------------------------------------------------------------

## Pre-compute the grid-invariant parts of utility and valuation.
## true_valuation = (date_fe + officer_fe + sr*beta + nw*beta + shock) / beta_ot
## — the net/cost terms cancel in the valuation formula, so base_val is
##   constant across all grid cells.
all_pairs[, base_util := date_fe + officer_fe +
            seniority_rank * beta_sr + normal_work * beta_nw + ot_rate * beta_ot]
all_pairs[, base_val  := (date_fe + officer_fe +
            seniority_rank * beta_sr + normal_work * beta_nw) / beta_ot]

## Map officers to integer indices for fast vector accumulation
unique_officers <- sort(unique(all_pairs$num_emp1))
n_officers      <- length(unique_officers)
all_pairs[, oidx := match(num_emp1, unique_officers)]

## Pre-compute per-officer payment if selected (constant within each date)
all_pairs[, pay_if_selected := fifelse(
  tot_ot_among > 0L, ot_rate * all_othours / tot_ot_among, 0)]

## Pre-split by date into lightweight lists of vectors.
## This avoids repeated grouped data.table operations in the inner loop.
date_splits <- lapply(split(all_pairs, by = "analysis_workdate"), function(d) {
  list(opp_dist   = d$opp_dist,
       suppliers  = d$suppliers_interacted,
       base_util  = d$base_util,
       base_val   = d$base_val,
       pay_rate   = d$pay_if_selected,
       oidx       = d$oidx,
       ot_rate_v  = d$ot_rate,
       k          = d$tot_ot_among[1L],
       n          = nrow(d))
})

#' ---------------------------------------------------------------------------
#' RUN SIMULATION (parallel across grid cells)
#' ---------------------------------------------------------------------------

max_iter <- 200

## Build grid of (net, cost) pairs
net_vals  <- c(seq(from = 0, to = 20 * coef(mod_mod)["suppliers_interacted"], length = 20),
               coef(mod_mod)["suppliers_interacted"])
cost_vals <- c(seq(from = 0, to = 20 * coef(mod_mod)["opp_dist"], length = 20),
               coef(mod_mod)["opp_dist"])
grid <- expand.grid(net = net_vals, cost = cost_vals)

## Flag the status-quo cell (true coefficient values) — only this cell needs
## by-worker output (downstream 07_02 filters to access_cost_mult == 1).
sq_net  <- unname(coef(mod_mod)["suppliers_interacted"])
sq_cost <- unname(coef(mod_mod)["opp_dist"])
grid$is_sq <- (grid$net == sq_net & grid$cost == sq_cost)

log_message(paste0("Starting informal trade simulation: ", nrow(grid),
                    " grid cells x ", max_iter, " iterations"))

## Generate per-cell seeds
cell_seeds <- sample.int(.Machine$integer.max, nrow(grid))

run_one_cell <- function(cell_idx) {
  library(data.table)
  net      <- grid$net[cell_idx]
  cost     <- grid$cost[cell_idx]
  need_bw  <- grid$is_sq[cell_idx]
  n_dates  <- length(date_splits)

  ## Precompute grid-specific det_util per date (constant across iterations)
  det_utils <- lapply(date_splits, function(ds) {
    ds$opp_dist * cost + ds$suppliers * net + ds$base_util
  })

  res_list <- vector("list", max_iter)
  if (need_bw) bw_list <- vector("list", max_iter)

  for (iter in seq_len(max_iter)) {
    total_value <- 0
    total_wage  <- 0
    ot_count    <- integer(n_officers)

    ## By-worker accumulators (only when needed)
    if (need_bw) {
      surplus_acc <- numeric(n_officers)
      pay_acc     <- numeric(n_officers)
      wage_max    <- rep(-Inf, n_officers)
      wage_min    <- rep( Inf, n_officers)
      wage_sum    <- numeric(n_officers)
      wage_n      <- integer(n_officers)
    }

    for (d in seq_len(n_dates)) {
      ds <- date_splits[[d]]
      k  <- ds$k
      if (k == 0L) next

      shock   <- rlogis(ds$n)
      utility <- det_utils[[d]] + shock
      val     <- ds$base_val + shock / beta_ot

      ## Select top-k officers by utility (replaces grouped frank)
      sel     <- order(utility, decreasing = TRUE)[seq_len(min(k, ds$n))]
      val_sel <- val[sel]

      total_value <- total_value + sum(val_sel)
      total_wage  <- total_wage  + sum(ds$pay_rate[sel])

      ## Accumulate OT counts per officer
      oids <- ds$oidx[sel]
      for (s in seq_along(oids)) {
        ot_count[oids[s]] <- ot_count[oids[s]] + 1L
      }

      ## Detailed by-worker stats only for status-quo cell
      if (need_bw) {
        for (s in seq_along(oids)) {
          o <- oids[s]
          surplus_acc[o] <- surplus_acc[o] + val_sel[s]
          pay_acc[o]     <- pay_acc[o]     + ds$pay_rate[sel[s]]
          w <- ds$ot_rate_v[sel[s]]
          if (w > wage_max[o]) wage_max[o] <- w
          if (w < wage_min[o]) wage_min[o] <- w
          wage_sum[o] <- wage_sum[o] + w
          wage_n[o]   <- wage_n[o]   + 1L
        }
      }
    }

    ## Inequality: OT share held by top 10% of officers
    sorted_ot <- sort(ot_count)
    sum_ot    <- sum(sorted_ot)
    if (sum_ot > 0) {
      pos      <- seq_len(n_officers) / n_officers
      prev_pos <- c(0, pos[-n_officers])
      crossing <- which(pos >= 0.9 & prev_pos < 0.9)[1L]
      share_top10 <- 1 - sum(sorted_ot[seq_len(crossing)]) / sum_ot
    } else {
      share_top10 <- 0
    }

    res_list[[iter]] <- data.table(
      network_reduction = net, access_cost = cost, sim_num = iter,
      worker_value = total_value, worker_surplus = total_value,
      wage_bill = total_wage, share_top10 = share_top10)

    if (need_bw) {
      active <- which(ot_count > 0L)
      bw_list[[iter]] <- data.table(
        num_emp1 = unique_officers[active],
        network_reduction = net, access_cost = cost, sim_num = iter,
        total_ot = ot_count[active],
        worker_surplus = surplus_acc[active],
        total_ot_pay = pay_acc[active],
        max_win_wage = wage_max[active],
        min_win_wage = wage_min[active],
        avg_win_wage = wage_sum[active] / wage_n[active])
    }
  }

  if (need_bw) {
    list(res = rbindlist(res_list), res_bw = rbindlist(bw_list))
  } else {
    list(res = rbindlist(res_list), res_bw = NULL)
  }
}

n_cores <- min(detectCores() - 1, 10)
log_message(paste0("Running on ", n_cores, " cores"))

cl <- makeCluster(n_cores)
clusterExport(cl, c("date_splits", "grid", "max_iter", "beta_ot",
                     "n_officers", "unique_officers", "cell_seeds",
                     "run_one_cell"), envir = environment())
clusterEvalQ(cl, library(data.table))

out <- parLapply(cl, seq_len(nrow(grid)), function(i) {
  set.seed(cell_seeds[i])
  run_one_cell(i)
})

stopCluster(cl)
log_message("Parallel run complete, collecting results")

results          <- rbindlist(lapply(out, `[[`, "res"))
bw_parts         <- Filter(Negate(is.null), lapply(out, `[[`, "res_bw"))
results_byworker <- rbindlist(bw_parts)
rm(out, bw_parts); gc()

#' ---------------------------------------------------------------------------
#' SAVE RESULTS
#' ---------------------------------------------------------------------------

ensure_directory(CONFIG$data_dir)
log_message("Saving informal trade simulation results")
saveRDS(results, file.path(CONFIG$data_dir, "06_04_sim_informal.rds"))
saveRDS(results_byworker, file.path(CONFIG$data_dir, "06_04_sim_informal_byworker.rds"))

log_complete(success = TRUE)
