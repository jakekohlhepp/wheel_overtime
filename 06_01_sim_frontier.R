#' =============================================================================
#' SIMULATE EFFICIENCY-EQUITY FRONTIER
#' =============================================================================
#' Traces the inequality-allocative efficiency frontier by starting from a
#' random OT allocation and progressively applying bubble sort swaps
#' (representing a manager improving assignments). Each swap level is a point
#' on the frontier between fully random and fully efficient allocation.
#'
#' Input:  data/04_01_estimate.Rdata        (logit model)
#'         data/02_01_estimation_sample.rds  (estimation sample)
#' Output: data/06_01_sim_frontier.rds
#' =============================================================================

library('data.table')
library('alpaca')
library('lubridate')
library('parallel')
library('Rcpp')

source('config.R')
source('utils/logging.R')

log_init("06_01_sim_frontier.R")
log_message("Simulating efficiency-equity frontier")

set.seed(477812)
RNGkind("L'Ecuyer-CMRG")

#' -----------------------------------------------------------------------------
#' RCPP: COMBINED SORT + VALUE + INEQUALITY
#' -----------------------------------------------------------------------------
#' Single C++ function that (1) applies bubble sort swaps across all dates,
#' (2) computes total worker value, and (3) computes top-10% OT share.
#' Eliminates per-date R loops and character-based officer lookups.

sim_step_src <- '
List sim_step(List vals_list, List ids_list, IntegerVector ot_counts,
              int swap_count, int n_officers, bool do_sort) {
  int n_dates = vals_list.size();
  int total_swaps = 0;

  // Apply bubble sort swaps across all dates
  if (do_sort) {
    for (int d = 0; d < n_dates; d++) {
      NumericVector xv = vals_list[d];
      IntegerVector iv = ids_list[d];
      int n = xv.size();
      if (n <= 1 || swap_count <= 0) continue;

      int remaining = swap_count;
      while (remaining > 0) {
        int new_swaps = 0;
        for (int i = 0; i < n - 1; i++) {
          if (xv[i] < xv[i + 1]) {
            double tmp_x = xv[i];
            xv[i] = xv[i + 1];
            xv[i + 1] = tmp_x;
            int tmp_n = iv[i];
            iv[i] = iv[i + 1];
            iv[i + 1] = tmp_n;
            new_swaps++;
            if (new_swaps == remaining) break;
          }
        }
        remaining -= new_swaps;
        total_swaps += new_swaps;
        if (new_swaps == 0) break;
      }
      // Write back to ensure sorted state persists across steps
      vals_list[d] = xv;
      ids_list[d] = iv;
    }
  }

  // Compute total worker value and officer OT counts
  double total_value = 0.0;
  std::vector<int> ot_by_officer(n_officers, 0);

  for (int d = 0; d < n_dates; d++) {
    NumericVector xv = vals_list[d];
    IntegerVector iv = ids_list[d];
    int k = std::min((int)ot_counts[d], (int)xv.size());
    for (int i = 0; i < k; i++) {
      total_value += xv[i];
      int oid = iv[i];
      if (oid >= 0 && oid < n_officers) ot_by_officer[oid] += 1;
    }
  }

  // Sort OT counts ascending for Lorenz curve (matches R scripts)
  std::sort(ot_by_officer.begin(), ot_by_officer.end());

  double sum_ot = 0.0;
  for (int i = 0; i < n_officers; i++) sum_ot += ot_by_officer[i];

  // Replicate R logic: position = (1:.N)/.N, find first i where
  // position[i] >= 0.9 AND position[i-1] < 0.9, then share_top10 = 1 - cum_ot[i]
  double share_top10 = 0.0;
  if (sum_ot > 0) {
    double cum = 0.0;
    int crossing = -1;
    for (int i = 0; i < n_officers; i++) {
      cum += ot_by_officer[i];
      double pos = (double)(i + 1) / n_officers;
      double prev_pos = (double)i / n_officers;
      if (pos >= 0.9 && prev_pos < 0.9) {
        crossing = i;
        break;
      }
    }
    if (crossing >= 0) {
      double cum_at_crossing = 0.0;
      for (int i = 0; i <= crossing; i++) cum_at_crossing += ot_by_officer[i];
      share_top10 = 1.0 - cum_at_crossing / sum_ot;
    }
  }

  return List::create(total_value, share_top10, total_swaps);
}
'

cppFunction(sim_step_src)
log_message("Compiled Rcpp simulation step")

#' -----------------------------------------------------------------------------
#' LOAD DATA AND MODEL
#' -----------------------------------------------------------------------------

load(file.path(CONFIG$data_dir, "04_01_estimate.Rdata"))
all_pairs <- readRDS(file.path(CONFIG$data_dir, "02_01_estimation_sample.rds"))

officer_fe <- data.table(officer_fe = getFEs(mod_mod)$num_emp1,
                         num_emp1 = as.numeric(names(getFEs(mod_mod)$num_emp1)))
all_pairs <- merge(all_pairs, officer_fe, by = "num_emp1", all.x = TRUE)
date_fe <- data.table(date_fe = getFEs(mod_mod)$analysis_workdate,
                      analysis_workdate = as.Date(names(getFEs(mod_mod)$analysis_workdate)))
all_pairs <- merge(all_pairs, date_fe, by = "analysis_workdate", all.x = TRUE)

log_message(paste0("Fraction officers without FE: ",
                   round(uniqueN(all_pairs[is.na(officer_fe)]$num_emp1) / uniqueN(all_pairs$num_emp1), 4)))
all_pairs <- all_pairs[!is.na(officer_fe), ]

all_pairs[, tot_ot_among := sum(ot_work), by = "analysis_workdate"]
all_pairs[, det_val := date_fe + officer_fe +
            seniority_rank * coef(mod_mod)["seniority_rank"] +
            normal_work * coef(mod_mod)["normal_work"]]

## Extract only the columns needed for simulation to reduce memory per copy
coef_ot_rate <- coef(mod_mod)["ot_rate"]
sim_cols <- c("analysis_workdate", "num_emp1", "det_val", "tot_ot_among")
sim_base <- all_pairs[, ..sim_cols]

## Map officer IDs to 0-based integer indices for fast C++ accumulation
unique_officers <- sort(unique(sim_base$num_emp1))
n_officers <- length(unique_officers)
officer_idx <- match(sim_base$num_emp1, unique_officers) - 1L
sim_base[, officer_idx := officer_idx]

## Pre-split by date for fast per-date operations
date_list <- split(sim_base, by = "analysis_workdate", keep.by = TRUE)
date_count <- length(date_list)

log_message(paste("Dates:", date_count, "| Officers:", n_officers))

setDTthreads(1)

#' -----------------------------------------------------------------------------
#' SIMULATION PARAMETERS + PARALLEL EXECUTION
#' -----------------------------------------------------------------------------

swap_count <- 80L
n_steps    <- 6000L

n_sims <- 2500L
core_count <- min(parallel::detectCores() - 1L, 50L)
log_message(paste("Running", n_sims, "simulations on", core_count, "cores"))

## Use parLapply (works on Windows, unlike mclapply which falls back to 1 core)
cl <- makeCluster(core_count)
on.exit({
  if (exists("cl") && !is.null(cl)) {
    try(stopCluster(cl), silent = TRUE)
  }
}, add = TRUE)

bootstrap_project_cluster(cl, packages = c("data.table", "Rcpp"))
clusterSetRNGStream(cl, 477812)

## Export everything except do_sim: Rcpp-compiled function pointers do not
## survive R serialization, so the sim_step symbol captured in do_sim's closure
## arrives on workers as NULL. Instead, do_sim is defined on each worker after
## cppFunction() runs locally. Compile in small batches to avoid Windows / GCC
## memory failures when many workers build the same shared library at once.
clusterExport(cl, c("date_list", "date_count", "n_officers",
                     "coef_ot_rate", "swap_count", "n_steps",
                     "sim_step_src"), envir = environment())

compile_batch_size <- min(2L, core_count)
worker_batches <- split(seq_len(core_count),
                        ceiling(seq_len(core_count) / compile_batch_size))

for (batch_idx in seq_along(worker_batches)) {
  batch <- worker_batches[[batch_idx]]
  log_message(
    paste0(
      "Initializing Rcpp worker batch ", batch_idx, "/", length(worker_batches),
      " (", length(batch), " workers)"
    )
  )

  clusterEvalQ(cl[batch], {
    cppFunction(sim_step_src)

    ## Redefine do_sim here so the sim_step reference resolves to the local copy
    do_sim <- function(sim_id) {
      res_savy   <- integer(n_steps)
      res_value  <- numeric(n_steps)
      res_ineq   <- numeric(n_steps)

      n_dates   <- length(date_list)
      vals_list <- vector("list", n_dates)
      ids_list  <- vector("list", n_dates)
      ot_counts <- integer(n_dates)

      for (d in seq_len(n_dates)) {
        dt           <- date_list[[d]]
        n            <- nrow(dt)
        ot_counts[d] <- dt$tot_ot_among[1L]
        true_val     <- (dt$det_val + rlogis(n)) / coef_ot_rate
        rand_order   <- sample.int(n)
        vals_list[[d]] <- true_val[rand_order]
        ids_list[[d]]  <- dt$officer_idx[rand_order]
      }

      savy      <- 0L
      converged <- FALSE

      for (step in seq_len(n_steps)) {
        do_sort <- step > 1L && !converged
        out <- sim_step(vals_list, ids_list, ot_counts,
                        swap_count, n_officers, do_sort)

        res_savy[step]  <- savy
        res_value[step] <- out[[1L]]
        res_ineq[step]  <- out[[2L]]

        if (do_sort && out[[3L]] == 0L) converged <- TRUE
        savy <- savy + swap_count * date_count
      }

      data.table(savy_num = res_savy, sim_num = sim_id,
                 worker_value = res_value, share_top10 = res_ineq)
    }

    invisible(TRUE)
  })
}

## Pass an anonymous wrapper rather than do_sim directly: parLapply evaluates
## its FUN argument in the main process before dispatching, and do_sim only
## exists on the workers (defined inside clusterEvalQ above). The anonymous
## function has no do_sim in its closure, so the lookup happens on the worker
## where do_sim is in .GlobalEnv.
sim_start <- Sys.time()
results_list <- parLapply(cl, 1:n_sims, function(i) do_sim(i))
stopCluster(cl)
cl <- NULL

sim_time <- difftime(Sys.time(), sim_start, units = "mins")
log_message(paste("Simulations complete:", round(sim_time, 1), "minutes"))

#' -----------------------------------------------------------------------------
#' SAVE OUTPUT
#' -----------------------------------------------------------------------------

all_res <- rbindlist(results_list)

output_path <- file.path(CONFIG$data_dir, "06_01_sim_frontier.rds")
ensure_directory(dirname(output_path))
saveRDS(all_res, output_path)

log_message(paste("Saved:", output_path, "-", nrow(all_res), "rows"))
log_complete(success = TRUE)
message("06_01_sim_frontier complete")
