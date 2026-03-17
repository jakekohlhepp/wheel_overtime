#' =============================================================================
#' SIMULATE EFFICIENCY-EQUITY FRONTIER
#' =============================================================================
#' Traces the inequality-allocative efficiency frontier by starting from a
#' random OT allocation and progressively applying bubble sort swaps
#' (representing a manager improving assignments). Each swap level is a point
#' on the frontier between fully random and fully efficient allocation.
#'
#' Input:  data/02_00_estimate.Rdata        (logit model)
#'         data/00_01_estimation_sample.rds  (estimation sample)
#' Output: data/03_00_sim_frontier.rds
#' =============================================================================

library('data.table')
library('alpaca')
library('lubridate')
library('parallel')
library('Rcpp')

source('config.R')
source('utils/logging.R')

log_init("03_00_sim_frontier.R")
log_message("Simulating efficiency-equity frontier")

set.seed(477812)
RNGkind("L'Ecuyer-CMRG")

#' -----------------------------------------------------------------------------
#' RCPP BUBBLE SORT
#' -----------------------------------------------------------------------------
#' Sorts values descending via bubble sort with a swap budget.
#' Returns both sorted values and permuted names in a single pass,
#' avoiding the double-sort bug in the original R implementation.

cppFunction('
List bubble_sort_both(NumericVector x, IntegerVector nms, int max_swaps) {
  int n = x.size();
  NumericVector xout = clone(x);
  IntegerVector nout = clone(nms);
  if (n <= 1 || max_swaps <= 0) return List::create(xout, nout);

  int tot_swaps = 0;
  while (tot_swaps < max_swaps) {
    int new_swaps = 0;
    for (int i = 0; i < n - 1; i++) {
      if (xout[i] < xout[i + 1]) {
        double tmp_x = xout[i];
        xout[i] = xout[i + 1];
        xout[i + 1] = tmp_x;
        int tmp_n = nout[i];
        nout[i] = nout[i + 1];
        nout[i + 1] = tmp_n;
        new_swaps++;
        if (tot_swaps + new_swaps == max_swaps) break;
      }
    }
    tot_swaps += new_swaps;
    if (new_swaps == 0) break;
  }
  return List::create(xout, nout);
}
')

log_message("Compiled Rcpp bubble sort")

#' -----------------------------------------------------------------------------
#' LOAD DATA AND MODEL
#' -----------------------------------------------------------------------------

load(file.path(CONFIG$data_dir, "02_00_estimate.Rdata"))
all_pairs <- readRDS(file.path(CONFIG$data_dir, "00_01_estimation_sample.rds"))

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

## Pre-split by date for fast per-date operations
date_list <- split(sim_base, by = "analysis_workdate", keep.by = TRUE)
date_count <- length(date_list)
unique_officers <- sort(unique(sim_base$num_emp1))
n_officers <- length(unique_officers)

log_message(paste("Dates:", date_count, "| Officers:", n_officers))

setDTthreads(1)

#' -----------------------------------------------------------------------------
#' SIMULATION FUNCTION
#' -----------------------------------------------------------------------------

swap_count <- 80L
n_steps <- 6000L

do_sim <- function(sim_id) {
  ## Pre-allocate results
  res_savy <- integer(n_steps)
  res_value <- numeric(n_steps)
  res_ineq <- numeric(n_steps)

  ## Per-date state: vectors of valuations and officer IDs
  n_dates <- length(date_list)
  vals_list <- vector("list", n_dates)
  ids_list <- vector("list", n_dates)
  ot_counts <- integer(n_dates)
  n_per_date <- integer(n_dates)

  ## Initialize: draw random order + logistic shocks
  for (d in seq_len(n_dates)) {
    dt <- date_list[[d]]
    n <- nrow(dt)
    n_per_date[d] <- n
    ot_counts[d] <- dt$tot_ot_among[1L]
    true_val <- (dt$det_val + rlogis(n)) / coef_ot_rate
    rand_order <- sample.int(n)
    vals_list[[d]] <- true_val[rand_order]
    ids_list[[d]] <- dt$num_emp1[rand_order]
  }

  savy <- 0L

  for (step in seq_len(n_steps)) {
    ## Apply bubble sort swaps (except first step = random baseline)
    if (step > 1L) {
      for (d in seq_len(n_dates)) {
        sorted <- bubble_sort_both(vals_list[[d]], ids_list[[d]], swap_count)
        vals_list[[d]] <- sorted[[1L]]
        ids_list[[d]] <- sorted[[2L]]
      }
    }

    ## Compute total worker value (sum of valuations for top-k workers per date)
    total_value <- 0
    ## Build officer-level OT totals using a named integer vector (fast lookup)
    ot_by_officer <- integer(n_officers)
    names(ot_by_officer) <- unique_officers

    for (d in seq_len(n_dates)) {
      k <- ot_counts[d]
      if (k > 0L) {
        total_value <- total_value + sum(vals_list[[d]][1:k])
        emp_ids <- as.character(ids_list[[d]][1:k])
        for (e in emp_ids) {
          ot_by_officer[e] <- ot_by_officer[e] + 1L
        }
      }
    }

    ## Compute top-10% share of OT (inequality measure)
    ot_sorted <- sort(ot_by_officer)
    positions <- seq_along(ot_sorted) / n_officers
    cum_ot <- cumsum(ot_sorted) / sum(ot_sorted)
    idx_90 <- which(positions >= 0.9 & c(FALSE, positions[-n_officers] < 0.9))
    share_top10 <- 1 - cum_ot[idx_90[1L]]

    res_savy[step] <- savy
    res_value[step] <- total_value
    res_ineq[step] <- share_top10
    savy <- savy + swap_count * date_count
  }

  data.table(savy_num = res_savy, sim_num = sim_id,
             worker_value = res_value, share_top10 = res_ineq)
}

#' -----------------------------------------------------------------------------
#' RUN SIMULATIONS IN PARALLEL
#' -----------------------------------------------------------------------------

n_sims <- 2500L
core_count <- min(parallel::detectCores() - 1L, 50L)
log_message(paste("Running", n_sims, "simulations on", core_count, "cores"))

## Use parLapply (works on Windows, unlike mclapply which falls back to 1 core)
cl <- makeCluster(core_count)
clusterSetRNGStream(cl, 477812)
clusterExport(cl, c("date_list", "date_count", "unique_officers", "n_officers",
                     "coef_ot_rate", "swap_count", "n_steps",
                     "bubble_sort_both", "do_sim"))
clusterEvalQ(cl, library(data.table))

sim_start <- Sys.time()
results_list <- parLapply(cl, 1:n_sims, do_sim)
stopCluster(cl)

sim_time <- difftime(Sys.time(), sim_start, units = "mins")
log_message(paste("Simulations complete:", round(sim_time, 1), "minutes"))

#' -----------------------------------------------------------------------------
#' SAVE OUTPUT
#' -----------------------------------------------------------------------------

all_res <- rbindlist(results_list)

output_path <- file.path(CONFIG$data_dir, "03_00_sim_frontier.rds")
ensure_directory(dirname(output_path))
saveRDS(all_res, output_path)

log_message(paste("Saved:", output_path, "-", nrow(all_res), "rows"))
log_complete(success = TRUE)
message("03_00_sim_frontier complete")
