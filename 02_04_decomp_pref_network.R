#' =============================================================================
#' DECOMPOSE PREFERENCES VS NETWORK
#' =============================================================================
#' Input:  data/00_01_estimation_sample.rds
#'         data/02_00_estimate.Rdata
#' Output: out/tables/02_04_decomp.tex
#' =============================================================================

library('data.table')
library('lubridate')
library('kableExtra')
library('igraph')
library('qgraph')
library('ggplot2')
library('fixest')
library('stringr')
library('alpaca')
library('stargazer')

source('config.R')
source('utils/logging.R')

log_init("02_04_decomp_pref_network.R")
log_message("Starting preference vs network decomposition")

set.seed(490028)

#' ---------------------------------------------------------------------------
#' LOAD DATA AND FIXED EFFECTS
#' ---------------------------------------------------------------------------

### simulations to parse contributions of network vs preferences.

### step 1: determine the constant that must be added to preferences when centrality is zero to achieve the same average work probability.
all_pairs <- readRDS(file.path(CONFIG$data_dir, "00_01_estimation_sample.rds"))
load(file.path(CONFIG$data_dir, "02_00_estimate.Rdata"))

date_fe <- data.table(datefe = getFEs(mod_mod)$analysis_workdate, analysis_workdate = as.Date(names(getFEs(mod_mod)$analysis_workdate)))
officer_fe <- data.table(officerfe = getFEs(mod_mod)$num_emp1, num_emp1 = as.numeric(names(getFEs(mod_mod)$num_emp1)))

## attach fes
all_pairs <- merge(all_pairs, date_fe, by = "analysis_workdate", all.x = TRUE)
all_pairs <- merge(all_pairs, officer_fe, by = "num_emp1", all.x = TRUE)
## exclude officers without fixed effects
log_message(paste0("Excluding ", uniqueN(all_pairs[is.na(officerfe)]$num_emp1), " officers without FE"))
all_pairs <- all_pairs[!is.na(officerfe), ]

#' ---------------------------------------------------------------------------
#' CONSTRUCT COUNTERFACTUAL INDICES
#' ---------------------------------------------------------------------------

log_message("Computing counterfactual linear indices")

## d stands for degree, r stands for rate, p stands for pref.
## so xb_dr means just degree and rate.

## all pairs
all_pairs[, xb_rp := opp_dist * coef(mod_mod)['opp_dist'] + ot_rate * coef(mod_mod)['ot_rate'] +
            seniority_rank * coef(mod_mod)['seniority_rank'] +
            +normal_work * coef(mod_mod)['normal_work'] +
            datefe + officerfe]
all_pairs[, xb_dr := suppliers_interacted * coef(mod_mod)['suppliers_interacted'] + opp_dist * coef(mod_mod)['opp_dist'] + ot_rate * coef(mod_mod)['ot_rate'] +
            seniority_rank * coef(mod_mod)['seniority_rank'] +
            +normal_work * coef(mod_mod)['normal_work'] +
            datefe]
all_pairs[, xb_dp := suppliers_interacted * coef(mod_mod)['suppliers_interacted'] + opp_dist * coef(mod_mod)['opp_dist'] +
            seniority_rank * coef(mod_mod)['seniority_rank'] +
            +normal_work * coef(mod_mod)['normal_work'] +
            datefe + officerfe]

## all singletons
all_pairs[, xb_r := ot_rate * coef(mod_mod)['ot_rate'] + opp_dist * coef(mod_mod)['opp_dist'] +
            seniority_rank * coef(mod_mod)['seniority_rank'] +
            +normal_work * coef(mod_mod)['normal_work'] +
            datefe]
all_pairs[, xb_d := suppliers_interacted * coef(mod_mod)['suppliers_interacted'] + opp_dist * coef(mod_mod)['opp_dist'] +
            seniority_rank * coef(mod_mod)['seniority_rank'] +
            +normal_work * coef(mod_mod)['normal_work'] +
            datefe]
all_pairs[, xb_p := seniority_rank * coef(mod_mod)['seniority_rank'] + opp_dist * coef(mod_mod)['opp_dist'] +
            +normal_work * coef(mod_mod)['normal_work'] +
            datefe + officerfe]

## 0
all_pairs[, xb_0 := opp_dist * coef(mod_mod)['opp_dist'] + seniority_rank * coef(mod_mod)['seniority_rank'] +
            +normal_work * coef(mod_mod)['normal_work'] +
            datefe]

#' ---------------------------------------------------------------------------
#' SOLVE FOR ADJUSTMENT CONSTANTS
#' ---------------------------------------------------------------------------

log_message("Solving for adjustment constants")

findit <- function(x, dat, actual) {
  return(sum(plogis(dat + x)) - sum(actual))
}
## get the relevant adjustment for each
sol_rp <- uniroot(findit, dat = all_pairs$xb_rp, actual = all_pairs$ot_work, interval = c(-20, 20), tol = 1e-08)
sol_dr <- uniroot(findit, dat = all_pairs$xb_dr, actual = all_pairs$ot_work, interval = c(-20, 20), tol = 1e-08)
sol_dp <- uniroot(findit, dat = all_pairs$xb_dp, actual = all_pairs$ot_work, interval = c(-20, 20), tol = 1e-08)
sol_r <- uniroot(findit, dat = all_pairs$xb_r, actual = all_pairs$ot_work, interval = c(-20, 20), tol = 1e-08)
sol_d <- uniroot(findit, dat = all_pairs$xb_d, actual = all_pairs$ot_work, interval = c(-20, 20), tol = 1e-08)
sol_p <- uniroot(findit, dat = all_pairs$xb_p, actual = all_pairs$ot_work, interval = c(-20, 20), tol = 1e-08)
sol_0 <- uniroot(findit, dat = all_pairs$xb_0, actual = all_pairs$ot_work, interval = c(-20, 20), tol = 1e-08)

#' ---------------------------------------------------------------------------
#' COMPUTE SIMULATED PROBABILITIES
#' ---------------------------------------------------------------------------

log_message("Computing simulated work probabilities")

## step 2: compute estimated probabilities
all_pairs[, work_sim_rp := plogis(xb_rp + sol_rp$root)]
all_pairs[, work_sim_dr := plogis(xb_dr + sol_dr$root)]
all_pairs[, work_sim_dp := plogis(xb_dp + sol_dp$root)]
all_pairs[, work_sim_r := plogis(xb_r + sol_r$root)]
all_pairs[, work_sim_d := plogis(xb_d + sol_d$root)]
all_pairs[, work_sim_p := plogis(xb_p + sol_p$root)]
all_pairs[, work_sim_0 := plogis(xb_0 + sol_0$root)]

#' ---------------------------------------------------------------------------
#' SHAPLEY DECOMPOSITION
#' ---------------------------------------------------------------------------

log_message("Computing Shapley decomposition of inequality contributions")

### step 3: compute changes.
res <- data.table()
## default
byemp <- all_pairs[, .(ot_tot = sum(ot_work)), by = "num_emp1"]
setorder(byemp, "ot_tot", "num_emp1")
byemp[, position := (1:.N) / .N]
byemp[, cum_ot := cumsum(ot_tot) / sum(ot_tot)]
byemp[, is_90th := position >= 0.9 & shift(position) < 0.9]
stopifnot(nrow(byemp[is_90th == 1]) == 1)

share_top10_observed <- 1 - byemp[is_90th == 1]$cum_ot[1]

## share with all shut down
byemp <- all_pairs[, .(ot_tot = sum(work_sim_0)), by = "num_emp1"]
setorder(byemp, "ot_tot", "num_emp1")
byemp[, position := (1:.N) / .N]
byemp[, cum_ot := cumsum(ot_tot) / sum(ot_tot)]
byemp[, is_90th := position >= 0.9 & shift(position) < 0.9]
stopifnot(nrow(byemp[is_90th == 1]) == 1)

share_0 <- 1 - byemp[is_90th == 1]$cum_ot[1]

### for each attribute compute change from removing everything except it, just it, and shapley.

## connectedness
# except it
byemp <- all_pairs[, .(ot_tot = sum(work_sim_d)), by = "num_emp1"]
setorder(byemp, "ot_tot", "num_emp1")
byemp[, position := (1:.N) / .N]
byemp[, cum_ot := cumsum(ot_tot) / sum(ot_tot)]
byemp[, is_90th := position >= 0.9 & shift(position) < 0.9]
stopifnot(nrow(byemp[is_90th == 1]) == 1)

share_except <- (1 - byemp[is_90th == 1]$cum_ot[1]) - share_top10_observed

# just shut it down
byemp <- all_pairs[, .(ot_tot = sum(work_sim_rp)), by = "num_emp1"]
setorder(byemp, "ot_tot", "num_emp1")
byemp[, position := (1:.N) / .N]
byemp[, cum_ot := cumsum(ot_tot) / sum(ot_tot)]
byemp[, is_90th := position >= 0.9 & shift(position) < 0.9]
stopifnot(nrow(byemp[is_90th == 1]) == 1)

share_just <- (1 - byemp[is_90th == 1]$cum_ot[1]) - share_top10_observed

## shapley
# all to 2 is share_just
## 1 to 0
byemp <- all_pairs[, .(ot_tot = sum(work_sim_d)), by = "num_emp1"]
setorder(byemp, "ot_tot", "num_emp1")
byemp[, position := (1:.N) / .N]
byemp[, cum_ot := cumsum(ot_tot) / sum(ot_tot)]
byemp[, is_90th := position >= 0.9 & shift(position) < 0.9]
stopifnot(nrow(byemp[is_90th == 1]) == 1)
share_1_0 <- (1 - byemp[is_90th == 1]$cum_ot[1]) - share_0
##  2 to 1 part 1
byemp <- all_pairs[, .(ot_tot = sum(work_sim_dr)), by = "num_emp1"]
setorder(byemp, "ot_tot", "num_emp1")
byemp[, position := (1:.N) / .N]
byemp[, cum_ot := cumsum(ot_tot) / sum(ot_tot)]
byemp[, is_90th := position >= 0.9 & shift(position) < 0.9]
stopifnot(nrow(byemp[is_90th == 1]) == 1)
share_2_1a <- (1 - byemp[is_90th == 1]$cum_ot[1])
byemp <- all_pairs[, .(ot_tot = sum(work_sim_r)), by = "num_emp1"]
setorder(byemp, "ot_tot", "num_emp1")
byemp[, position := (1:.N) / .N]
byemp[, cum_ot := cumsum(ot_tot) / sum(ot_tot)]
byemp[, is_90th := position >= 0.9 & shift(position) < 0.9]
stopifnot(nrow(byemp[is_90th == 1]) == 1)
share_2_1a <- (1 - byemp[is_90th == 1]$cum_ot[1]) - share_2_1a
##  2 to 1 part 2
byemp <- all_pairs[, .(ot_tot = sum(work_sim_dp)), by = "num_emp1"]
setorder(byemp, "ot_tot", "num_emp1")
byemp[, position := (1:.N) / .N]
byemp[, cum_ot := cumsum(ot_tot) / sum(ot_tot)]
byemp[, is_90th := position >= 0.9 & shift(position) < 0.9]
stopifnot(nrow(byemp[is_90th == 1]) == 1)
share_2_1b <- (1 - byemp[is_90th == 1]$cum_ot[1])
byemp <- all_pairs[, .(ot_tot = sum(work_sim_p)), by = "num_emp1"]
setorder(byemp, "ot_tot", "num_emp1")
byemp[, position := (1:.N) / .N]
byemp[, cum_ot := cumsum(ot_tot) / sum(ot_tot)]
byemp[, is_90th := position >= 0.9 & shift(position) < 0.9]
stopifnot(nrow(byemp[is_90th == 1]) == 1)
share_2_1b <- (1 - byemp[is_90th == 1]$cum_ot[1]) - share_2_1b

shapley <- share_2_1a * 1/4 + share_2_1b * 1/4 + share_1_0 * 1/4 + share_just * 1/4
res <- rbind(res, data.table(Channel = "Informal Network", allelse = share_except, just = share_just, shapley = shapley))

## pref
# except it
byemp <- all_pairs[, .(ot_tot = sum(work_sim_p)), by = "num_emp1"]
setorder(byemp, "ot_tot", "num_emp1")
byemp[, position := (1:.N) / .N]
byemp[, cum_ot := cumsum(ot_tot) / sum(ot_tot)]
byemp[, is_90th := position >= 0.9 & shift(position) < 0.9]
stopifnot(nrow(byemp[is_90th == 1]) == 1)

share_except <- (1 - byemp[is_90th == 1]$cum_ot[1]) - share_top10_observed

# just shut it down
byemp <- all_pairs[, .(ot_tot = sum(work_sim_dr)), by = "num_emp1"]
setorder(byemp, "ot_tot", "num_emp1")
byemp[, position := (1:.N) / .N]
byemp[, cum_ot := cumsum(ot_tot) / sum(ot_tot)]
byemp[, is_90th := position >= 0.9 & shift(position) < 0.9]
stopifnot(nrow(byemp[is_90th == 1]) == 1)

share_just <- (1 - byemp[is_90th == 1]$cum_ot[1]) - share_top10_observed

## shapley
# all to 2 is share_just
## 1 to 0
byemp <- all_pairs[, .(ot_tot = sum(work_sim_p)), by = "num_emp1"]
setorder(byemp, "ot_tot", "num_emp1")
byemp[, position := (1:.N) / .N]
byemp[, cum_ot := cumsum(ot_tot) / sum(ot_tot)]
byemp[, is_90th := position >= 0.9 & shift(position) < 0.9]
stopifnot(nrow(byemp[is_90th == 1]) == 1)
share_1_0 <- (1 - byemp[is_90th == 1]$cum_ot[1]) - share_0
##  2 to 1 part 1
byemp <- all_pairs[, .(ot_tot = sum(work_sim_rp)), by = "num_emp1"]
setorder(byemp, "ot_tot", "num_emp1")
byemp[, position := (1:.N) / .N]
byemp[, cum_ot := cumsum(ot_tot) / sum(ot_tot)]
byemp[, is_90th := position >= 0.9 & shift(position) < 0.9]
stopifnot(nrow(byemp[is_90th == 1]) == 1)
share_2_1a <- (1 - byemp[is_90th == 1]$cum_ot[1])
byemp <- all_pairs[, .(ot_tot = sum(work_sim_r)), by = "num_emp1"]
setorder(byemp, "ot_tot", "num_emp1")
byemp[, position := (1:.N) / .N]
byemp[, cum_ot := cumsum(ot_tot) / sum(ot_tot)]
byemp[, is_90th := position >= 0.9 & shift(position) < 0.9]
stopifnot(nrow(byemp[is_90th == 1]) == 1)
share_2_1a <- (1 - byemp[is_90th == 1]$cum_ot[1]) - share_2_1a
##  2 to 1 part 2
byemp <- all_pairs[, .(ot_tot = sum(work_sim_dp)), by = "num_emp1"]
setorder(byemp, "ot_tot", "num_emp1")
byemp[, position := (1:.N) / .N]
byemp[, cum_ot := cumsum(ot_tot) / sum(ot_tot)]
byemp[, is_90th := position >= 0.9 & shift(position) < 0.9]
stopifnot(nrow(byemp[is_90th == 1]) == 1)
share_2_1b <- (1 - byemp[is_90th == 1]$cum_ot[1])
byemp <- all_pairs[, .(ot_tot = sum(work_sim_d)), by = "num_emp1"]
setorder(byemp, "ot_tot", "num_emp1")
byemp[, position := (1:.N) / .N]
byemp[, cum_ot := cumsum(ot_tot) / sum(ot_tot)]
byemp[, is_90th := position >= 0.9 & shift(position) < 0.9]
stopifnot(nrow(byemp[is_90th == 1]) == 1)
share_2_1b <- (1 - byemp[is_90th == 1]$cum_ot[1]) - share_2_1b

shapley <- share_2_1a * 1/4 + share_2_1b * 1/4 + share_1_0 * 1/4 + share_just * 1/4
res <- rbind(res, data.table(Channel = "Non-Wage Preferences", allelse = share_except, just = share_just, shapley = shapley))

## wages
# except it
byemp <- all_pairs[, .(ot_tot = sum(work_sim_r)), by = "num_emp1"]
setorder(byemp, "ot_tot", "num_emp1")
byemp[, position := (1:.N) / .N]
byemp[, cum_ot := cumsum(ot_tot) / sum(ot_tot)]
byemp[, is_90th := position >= 0.9 & shift(position) < 0.9]
stopifnot(nrow(byemp[is_90th == 1]) == 1)

share_except <- (1 - byemp[is_90th == 1]$cum_ot[1]) - share_top10_observed

# just shut it down
byemp <- all_pairs[, .(ot_tot = sum(work_sim_dp)), by = "num_emp1"]
setorder(byemp, "ot_tot", "num_emp1")
byemp[, position := (1:.N) / .N]
byemp[, cum_ot := cumsum(ot_tot) / sum(ot_tot)]
byemp[, is_90th := position >= 0.9 & shift(position) < 0.9]
stopifnot(nrow(byemp[is_90th == 1]) == 1)

share_just <- (1 - byemp[is_90th == 1]$cum_ot[1]) - share_top10_observed

## shapley
# all to 2 is share_just
## 1 to 0
byemp <- all_pairs[, .(ot_tot = sum(work_sim_r)), by = "num_emp1"]
setorder(byemp, "ot_tot", "num_emp1")
byemp[, position := (1:.N) / .N]
byemp[, cum_ot := cumsum(ot_tot) / sum(ot_tot)]
byemp[, is_90th := position >= 0.9 & shift(position) < 0.9]
stopifnot(nrow(byemp[is_90th == 1]) == 1)
share_1_0 <- (1 - byemp[is_90th == 1]$cum_ot[1]) - share_0
##  2 to 1 part 1
byemp <- all_pairs[, .(ot_tot = sum(work_sim_rp)), by = "num_emp1"]
setorder(byemp, "ot_tot", "num_emp1")
byemp[, position := (1:.N) / .N]
byemp[, cum_ot := cumsum(ot_tot) / sum(ot_tot)]
byemp[, is_90th := position >= 0.9 & shift(position) < 0.9]
stopifnot(nrow(byemp[is_90th == 1]) == 1)
share_2_1a <- (1 - byemp[is_90th == 1]$cum_ot[1])
byemp <- all_pairs[, .(ot_tot = sum(work_sim_p)), by = "num_emp1"]
setorder(byemp, "ot_tot", "num_emp1")
byemp[, position := (1:.N) / .N]
byemp[, cum_ot := cumsum(ot_tot) / sum(ot_tot)]
byemp[, is_90th := position >= 0.9 & shift(position) < 0.9]
stopifnot(nrow(byemp[is_90th == 1]) == 1)
share_2_1a <- (1 - byemp[is_90th == 1]$cum_ot[1]) - share_2_1a
##  2 to 1 part 2
byemp <- all_pairs[, .(ot_tot = sum(work_sim_dr)), by = "num_emp1"]
setorder(byemp, "ot_tot", "num_emp1")
byemp[, position := (1:.N) / .N]
byemp[, cum_ot := cumsum(ot_tot) / sum(ot_tot)]
byemp[, is_90th := position >= 0.9 & shift(position) < 0.9]
stopifnot(nrow(byemp[is_90th == 1]) == 1)
share_2_1b <- (1 - byemp[is_90th == 1]$cum_ot[1])
byemp <- all_pairs[, .(ot_tot = sum(work_sim_d)), by = "num_emp1"]
setorder(byemp, "ot_tot", "num_emp1")
byemp[, position := (1:.N) / .N]
byemp[, cum_ot := cumsum(ot_tot) / sum(ot_tot)]
byemp[, is_90th := position >= 0.9 & shift(position) < 0.9]
stopifnot(nrow(byemp[is_90th == 1]) == 1)
share_2_1b <- (1 - byemp[is_90th == 1]$cum_ot[1]) - share_2_1b

shapley <- share_2_1a * 1/4 + share_2_1b * 1/4 + share_1_0 * 1/4 + share_just * 1/4
res <- rbind(res, data.table(Channel = "Wages", allelse = share_except, just = share_just, shapley = shapley))
res[, allelse := round(allelse, digits = 3)]
res[, just := round(just, digits = 3)]
res[, shapley := round(shapley, digits = 3)]

colnames(res) <- c("Channel", "Remove All Others", "Remove Just Channel", "Shapley Average")

#' ---------------------------------------------------------------------------
#' SAVE DECOMPOSITION TABLE
#' ---------------------------------------------------------------------------

ensure_directory(CONFIG$tables_dir)
output <- kable(res, "latex", align = "c", booktabs = TRUE, linesep = c(""), escape = F, caption = NA, label = NA)
cat(output, file = file.path(CONFIG$tables_dir, "02_04_decomp.tex"))

log_message("Saved decomposition table")
log_complete(success = TRUE)
