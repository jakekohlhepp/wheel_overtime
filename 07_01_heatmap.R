#' =============================================================================
#' HEATMAP VISUALIZATION
#' =============================================================================
#' Create heatmaps of allocative efficiency across network/access cost grid.
#' Input:  file.path(CONFIG$data_dir, "04_01_estimate.Rdata")
#'         file.path(CONFIG$data_dir, "02_01_estimation_sample.rds")
#'         file.path(CONFIG$data_dir, "06_02_sim_random.rds")
#'         file.path(CONFIG$data_dir, "06_03_sim_auction_straight.rds")
#'         file.path(CONFIG$data_dir, "06_04_sim_informal.rds")
#'         file.path(CONFIG$data_dir, "06_05_sim_informal_reverse.rds")
#'         file.path(CONFIG$data_dir, "06_06_sim_informal_perfect.rds")
#' Output: file.path(CONFIG$figures_dir, "07_01_heatmap.png")
#'         file.path(CONFIG$figures_dir, "07_01_heatmap_less_granular.png")
#' =============================================================================

library('ggplot2')
library('data.table')
library('gridExtra')

source('config.R')
source('utils/logging.R')
log_init("07_01_heatmap.R")

#' ---------------------------------------------------------------------------
#' LOAD DATA
#' ---------------------------------------------------------------------------

log_message("Loading estimation data and simulation results")
load(file.path(CONFIG$data_dir, "04_01_estimate.Rdata"))

all_pairs <- readRDS(file.path(CONFIG$data_dir, "02_01_estimation_sample.rds"))
### compute average ot per shift.
avg_ot_hours <- sum(all_pairs$varot_hours) / sum(all_pairs$ot_work)

## compute minimum and maximum benchmarks
random_assignment <- readRDS(file.path(CONFIG$data_dir, "06_02_sim_random.rds"))
min_benchmark <- mean(random_assignment$worker_value) * avg_ot_hours

auctions <- readRDS(file.path(CONFIG$data_dir, "06_03_sim_auction_straight.rds"))
auctions[, worker_value := worker_value * avg_ot_hours]
auctions[, worker_surplus := worker_surplus * avg_ot_hours]
auctions_sum <- auctions[, .(mean_ineq = mean(share_top10),
                             mean_allocative = mean(worker_value), mean_wage_bill = mean(wage_bill),
                             mean_workersurplus = mean(worker_surplus))]
max_benchmark <- auctions_sum$mean_allocative[1]

#' ---------------------------------------------------------------------------
#' COMBINE SIMULATION RESULTS
#' ---------------------------------------------------------------------------

log_message("Combining simulation results across regimes")
results <- readRDS(file.path(CONFIG$data_dir, "06_04_sim_informal.rds"))
results[, regime := "Observed"]
results_together <- copy(results)
results <- readRDS(file.path(CONFIG$data_dir, "06_06_sim_informal_perfect.rds"))
results[, regime := "Positive"]
results_together <- rbind(results_together, results)
results <- readRDS(file.path(CONFIG$data_dir, "06_05_sim_informal_reverse.rds"))
results[, regime := "Negative"]
results_together <- rbind(results_together, results)

agg_res <- results_together[, .(avg_alloc = mean(worker_value) * avg_ot_hours), by = c("network_reduction", "access_cost", "regime")]
agg_res[, avg_alloc := (avg_alloc - min_benchmark) / (max_benchmark - min_benchmark)]
agg_res[, network_reduction := network_reduction / coef(mod_mod)['ot_rate'] * avg_ot_hours]
agg_res[, access_cost := -access_cost / coef(mod_mod)['ot_rate'] * avg_ot_hours]

#' ---------------------------------------------------------------------------
#' CONTINUOUS HEATMAP
#' ---------------------------------------------------------------------------

log_message("Creating continuous heatmap")
for_disp <- copy(agg_res[network_reduction <= 5 & access_cost <= 400])

p_cont <- ggplot(for_disp, aes(access_cost, network_reduction, fill = avg_alloc)) +
  geom_tile(width = max(for_disp$access_cost) / 5, height = max(for_disp$network_reduction) / 4) + theme_bw() + scale_fill_distiller(palette = "RdYlGn", direction = 1) +
  geom_vline(xintercept = -coef(mod_mod)['opp_dist'] / coef(mod_mod)['ot_rate'] * avg_ot_hours, linetype = "dashed") +
  geom_hline(yintercept = coef(mod_mod)['suppliers_interacted'] / coef(mod_mod)['ot_rate'] * avg_ot_hours, linetype = "dashed") +
  facet_wrap(~ regime, ncol = 2) +
  ylab("Reduction per Potential Supplier ($)") +
  xlab("Overtime Access Cost ($)")
ggsave(file.path(CONFIG$figures_dir, "07_01_heatmap_continuous.png"), p_cont, width = 12, height = 8, units = "in")

#' ---------------------------------------------------------------------------
#' DISCRETE HEATMAP
#' ---------------------------------------------------------------------------

log_message("Creating discrete heatmaps")
## Compute reference line positions before converting to factors
est_access_cost <- -coef(mod_mod)['opp_dist'] / coef(mod_mod)['ot_rate'] * avg_ot_hours
est_network_red <- coef(mod_mod)['suppliers_interacted'] / coef(mod_mod)['ot_rate'] * avg_ot_hours
ac_levels <- sort(unique(agg_res$access_cost))
nr_levels <- sort(unique(agg_res$network_reduction))
ref_ac_idx <- which.min(abs(ac_levels - est_access_cost))
ref_nr_idx <- which.min(abs(nr_levels - est_network_red))

## label factor variables for viewing
agg_res[, network_reduction := as.factor(network_reduction)]
agg_res[, access_cost := as.factor(access_cost)]
stopifnot(uniqueN(agg_res[, c("access_cost", "network_reduction", "regime")]) == nrow(agg_res))

lab_less <- as.character(round(as.numeric(levels(agg_res$access_cost))))
lab_less[!as.logical(1:length(lab_less) %% 2)] <- ""

lab_less_y <- as.character(round(as.numeric(levels(agg_res$network_reduction)), 2))
lab_less_y[!as.logical(1:length(lab_less_y) %% 2)] <- ""

ensure_directory(CONFIG$figures_dir)

ggplot(agg_res, aes(access_cost, network_reduction, fill = avg_alloc)) +
  geom_tile() + theme_bw() + scale_fill_distiller(palette = "RdYlGn", direction = 1) +
  geom_vline(xintercept = ref_ac_idx, linetype = "dashed") +
  geom_hline(yintercept = ref_nr_idx, linetype = "dashed") +
  facet_wrap(~ regime, ncol = 2) +
  ylab("Reduction per Potential Supplier ($)") +
  xlab("Overtime Access Cost ($)") +
  scale_x_discrete(labels = lab_less) +
  scale_y_discrete(labels = lab_less_y) +
  labs(fill = "% Efficiency") + theme(legend.title = element_text(size = 15), legend.text = element_text(size = 15), axis.title = element_text(size = 30), axis.text = element_text(size = 15), strip.text.x = element_text(size = 30))
ggsave(file.path(CONFIG$figures_dir, "07_01_heatmap.png"), width = 12, height = 8, units = "in")

#' ---------------------------------------------------------------------------
#' DISCRETIZED COLOR HEATMAP
#' ---------------------------------------------------------------------------

log_message("Creating discretized color heatmap")
lab_less <- as.character(round(as.numeric(levels(agg_res$access_cost))))
lab_less[!as.logical(1:length(lab_less) %% 2)] <- ""

lab_less_y <- as.character(round(as.numeric(levels(agg_res$network_reduction)), 2))
lab_less_y[!as.logical(1:length(lab_less_y) %% 2)] <- ""

ggplot(agg_res, aes(access_cost, network_reduction, fill = avg_alloc)) +
  geom_tile() + theme_bw() + scale_fill_fermenter(palette = "RdYlGn", direction = 1) +
  geom_vline(xintercept = ref_ac_idx, linetype = "dashed") +
  geom_hline(yintercept = ref_nr_idx, linetype = "dashed") +
  facet_wrap(~ regime, ncol = 2) +
  ylab("Reduction per Potential Supplier ($)") +
  xlab("Overtime Access Cost ($)") +
  scale_x_discrete(labels = lab_less) +
  scale_y_discrete(labels = lab_less_y) +
  labs(fill = "% Efficiency") + theme(legend.title = element_text(size = 15), legend.text = element_text(size = 15), axis.title = element_text(size = 30), axis.text = element_text(size = 15), strip.text.x = element_text(size = 30))
ggsave(file.path(CONFIG$figures_dir, "07_01_heatmap_less_granular.png"), width = 12, height = 8, units = "in")

log_complete(success = TRUE)
