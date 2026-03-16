#' =============================================================================
#' HEATMAP VISUALIZATION
#' =============================================================================
#' Create heatmaps of allocative efficiency across network/access cost grid.
#' Input:  file.path(CONFIG$data_dir, "02_00_estimate.Rdata")
#'         file.path(CONFIG$data_dir, "00_02_estimation_sample.rds")
#'         file.path(CONFIG$data_dir, "03_00_sim_random.rds")
#'         file.path(CONFIG$data_dir, "03_01_sim_auction_straight.rds")
#'         file.path(CONFIG$data_dir, "03_02_sim_informal.rds")
#'         file.path(CONFIG$data_dir, "03_03_sim_informal_reverse.rds")
#'         file.path(CONFIG$data_dir, "03_04_sim_informal_perfect.rds")
#' Output: file.path(CONFIG$figures_dir, "03_98_heatmap.png")
#'         file.path(CONFIG$figures_dir, "03_98_heatmap_less_granular.png")
#' =============================================================================

library('ggplot2')
library('data.table')
library('gridExtra')

source('config.R')
source('utils/logging.R')
log_init("03_98_heatmap.R")

#' ---------------------------------------------------------------------------
#' LOAD DATA
#' ---------------------------------------------------------------------------

log_message("Loading estimation data and simulation results")
load(file.path(CONFIG$data_dir, "02_00_estimate.Rdata"))

all_pairs <- readRDS(file.path(CONFIG$data_dir, "00_02_estimation_sample.rds"))
### compute average ot per shift.
avg_ot_hours <- sum(all_pairs$varot_hours) / sum(all_pairs$ot_work)

## compute minimum and maximum benchmarks
random_assignment <- readRDS(file.path(CONFIG$data_dir, "03_00_sim_random.rds"))
min_benchmark <- mean(random_assignment$worker_value) * avg_ot_hours

auctions <- readRDS(file.path(CONFIG$data_dir, "03_01_sim_auction_straight.rds"))
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
results <- readRDS(file.path(CONFIG$data_dir, "03_02_sim_informal.rds"))
setnames(results, "network_reduce", "network_reduction")
results[, regime := "Observed"]
results_together <- copy(results)
results <- readRDS(file.path(CONFIG$data_dir, "03_04_sim_informal_perfect.rds"))
results[, regime := "Positive"]
results_together <- rbind(results_together, results)
results <- readRDS(file.path(CONFIG$data_dir, "03_03_sim_informal_reverse.rds"))
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

ggplot(for_disp, aes(access_cost, network_reduction, fill = avg_alloc)) +
  geom_tile(width = max(for_disp$access_cost) / 5, height = max(for_disp$network_reduction) / 4) + theme_bw() + scale_fill_distiller(palette = "RdYlGn", direction = 1) +
  ylab("Reduction per Potential Supplier ($)") +
  xlab("Overtime Access Cost ($)") + facet_wrap(~ regime, ncol = 2) +
  geom_vline(xintercept = -coef(mod_mod)['opp_dist'] / coef(mod_mod)['ot_rate'] * avg_ot_hours, linetype = "dashed") +
  geom_hline(yintercept = coef(mod_mod)['suppliers_interacted'] / coef(mod_mod)['ot_rate'] * avg_ot_hours, linetype = "dashed") +
  facet_wrap(~ regime, ncol = 2) +
  ylab("Reduction per Potential Supplier ($)") +
  xlab("Overtime Access Cost ($)")

#' ---------------------------------------------------------------------------
#' DISCRETE HEATMAP
#' ---------------------------------------------------------------------------

log_message("Creating discrete heatmaps")
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
  geom_vline(xintercept = as.numeric(labels(agg_res$access_cost)[which(levels(agg_res$access_cost) == as.character(-coef(mod_mod)['opp_dist'] / coef(mod_mod)['ot_rate'] * avg_ot_hours))]), linetype = "dashed") +
  geom_hline(yintercept = as.numeric(labels(agg_res$network_reduction)[which(levels(agg_res$network_reduction) == as.character(coef(mod_mod)['suppliers_interacted'] / coef(mod_mod)['ot_rate'] * avg_ot_hours))]), linetype = "dashed") +
  facet_wrap(~ regime, ncol = 2) +
  ylab("Reduction per Potential Supplier ($)") +
  xlab("Overtime Access Cost ($)") +
  scale_x_discrete(labels = lab_less) +
  scale_y_discrete(labels = lab_less_y) +
  labs(fill = "% Efficiency") + theme(legend.title = element_text(size = 15), legend.text = element_text(size = 15), axis.title = element_text(size = 30), axis.text = element_text(size = 15), strip.text.x = element_text(size = 30))
ggsave(file.path(CONFIG$figures_dir, "03_98_heatmap.png"), width = 12, height = 8, units = "in")

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
  geom_vline(xintercept = as.numeric(labels(agg_res$access_cost)[which(levels(agg_res$access_cost) == as.character(-coef(mod_mod)['opp_dist'] / coef(mod_mod)['ot_rate'] * avg_ot_hours))]), linetype = "dashed") +
  geom_hline(yintercept = as.numeric(labels(agg_res$network_reduction)[which(levels(agg_res$network_reduction) == as.character(coef(mod_mod)['suppliers_interacted'] / coef(mod_mod)['ot_rate'] * avg_ot_hours))]), linetype = "dashed") +
  facet_wrap(~ regime, ncol = 2) +
  ylab("Reduction per Potential Supplier ($)") +
  xlab("Overtime Access Cost ($)") +
  scale_x_discrete(labels = lab_less) +
  scale_y_discrete(labels = lab_less_y) +
  labs(fill = "% Efficiency") + theme(legend.title = element_text(size = 15), legend.text = element_text(size = 15), axis.title = element_text(size = 30), axis.text = element_text(size = 15), strip.text.x = element_text(size = 30))
ggsave(file.path(CONFIG$figures_dir, "03_98_heatmap_less_granular.png"), width = 12, height = 8, units = "in")

log_complete(success = TRUE)
