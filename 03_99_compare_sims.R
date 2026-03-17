#' =============================================================================
#' COMPARE SIMULATION OUTCOMES
#' =============================================================================
#' Compares outcomes across all counterfactual simulations: informal trading,
#' uniform-wage auction, uniform-markdown auction, and random assignment.
#' Produces equity-efficiency frontier, histograms, summary tables, and
#' individual-level winner/loser analysis.
#'
#' Input:  data/02_00_estimate.Rdata               (logit model)
#'         data/00_01_estimation_sample.rds         (estimation sample)
#'         data/03_03_sim_informal.rds              (status quo sim)
#'         data/03_02_sim_auction_straight.rds      (uniform-wage auction)
#'         data/03_02_sim_auction_dev.rds           (uniform-markdown auction)
#'         data/03_00_sim_frontier.rds      (random assignment frontier)
#'         data/03_02_sim_auction_dev_markdown.rds  (markdown by date)
#'         data/03_02_sim_auction_straight_wage.rds (wage by date)
#'         data/03_03_sim_informal_byworker.rds     (worker-level informal)
#'         data/03_02_sim_auction_dev_byworker.rds  (worker-level markdown)
#'         data/03_02_sim_auction_straight_byworker.rds (worker-level wage)
#'         data/03_04_sim_informal_reverse.rds      (reverse access costs)
#'         data/03_05_sim_informal_perfect.rds      (perfect access)
#' Output: out/figures/03_99_*.png
#'         out/tables/03_99_*.tex
#' =============================================================================

library('data.table')
library('alpaca')
library('lubridate')
library('ggplot2')
library('kableExtra')
library('scales')

source('config.R')
source('utils/logging.R')

log_init("03_99_compare_sims.R")
log_message("Comparing simulation outcomes")

#' -----------------------------------------------------------------------------
#' LOAD DATA AND MODEL
#' -----------------------------------------------------------------------------

load(file.path(CONFIG$data_dir, "02_00_estimate.Rdata"))
all_pairs <- readRDS(file.path(CONFIG$data_dir, "00_01_estimation_sample.rds"))

## Average OT hours per shift
avg_ot_hours <- sum(all_pairs$varot_hours) / sum(all_pairs$ot_work)

officer_fe <- data.table(officer_fe = getFEs(mod_mod)$num_emp1, num_emp1 = as.numeric(names(getFEs(mod_mod)$num_emp1)))
all_pairs <- merge(all_pairs, officer_fe, by = "num_emp1", all.x = TRUE)
date_fe <- data.table(date_fe = getFEs(mod_mod)$analysis_workdate, analysis_workdate = as.Date(names(getFEs(mod_mod)$analysis_workdate)))
all_pairs <- merge(all_pairs, date_fe, by = "analysis_workdate", all.x = TRUE)

## Exclude officers without fixed effects
print(uniqueN(all_pairs[is.na(officer_fe)]$num_emp1) / uniqueN(all_pairs$num_emp1))
all_pairs <- all_pairs[!is.na(officer_fe), ]

#' -----------------------------------------------------------------------------
#' IMPORT SIMULATION RESULTS (AGGREGATE)
#' -----------------------------------------------------------------------------

## Status quo (informal trading)
status_quo <- readRDS(file.path(CONFIG$data_dir, "03_03_sim_informal.rds"))
status_quo[, worker_value := worker_value * avg_ot_hours]
status_quo <- status_quo[access_cost_mult == 1, ]
status_quo[, worker_surplus := worker_surplus * avg_ot_hours]
status_quo[, access_cost_mult := NULL]
status_quo_sum <- status_quo[, .(mean_ineq = mean(share_top10),
                                 mean_allocative = mean(worker_value),
                                 mean_wage_bill = mean(wage_bill),
                                 mean_workersurplus = mean(worker_surplus))]

## Uniform-wage auction
auctions <- readRDS(file.path(CONFIG$data_dir, "03_02_sim_auction_straight.rds"))
auctions[, worker_value := worker_value * avg_ot_hours]
auctions[, worker_surplus := worker_surplus * avg_ot_hours]
auctions_sum <- auctions[, .(mean_ineq = mean(share_top10),
                              mean_allocative = mean(worker_value),
                              mean_wage_bill = mean(wage_bill),
                              mean_workersurplus = mean(worker_surplus))]

## Uniform-markdown auction
auctions_dev <- readRDS(file.path(CONFIG$data_dir, "03_02_sim_auction_dev.rds"))
auctions_dev[, worker_value := worker_value * avg_ot_hours]
auctions_dev[, worker_surplus := worker_surplus * avg_ot_hours]
auctions_dev_sum <- auctions_dev[, .(mean_ineq = mean(share_top10),
                                      mean_allocative = mean(worker_value),
                                      mean_wage_bill = mean(wage_bill),
                                      mean_workersurplus = mean(worker_surplus))]

## Random assignment frontier
managers_val <- readRDS(file.path(CONFIG$data_dir, "03_00_sim_frontier.rds"))
managers_val_sum <- managers_val[, .(mean_surplus = mean(worker_value) * avg_ot_hours,
                                     mean_ineq = mean(share_top10)), by = "savy_num"]

#' -----------------------------------------------------------------------------
#' MARKDOWN AND WAGE PLOTS BY DATE
#' -----------------------------------------------------------------------------

ensure_directory(CONFIG$figures_dir)
ensure_directory(CONFIG$tables_dir)

results_wage <- readRDS(file.path(CONFIG$data_dir, "03_02_sim_auction_dev_markdown.rds"))
results_wage <- results_wage[, .(mean_wage = mean(sim_markdown)), by = "analysis_workdate"]

ggplot(data = results_wage, aes(x = analysis_workdate, y = mean_wage)) +
  geom_point() + ylab("Avg. Uniform-Markdown ($)") + xlab("Date") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title = element_text(size = 30), axis.text = element_text(size = 15),
        legend.title = element_text(size = 15), legend.text = element_text(size = 15))
ggsave(file.path(CONFIG$figures_dir, "03_99_markdowns_bydate.png"), width = 12, height = 8, units = "in")

results_wage <- readRDS(file.path(CONFIG$data_dir, "03_02_sim_auction_straight_wage.rds"))
results_wage <- results_wage[, .(mean_wage = -mean(sim_win_wage)), by = "analysis_workdate"]

ggplot(data = results_wage, aes(x = analysis_workdate, y = mean_wage)) +
  geom_point() + ylab("Avg. Uniform-Wage ($)") + xlab("Date") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title = element_text(size = 30), axis.text = element_text(size = 15),
        legend.title = element_text(size = 15), legend.text = element_text(size = 15))
ggsave(file.path(CONFIG$figures_dir, "03_99_uniformwages_bydate.png"), width = 12, height = 8, units = "in")

#' -----------------------------------------------------------------------------
#' EQUITY-EFFICIENCY FRONTIER
#' -----------------------------------------------------------------------------

ggplot(data = managers_val_sum[, c("mean_surplus", "mean_ineq")], aes(x = mean_surplus, y = mean_ineq)) +
  geom_line(linewidth = 2) +
  annotate("point", x = auctions_sum$mean_allocative, y = auctions_sum$mean_ineq, colour = "blue", size = 3) +
  annotate("text", x = auctions_sum$mean_allocative - 700000, y = auctions_sum$mean_ineq, colour = "blue", label = "Uniform-Wage Auction", size = 20 / .pt) +
  annotate("point", x = auctions_dev_sum$mean_allocative, y = auctions_dev_sum$mean_ineq, colour = "darkgreen", size = 3) +
  annotate("text", x = auctions_dev_sum$mean_allocative - 800000, y = -0.005 + auctions_dev_sum$mean_ineq, colour = "darkgreen", label = "Uniform-Markdown Auction", size = 20 / .pt) +
  annotate("point", x = status_quo_sum$mean_allocative, y = status_quo_sum$mean_ineq, colour = "red", size = 3) +
  annotate("text", x = status_quo_sum$mean_allocative - 500000, y = 0.0025 + status_quo_sum$mean_ineq, colour = "red", label = "Informal Trade", size = 20 / .pt) +
  xlab("Allocative Efficiency ($)") + ylab("Overtime Share of Top 10%") +
  theme_bw() +
  scale_x_continuous(labels = function(l) { paste0(round(l / 1e6, 2), "m") }) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title = element_text(size = 30), axis.text = element_text(size = 15),
        legend.title = element_text(size = 15), legend.text = element_text(size = 15))
ggsave(file.path(CONFIG$figures_dir, "03_99_equity_efficiency.png"), width = 12, height = 8, units = "in")

## Differences between random and max
print(managers_val_sum[savy_num == 0]$mean_surplus - auctions_sum$mean_allocative)
print(managers_val_sum[savy_num == 0]$mean_surplus - status_quo_sum$mean_allocative)
print((managers_val_sum[savy_num == 0]$mean_surplus - status_quo_sum$mean_allocative) /
        (managers_val_sum[savy_num == 0]$mean_surplus - auctions_sum$mean_allocative))

#' -----------------------------------------------------------------------------
#' AGGREGATE HISTOGRAMS
#' -----------------------------------------------------------------------------

auctions_dev[, a_type := "Uniform-Markdown Auction"]
auctions[, a_type := "Uniform-Wage Auction"]
status_quo[, a_type := "Informal Trading (Status Quo)"]
all_auctions <- rbind(auctions_dev, auctions, status_quo)

ggplot(all_auctions, aes(x = share_top10, fill = a_type)) +
  geom_histogram(position = "identity", alpha = 0.8, bins = 100) +
  xlab("Top 10% Share of Overtime") + ylab("Simulation Count") +
  theme_bw() + guides(fill = guide_legend(title = "Assignment Mechanism")) +
  geom_vline(xintercept = status_quo$ineq, linetype = "dashed", linewidth = 1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title = element_text(size = 30), axis.text = element_text(size = 15),
        legend.title = element_text(size = 15), legend.text = element_text(size = 15))
ggsave(file.path(CONFIG$figures_dir, "03_99_inequality_hist.png"), width = 12, height = 8, units = "in")

ggplot(all_auctions, aes(x = worker_value, fill = a_type)) +
  geom_histogram(position = "identity", alpha = 0.8, bins = 100) +
  xlab("Worker Non-Wage Utility ($)") + ylab("Simulation Count") +
  scale_x_continuous(labels = function(l) { paste0(round(l / 1e6, 2), "m") }) +
  theme_bw() + guides(fill = guide_legend(title = "Auction Type")) +
  geom_vline(xintercept = status_quo$allocative, linetype = "dashed", size = 1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title = element_text(size = 30), axis.text = element_text(size = 15),
        legend.title = element_text(size = 15), legend.text = element_text(size = 15))
ggsave(file.path(CONFIG$figures_dir, "03_99_nonwage_utility_hist.png"), width = 12, height = 8, units = "in")

ggplot(all_auctions, aes(x = worker_surplus, fill = a_type)) +
  geom_histogram(position = "identity", alpha = 0.8, bins = 100) +
  xlab("Worker Surplus ($)") + ylab("Simulation Count") +
  scale_x_continuous(labels = function(l) { paste0(round(l / 1e6, 2), "m") }) +
  theme_bw() + guides(fill = guide_legend(title = "Auction Type")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title = element_text(size = 30), axis.text = element_text(size = 15),
        legend.title = element_text(size = 15), legend.text = element_text(size = 15))
ggsave(file.path(CONFIG$figures_dir, "03_99_worker_surplus_hist.png"), width = 12, height = 8, units = "in")

ggplot(all_auctions, aes(x = wage_bill, fill = a_type)) +
  geom_histogram(position = "identity", alpha = 0.8, binwidth = 5000) +
  xlab("Total Wage Bill ($)") + ylab("Simulation Count") +
  scale_x_continuous(labels = function(l) { paste0(round(l / 1e6, 1), "m") }) +
  theme_bw() + guides(fill = guide_legend(title = "Auction Type")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title = element_text(size = 30), axis.text = element_text(size = 15),
        legend.title = element_text(size = 15), legend.text = element_text(size = 15))
ggsave(file.path(CONFIG$figures_dir, "03_99_wages_hist.png"), width = 12, height = 8, units = "in")

#' -----------------------------------------------------------------------------
#' SUMMARY TABLES
#' -----------------------------------------------------------------------------

fortable <- all_auctions[, .(
  p5_wage_bill = quantile(wage_bill, p = 0.05), mean_wage_bill = mean(wage_bill), p95_wage_bill = quantile(wage_bill, p = 0.95),
  p5_worker_surplus = quantile(worker_surplus, p = 0.05), mean_worker_surplus = mean(worker_surplus), p95_worker_surplus = quantile(worker_surplus, p = 0.95),
  p5_worker_value = quantile(worker_value, p = 0.05), mean_worker_value = mean(worker_value), p95_worker_value = quantile(worker_value, p = 0.95),
  p5_share_top10 = quantile(share_top10, p = 0.05), mean_share_top10 = mean(share_top10), p95_share_top10 = quantile(share_top10, p = 0.95)
), by = "a_type"]

## Format table
fortable[, p5_share_top10 := round(p5_share_top10, digits = 3)]
fortable[, mean_share_top10 := round(mean_share_top10, digits = 3)]
fortable[, p95_share_top10 := round(p95_share_top10, digits = 3)]

fortable[, p5_worker_surplus := paste0("\\$", format(round(as.numeric(p5_worker_surplus), 0), nsmall = 0, big.mark = ","))]
fortable[, mean_worker_surplus := paste0("\\$", format(round(as.numeric(mean_worker_surplus), 0), nsmall = 0, big.mark = ","))]
fortable[, p95_worker_surplus := paste0("\\$", format(round(as.numeric(p95_worker_surplus), 0), nsmall = 0, big.mark = ","))]

fortable[, p5_wage_bill := paste0("\\$", format(round(as.numeric(p5_wage_bill), 0), nsmall = 0, big.mark = ","))]
fortable[, mean_wage_bill := paste0("\\$", format(round(as.numeric(mean_wage_bill), 0), nsmall = 0, big.mark = ","))]
fortable[, p95_wage_bill := paste0("\\$", format(round(as.numeric(p95_wage_bill), 0), nsmall = 0, big.mark = ","))]

fortable[, p5_worker_value := paste0("\\$", format(round(as.numeric(p5_worker_value), 0), nsmall = 0, big.mark = ","))]
fortable[, mean_worker_value := paste0("\\$", format(round(as.numeric(mean_worker_value), 0), nsmall = 0, big.mark = ","))]
fortable[, p95_worker_value := paste0("\\$", format(round(as.numeric(p95_worker_value), 0), nsmall = 0, big.mark = ","))]

setnames(fortable, "a_type", "Assignment Mechanism")
colnames(fortable)[-1] <- rep(c("p5", "Mean", "p95"), 4)

kable(fortable[, .SD, .SDcols = 1:7], "latex", align = "c", booktabs = TRUE, linesep = c(""), escape = F, caption = NA, label = NA) %>%
  add_header_above(c(" " = 1, "Overtime Wage Costs" = 3, "Worker Surplus" = 3)) %>%
  cat(., file = file.path(CONFIG$tables_dir, "03_99_system_summary_wages_surplus.tex"))

kable(fortable[, .SD, .SDcols = c(1, 8:13)], "latex", align = "c", booktabs = TRUE, linesep = c(""), escape = F, caption = NA, label = NA) %>%
  add_header_above(c(" " = 1, "Allocative Efficiency" = 3, "Share OT by Top 10" = 3)) %>%
  cat(., file = file.path(CONFIG$tables_dir, "03_99_system_summary_inequality_allocative.tex"))

#' -----------------------------------------------------------------------------
#' INDIVIDUAL WINNERS AND LOSERS
#' -----------------------------------------------------------------------------

status_quo_byemp <- readRDS(file.path(CONFIG$data_dir, "03_03_sim_informal_byworker.rds"))
status_quo_byemp_sum <- status_quo_byemp[access_cost_mult == 1, .(s_surplus = mean(worker_surplus * avg_ot_hours),
                                                                    s_otpay = mean(total_ot_pay),
                                                                    s_totalot = mean(total_ot)), by = "num_emp1"]

auction_dev_byemp <- readRDS(file.path(CONFIG$data_dir, "03_02_sim_auction_dev_byworker.rds"))
auction_dev_byemp_sum <- auction_dev_byemp[, .(m_surplus = mean(worker_surplus * avg_ot_hours),
                                                m_otpay = mean(total_ot_pay),
                                                m_totalot = mean(total_ot)), by = "num_emp1"]

auction_byemp <- readRDS(file.path(CONFIG$data_dir, "03_02_sim_auction_straight_byworker.rds"))
auction_byemp_sum <- auction_byemp[, .(a_surplus = mean(worker_surplus * avg_ot_hours),
                                        a_otpay = mean(total_ot_pay),
                                        a_totalot = mean(total_ot)), by = "num_emp1"]

all_auctions_byemp <- merge(status_quo_byemp_sum, auction_dev_byemp_sum, by = "num_emp1", all.x = TRUE)
all_auctions_byemp <- merge(all_auctions_byemp, auction_byemp_sum, by = "num_emp1", all.x = TRUE)
all_auctions_byemp <- merge(all_auctions_byemp,
                             all_pairs[, .(avg_degree = mean(l_wheel_degree, na.rm = TRUE),
                                           an_age = mean(an_age)), by = c("num_emp1", "officer_fe")],
                             by = "num_emp1", all.x = TRUE)

all_auctions_byemp[, as_surplus := a_surplus - s_surplus]
all_auctions_byemp[, as_ot := a_totalot - s_totalot]

all_auctions_byemp[, ms_surplus := m_surplus - s_surplus]
all_auctions_byemp[, ms_ot := m_totalot - s_totalot]
all_auctions_byemp[, ms_otpay := m_otpay - s_otpay]

all_auctions_byemp[, ma_surplus := m_surplus - a_surplus]
all_auctions_byemp[, ma_ot := m_totalot - a_totalot]
all_auctions_byemp[, ma_otpay := m_otpay - a_otpay]

ggplot(all_auctions_byemp, aes(x = frank(avg_degree), y = frank(officer_fe), color = as_surplus)) +
  geom_point(size = 4) + xlab("Connectedness Rank") + ylab("Officer FE Rank") +
  theme_bw() + scale_color_gradient2(midpoint = 0, low = "firebrick", mid = "white",
                                      high = "forestgreen", space = "Lab") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title = element_text(size = 30), axis.text = element_text(size = 15),
        legend.title = element_text(size = 15), legend.text = element_text(size = 15))

## OT change by officer FE rank
ggplot(all_auctions_byemp,
       aes(x = reorder(num_emp1, frank(-officer_fe)), y = as_ot)) +
  geom_bar(stat = "identity") + theme_bw() +
  ylab('Individual worker Surplus Change ($)') + xlab('Officer Fixed Effect Rank') +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title = element_text(size = 30), axis.text = element_text(size = 15),
        legend.title = element_text(size = 15), legend.text = element_text(size = 15))

## Surplus change by officer FE rank
ggplot(all_auctions_byemp,
       aes(x = reorder(num_emp1, frank(-officer_fe)), y = as_surplus)) +
  geom_bar(stat = "identity") + theme_bw() +
  ylab('Individual Worker Overtime Change') + xlab('Officer Fixed Effect Rank') +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title = element_text(size = 30), axis.text = element_text(size = 15),
        legend.title = element_text(size = 15), legend.text = element_text(size = 15))

## OT change by supplier count rank
ggplot(all_auctions_byemp,
       aes(x = reorder(num_emp1, frank(-avg_degree)), y = as_ot)) +
  geom_bar(stat = "identity") + theme_bw() +
  ylab('Individual Worker Overtime Change') + xlab('Average Supplier Count Rank') +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title = element_text(size = 30), axis.text = element_text(size = 15),
        legend.title = element_text(size = 15), legend.text = element_text(size = 15))

#' -----------------------------------------------------------------------------
#' CARTEL ANALYSIS
#' -----------------------------------------------------------------------------

all_pairs[, observed_ot := sum(ot_work), by = "num_emp1"]
all_pairs[, is_cartel := sum(ot_work) >= 300, by = "num_emp1"]
all_auctions_byemp <- merge(all_auctions_byemp, unique(all_pairs[, c("is_cartel", "num_emp1", "observed_ot")]),
                             by = "num_emp1", all.x = TRUE)
setorder(all_auctions_byemp, -"observed_ot", "num_emp1")
all_auctions_byemp[, fe_rank := paste0(comma_format(accuracy = 1)(frank(-officer_fe)))]
cartel <- all_auctions_byemp[is_cartel == 1,
                              c("num_emp1", "fe_rank", "s_totalot", "a_totalot", "m_totalot",
                                "s_otpay", "a_otpay", "m_otpay", "s_surplus", "a_surplus", "m_surplus")]

cartel[, s_otpay := paste0("\\$", comma_format(accuracy = 1)(s_otpay))]
cartel[, a_otpay := paste0("\\$", comma_format(accuracy = 1)(a_otpay))]
cartel[, m_otpay := paste0("\\$", comma_format(accuracy = 1)(m_otpay))]

cartel[, s_surplus := paste0("\\$", comma_format(accuracy = 1)(s_surplus))]
cartel[, a_surplus := paste0("\\$", comma_format(accuracy = 1)(a_surplus))]
cartel[, m_surplus := paste0("\\$", comma_format(accuracy = 1)(m_surplus))]

cartel[, s_totalot := paste0(comma_format(accuracy = 1)(s_totalot))]
cartel[, a_totalot := paste0(comma_format(accuracy = 1)(a_totalot))]
cartel[, m_totalot := paste0(comma_format(accuracy = 1)(m_totalot))]

colnames(cartel) <- c("Officer ID", "FE Rank", rep(c("Informal", "Wage", "Markdown"), 3))
kable(cartel, "latex", align = "c", booktabs = TRUE, linesep = c(""), escape = F, caption = NA, label = NA) %>%
  add_header_above(c(" " = 2, "Overtime Shifts" = 3, "Overtime Pay" = 3, "Worker Surplus" = 3)) %>%
  cat(., file = file.path(CONFIG$tables_dir, "03_99_cartel_impacts.tex"))

#' -----------------------------------------------------------------------------
#' ACCESS COST ROBUSTNESS
#' -----------------------------------------------------------------------------

status_quo <- readRDS(file.path(CONFIG$data_dir, "03_03_sim_informal.rds"))
status_quo[, worker_value := worker_value * avg_ot_hours]
status_quo[, worker_surplus := worker_surplus * avg_ot_hours]
status_quo_sum <- status_quo[, .(mean_ineq = mean(share_top10),
                                 mean_allocative = mean(worker_value),
                                 mean_wage_bill = mean(wage_bill),
                                 mean_workersurplus = mean(worker_surplus)), by = "access_cost_mult"]

status_quo_reverse <- readRDS(file.path(CONFIG$data_dir, "03_04_sim_informal_reverse.rds"))
status_quo_reverse[, worker_value := worker_value * avg_ot_hours]
status_quo_reverse[, worker_surplus := worker_surplus * avg_ot_hours]
status_quo_reverse_sum <- status_quo_reverse[, .(mean_ineq = mean(share_top10),
                                                  mean_allocative = mean(worker_value),
                                                  mean_wage_bill = mean(wage_bill),
                                                  mean_workersurplus = mean(worker_surplus)), by = "access_cost_mult"]

status_quo_perfect <- readRDS(file.path(CONFIG$data_dir, "03_05_sim_informal_perfect.rds"))
status_quo_perfect[, worker_value := worker_value * avg_ot_hours]
status_quo_perfect[, worker_surplus := worker_surplus * avg_ot_hours]
status_quo_perfect_sum <- status_quo_perfect[, .(mean_ineq = mean(share_top10),
                                                  mean_allocative = mean(worker_value),
                                                  mean_wage_bill = mean(wage_bill),
                                                  mean_workersurplus = mean(worker_surplus)), by = "access_cost_mult"]

status_quo_perfect_sum[, type := "Positive"]
status_quo_reverse_sum[, type := "Negative"]
status_quo_sum[, type := "Status Quo"]
together <- rbind(status_quo_sum, status_quo_reverse_sum, status_quo_perfect_sum)
together[, mean_allocative := (mean_allocative - managers_val_sum[savy_num == 0]$mean_surplus) /
           (auctions_sum$mean_allocative - managers_val_sum[savy_num == 0]$mean_surplus)]
setnames(together, "type", "Regime")

ggplot(data = together, aes(x = access_cost_mult, y = mean_allocative, color = Regime)) +
  geom_line(size = 1) + geom_point(size = 2) +
  geom_vline(xintercept = 1, linetype = "dashed", linewidth = 1) +
  theme_bw() + ylab('Fraction Max Allocative Efficiency') + xlab('Access Cost Multiplier (M)') +
  theme(legend.position = "bottom") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title = element_text(size = 30), axis.text = element_text(size = 30),
        legend.title = element_text(size = 30), legend.text = element_text(size = 30))
ggsave(file.path(CONFIG$figures_dir, "03_99_access_cost_robustness.png"), width = 12, height = 8, units = "in")

log_complete(success = TRUE)
message("03_99_compare_sims complete")
