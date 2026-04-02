#' =============================================================================
#' DESCRIPTIVE FACTS AND NETWORK VISUALIZATION
#' =============================================================================
#' Input:  data/02_01_estimation_sample.rds
#'         data/{pre_network_prefix}{network_window_default}.csv (tenure, injury)
#'         Contact matrix via load_contact_matrix() (for network plots)
#'         {raw_pay_dir}/anonymized_data_073117.txt (bereavement, FMLA flags)
#' Output: out/tables/03_01_summary_stats.tex (stacked officer-day and officer-level summary stats)
#'         out/tables/03_01_claim.tex
#'         out/tables/03_01_nature.tex
#'         out/figures/03_01_*.png (various descriptive plots)
#' =============================================================================

source('config.R')
source('utils/logging.R')
log_init("03_01_facts.R")

#' ---------------------------------------------------------------------------
#' LOAD PACKAGES
#' ---------------------------------------------------------------------------

library('data.table')
library('lubridate')
library('kableExtra')
library('igraph')
library('qgraph')
library('ggplot2')
library('fixest')
library('stringr')
library('viridis')
library('stargazer')
library('almanac')
library('xtable')

#' ---------------------------------------------------------------------------
#' LOAD DATA
#' ---------------------------------------------------------------------------

log_message("Loading estimation sample")
all_pairs <- readRDS(file.path(CONFIG$data_dir, "02_01_estimation_sample.rds"))

#' ---------------------------------------------------------------------------
#' MERGE TENURE AND NATURE OF INJURY
#' ---------------------------------------------------------------------------

log_message("Merging tenure and nature of injury from pre-network CSV")
gettenure <- fread(file.path(CONFIG$data_dir, paste0(CONFIG$pre_network_prefix, CONFIG$network_window_default, ".csv")))
gettenure[, analysis_workdate := dmy(analysis_workdate)]
gettenure[, num_emp1 := as.integer(gsub(CONFIG$employee_name_pattern, "", employee_name))]

all_pairs <- merge(all_pairs, gettenure[, c("tenure", "natureofinjury", "analysis_workdate", "num_emp1")],
                   by = c("analysis_workdate", "num_emp1"), all.x = TRUE)
rm(gettenure)
all_pairs[, s_degree := l_degree / sd(l_degree, na.rm = TRUE)]

#' ---------------------------------------------------------------------------
#' SUMMARY STATISTICS
#' ---------------------------------------------------------------------------

log_message("Computing summary statistics")

format_summary_panel <- function(dt) {
  panel <- data.table(
    Variable = names(dt),
    Mean = sapply(dt, function(x) mean(x, na.rm = TRUE)),
    SD = sapply(dt, function(x) sd(x, na.rm = TRUE)),
    P25 = sapply(dt, function(x) as.numeric(quantile(x, probs = 0.25, na.rm = TRUE))),
    P75 = sapply(dt, function(x) as.numeric(quantile(x, probs = 0.75, na.rm = TRUE)))
  )

  for (col in c("Mean", "SD", "P25", "P75")) {
    panel[, (col) := sprintf("%.3f", get(col))]
  }

  panel
}

format_count <- function(x) {
  format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
}

sum_stat <- copy(all_pairs[, c("max_rate", "an_age", "tenure", "normal_work", "ot_work", "expected_earnings")])
names(sum_stat) <- c("Wage", "Age", "Tenure",
                     "Standard Work", "Overtime", "Expected Earnings")
officer_day_n_officers <- uniqueN(all_pairs$num_emp1)
officer_day_n_observations <- nrow(all_pairs)
stopifnot(officer_day_n_observations == CONFIG$expected_estimation_rows)
stopifnot(officer_day_n_officers == CONFIG$expected_estimation_officers)

#' ---------------------------------------------------------------------------
#' OFFICER-LEVEL SUMMARY
#' ---------------------------------------------------------------------------

log_message("Computing officer-level summary statistics")
setorder(all_pairs, "num_emp1", "analysis_workdate")
byofficer <- all_pairs[, .(rel_date = first(analysis_workdate), rate = first(max_rate), age = first(an_age), tenure = first(tenure),
                           count = .N, ot_count = sum(ot_work), work_count = sum(normal_work)
                           ), by = "num_emp1"]
byofficer[, age := age + as.numeric(CONFIG$estimation_start - rel_date) / 365.25]
byofficer[, tenure := tenure + as.numeric(CONFIG$estimation_start - rel_date) / 365.25]
byofficer <- byofficer[, c("rate", "age", "tenure", "count", "work_count", "ot_count")]
names(byofficer) <- c("Wage", "Age", "Tenure", "Days Active",
                     "Standard Work", "Overtime")
officer_level_n_officers <- nrow(byofficer)
officer_level_n_observations <- nrow(byofficer)
stopifnot(officer_level_n_officers == CONFIG$expected_estimation_officers)

stacked_summary <- rbindlist(list(
  data.table(
    Variable = sprintf("\\textit{Officer-Day Summary Statistics (N officers = %s, N observations = %s)}",
                       format_count(officer_day_n_officers),
                       format_count(officer_day_n_observations)),
    Mean = "", SD = "", P25 = "", P75 = ""
  ),
  format_summary_panel(sum_stat),
  data.table(
    Variable = sprintf("\\textit{Officer-Level Summary Statistics (N officers = %s, N observations = %s)}",
                       format_count(officer_level_n_officers),
                       format_count(officer_level_n_observations)),
    Mean = "", SD = "", P25 = "", P75 = ""
  ),
  format_summary_panel(byofficer)
), use.names = TRUE, fill = TRUE)

summary_output_path <- file.path(CONFIG$tables_dir, "03_01_summary_stats.tex")
legacy_officer_output_path <- file.path(CONFIG$tables_dir, "03_01_summary_stats_officer.tex")

ensure_directory(CONFIG$tables_dir)
kable(stacked_summary, "latex", align = c("l", "c", "c", "c", "c"),
      booktabs = TRUE, linesep = c(""), escape = FALSE, caption = NA, label = NA) %>%
  cat(., file = summary_output_path)

if (file.exists(legacy_officer_output_path)) {
  file.remove(legacy_officer_output_path)
}
log_message("Saved stacked summary statistics table")

#' ---------------------------------------------------------------------------
#' CLAIM AND INJURY TABLES
#' ---------------------------------------------------------------------------

log_message("Creating claim and injury tables")
fortable <- all_pairs[matched_injury == 1, .(count = .N), by = c("claimcause")]
setorder(fortable, -"count")
print(xtable(fortable, type = "latex"), file = file.path(CONFIG$tables_dir, "03_01_claim.tex"), include.rownames = FALSE)

fortable <- all_pairs[matched_injury == 1, .(count = .N), by = c("natureofinjury")]
setorder(fortable, -"count")
print(xtable(fortable, type = "latex"), file = file.path(CONFIG$tables_dir, "03_01_nature.tex"), include.rownames = FALSE)

#' ---------------------------------------------------------------------------
#' AGE DISPERSION
#' ---------------------------------------------------------------------------

log_message("Plotting age dispersion")
ensure_directory(CONFIG$figures_dir)

ggplot(all_pairs, aes(x = an_age)) +
  geom_histogram(fill = "black") + xlab("Age (Years)") + ylab("Officer-Day Count") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title = element_text(size = 30), axis.text = element_text(size = 15)) + theme_bw()

ggsave(file.path(CONFIG$figures_dir, "03_01_age_dispersion.png"), width = 12, height = 8, units = "in")

#' ---------------------------------------------------------------------------
#' WHEEL TURNING
#' ---------------------------------------------------------------------------

log_message("Plotting wheel turning")
all_pairs[, disp_med := med_circ * max_rank_date / 2 / pi]
ggplot(unique(all_pairs[floor_date(analysis_workdate, unit = "month") == as.Date('2015-10-01'), c("analysis_workdate", "disp_med")]),
       aes(x = analysis_workdate, y = disp_med)) +
  geom_point(fill = "black", size = 3) + geom_line(linetype = "dashed") +
  xlab("Work Date") + ylab("Circular Median of Officer Seniority Rank") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title = element_text(size = 30), axis.text = element_text(size = 15))

ggsave(file.path(CONFIG$figures_dir, "03_01_wheel_turning_oct.png"), width = 10, height = 10, units = "in")

ggplot(unique(all_pairs[floor_date(analysis_workdate, unit = "month") == as.Date('2015-07-01'), c("analysis_workdate", "disp_med")]),
       aes(x = analysis_workdate, y = disp_med)) +
  geom_point(fill = "black", size = 3) + geom_line(linetype = "dashed") +
  xlab("Work Date") + ylab("Circular Median of Officer Seniority Rank") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title = element_text(size = 30), axis.text = element_text(size = 15))

ggsave(file.path(CONFIG$figures_dir, "03_01_wheel_turning_july.png"), width = 10, height = 10, units = "in")

#' ---------------------------------------------------------------------------
#' WHEEL POSITION AND OVERTIME
#' ---------------------------------------------------------------------------

log_message("Plotting wheel position and overtime")
ggplot(all_pairs, aes(x = 1 - dist_from_med, y = as.numeric(ot_work))) +
  stat_summary_bin(fun = match.fun(mean), bins = 40,
                   color = 'black', geom = 'point', size = 2) +
  stat_summary_bin(fun.data = 'mean_cl_boot', bins = 40,
                   color = 'black', geom = 'errorbar', position = position_dodge(1), width = .05) +
  ylab("Fraction Officer-Days with Overtime") + xlab("Officer Angular Distance from Approximate Wheel") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title = element_text(size = 30), axis.text = element_text(size = 15))

ggsave(file.path(CONFIG$figures_dir, "03_01_wheel_predicts_ot.png"), width = 12, height = 8, units = "in")

#' ---------------------------------------------------------------------------
#' OVERTIME DISPERSION
#' ---------------------------------------------------------------------------

log_message("Plotting overtime dispersion")
ot_emp_level <- all_pairs[, .(ot_tot = sum(ot_work), ot_frac = sum(ot_work) / .N), by = "num_emp1"]

ggplot(ot_emp_level, aes(x = ot_frac)) +
  geom_histogram(fill = "black") + xlab("Fraction of Days Working Overtime") + ylab("Officer Count") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title = element_text(size = 30), axis.text = element_text(size = 15))

ggsave(file.path(CONFIG$figures_dir, "03_01_ot_dipersion_frac.png"), width = 12, height = 8, units = "in")

## among those who work 1, p25 is 0.023766  , p75 is 0.250457, 10.5 times.
summary(ot_emp_level[ot_tot > 1, ]$ot_frac)
## among all, p25 is 0.009141  , p75 is 0.2294 , 25 times.
summary(ot_emp_level$ot_frac)

## inequality top 10%
setorder(ot_emp_level, "ot_tot", "num_emp1")
ot_emp_level[, position := (1:.N) / .N]
ot_emp_level[, cum_ot := cumsum(ot_tot) / sum(ot_tot)]
ot_emp_level[, is_90th := position >= 0.9 & shift(position) < 0.9]
stopifnot(nrow(ot_emp_level[is_90th == TRUE]) == 1)
observed_top10_ot_share <- 1 - ot_emp_level[is_90th == TRUE]$cum_ot[1]
log_message(sprintf("Observed overtime share worked by the top 10%% of officers: %.3f", observed_top10_ot_share))
print(ot_emp_level[is_90th == 1])

ggplot(ot_emp_level, aes(x = ot_tot)) +
  geom_histogram(fill = "black") + xlab("Number of Days Working Overtime") + ylab("Officer Count") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title = element_text(size = 30), axis.text = element_text(size = 15))
ggsave(file.path(CONFIG$figures_dir, "03_01_ot_dipersion_count.png"), width = 12, height = 8, units = "in")

ot_positive_quartiles <- quantile(ot_emp_level[ot_tot > 0]$ot_tot, c(0.25, 0.75), na.rm = TRUE)
ot_positive_iqr <- as.numeric(ot_positive_quartiles["75%"] - ot_positive_quartiles["25%"])
log_message(sprintf("IQR of overtime shifts worked among officers with any overtime: %.3f (P25 = %.3f, P75 = %.3f)",
                    ot_positive_iqr, as.numeric(ot_positive_quartiles["25%"]), as.numeric(ot_positive_quartiles["75%"])))

## among those who work 1, p25 is 12.00  , p75 is 134.00, 11.2 times.
summary(ot_emp_level[ot_tot > 1, ]$ot_tot)
## among all, p25 is 4  , p75 is 121.00 , 30.3 times.
summary(ot_emp_level$ot_tot)

#' ---------------------------------------------------------------------------
#' BUYERS AND SELLERS CLASSIFICATION
#' ---------------------------------------------------------------------------

log_message("Classifying buyers and sellers")
all_pairs[(dist_from_med > 0.4) & ot_work == 1, class_type := "Likely Buyers"]
all_pairs[(dist_from_med < 0.03) & ot_work == 0, class_type := "Likely Sellers"]
all_pairs[is.na(class_type), class_type := "Ambigious"]

## likely buyers are central, but not likely sellers
ggplot(all_pairs[class_type != "Ambigious"], aes(x = l_degree, fill = as.factor(class_type))) +
  geom_histogram(position = "identity", alpha = 0.75) + xlab("Connectedness") + ylab("Officer-Day Count") +
  theme_bw() + guides(fill = guide_legend(title = "Week Day")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title = element_text(size = 30), axis.text = element_text(size = 15),
        legend.title = element_text(size = 15), legend.text = element_text(size = 15))
ggsave(file.path(CONFIG$figures_dir, "03_01_buyers_sellers_connectedness.png"), width = 12, height = 8, units = "in")

#' ---------------------------------------------------------------------------
#' NETWORK VISUALIZATION (BUYERS/SELLERS)
#' ---------------------------------------------------------------------------

log_message("Loading contact matrix for network visualization")
contact_matrix <- load_contact_matrix()
all_pairs_net <- merge(all_pairs, contact_matrix, by = c("num_emp1", "analysis_workdate"), all.x = TRUE)
id_cols <- grep("^[0-9]+$", names(all_pairs_net), value = TRUE)

set.seed(1129)
start_date <- as.Date('2015-07-04')

snapshot <- all_pairs_net[analysis_workdate == start_date, .SD, .SDcols = c(id_cols, "num_emp1")]
cols_keep <- as.character(snapshot$num_emp1)
qgraph_mat <- as.matrix(snapshot[, .SD, .SDcols = cols_keep])
stopifnot(diag(qgraph_mat) == 0)

rownames(qgraph_mat) <- as.character(snapshot$num_emp1)
colnames(qgraph_mat) <- colnames(snapshot[, .SD, .SDcols = cols_keep])
stopifnot(ncol(qgraph_mat) == nrow(qgraph_mat))
qgraph_obj <- qgraph(qgraph_mat, DoNotPlot = TRUE)

cent_size <- centrality(qgraph_mat)$InDegree
igraph_mat <- graph_from_adjacency_matrix(qgraph_mat, mode = "undirected", weighted = TRUE)

## color based on buyer or seller
nodecolor <- c("green", "white", "red")
names(nodecolor) <- c("Likely Buyers", "Ambigious", "Likely Sellers")
nodecolor <- nodecolor[all_pairs_net[analysis_workdate == start_date]$class_type]
names(nodecolor) <- as.character(snapshot$num_emp1)

lay_try <- layout_with_graphopt(igraph_mat, mass = 40, charge = 0.02, spring.length = 1, spring = 1)
png(file.path(CONFIG$figures_dir, "03_01_zoomed_out_network.png"), width = 1000, height = 1000, units = "px")
plot(igraph_mat, layout = lay_try, vertex.size = 3, vertex.label = NA,
     vertex.color = nodecolor, edge.width = 0.2)
dev.off()

# zoom in on one cluster
zoom_in <- induced_subgraph(igraph_mat, with(components(igraph_mat), membership == which(components(igraph_mat)$csize == 18)), )

nodecolor <- c("green", "white", "red")
names(nodecolor) <- c("Likely Buyers", "Ambigious", "Likely Sellers")
nodecolor <- nodecolor[all_pairs_net[analysis_workdate == start_date]$class_type]
names(nodecolor) <- as.character(snapshot$num_emp1)

## color based on quantiles
edgecolor <- c("lightgrey", "black")
edgecolor <- edgecolor[1 + (E(zoom_in)$weight >= 7)]

png(file.path(CONFIG$figures_dir, "03_01_zoomed_in_network.png"), width = 500, height = 500, units = "px")
plot(zoom_in, layout = layout_in_circle(zoom_in), vertex.size = 15, vertex.label = NA, edge.width = 1, edge.color = edgecolor,
     vertex.color = nodecolor[names(V(zoom_in))])
dev.off()

#' ---------------------------------------------------------------------------
#' NETWORK VISUALIZATION (CENTRALITY)
#' ---------------------------------------------------------------------------

log_message("Plotting network with centrality coloring")
set.seed(1129)
start_date <- as.Date('2015-01-01')

snapshot <- all_pairs_net[analysis_workdate == start_date, .SD, .SDcols = c(id_cols, "num_emp1")]
cols_keep <- as.character(snapshot$num_emp1)
qgraph_mat <- as.matrix(snapshot[, .SD, .SDcols = cols_keep])
stopifnot(diag(qgraph_mat) == 0)

rownames(qgraph_mat) <- as.character(snapshot$num_emp1)
colnames(qgraph_mat) <- colnames(snapshot[, .SD, .SDcols = cols_keep])
stopifnot(ncol(qgraph_mat) == nrow(qgraph_mat))
qgraph_obj <- qgraph(qgraph_mat, DoNotPlot = TRUE)

cent_size <- centrality(qgraph_mat)$InDegree
igraph_mat <- graph_from_adjacency_matrix(qgraph_mat, mode = "undirected", weighted = TRUE)

## color based on quantiles
colfunc <- colorRampPalette(c("white", "red"))
nodecolor <- colfunc(uniqueN(quantile(cent_size, seq(from = 0, to = 1, by = 0.10))))
names(nodecolor) <- names(quantile(cent_size, seq(from = 0, to = 1, by = 0.10)))
nodecolor <- nodecolor[paste0(floor(ecdf(cent_size)(cent_size) / 0.1) * 10, "%")]
names(nodecolor) <- as.character(snapshot$num_emp1)

lay_try <- layout_with_graphopt(igraph_mat, mass = 15, charge = 0.02, spring.length = 1, spring = 1)
png(file.path(CONFIG$figures_dir, "03_01_zoomed_out_network_centrality.png"), width = 1000, height = 1000, units = "px")
plot(igraph_mat, layout = lay_try, vertex.size = 3, vertex.label = NA,
     vertex.color = nodecolor[as.character(snapshot$num_emp1)], edge.width = 0.2)
dev.off()

# zoom in on one cluster
zoom_in <- induced_subgraph(igraph_mat, with(components(igraph_mat), membership == which(components(igraph_mat)$csize == 13)), )

## color based on quantiles
edgecolor <- c("lightgrey", "black")
edgecolor <- edgecolor[1 + (E(zoom_in)$weight >= 5)]

png(file.path(CONFIG$figures_dir, "03_01_zoomed_in_network_centrality.png"), width = 500, height = 500, units = "px")
plot(zoom_in, layout = layout_in_circle(zoom_in), vertex.size = 15, vertex.label = NA, edge.width = 1, edge.color = edgecolor,
     vertex.color = nodecolor[names(V(zoom_in))])
dev.off()

zoom_in <- induced_subgraph(igraph_mat, with(components(igraph_mat), membership == which(components(igraph_mat)$csize == 18)), )

## color based on quantiles
edgecolor <- c("lightgrey", "black")
edgecolor <- edgecolor[1 + (E(zoom_in)$weight >= 3.5)]

png(file.path(CONFIG$figures_dir, "03_01_zoomed_in_network_centrality_18.png"), width = 500, height = 500, units = "px")
plot(zoom_in, layout = layout_in_circle(zoom_in), vertex.size = 15, vertex.label = NA, edge.width = 1, edge.color = edgecolor,
     vertex.color = nodecolor[names(V(zoom_in))])
dev.off()

#' ---------------------------------------------------------------------------
#' TIME-VARYING NETWORK FOR OFFICER 230
#' ---------------------------------------------------------------------------

log_message("Plotting time-varying network for officer 230")
v_sel <- 230
var_one <- induced_subgraph(igraph_mat, c(which(names(V(igraph_mat)) == paste0(v_sel)), neighbors(igraph_mat, which(names(V(igraph_mat)) == paste0(v_sel)))))
var_one <- delete.edges(var_one, E(var_one)[-incident(var_one, which(names(V(var_one)) == paste0(v_sel)))])

png(file.path(CONFIG$figures_dir, "03_01_timevarying_230_20150101.png"), width = 800, height = 800, units = "px")
plot(var_one, vertex.label.cex = 3, vertex.color = ifelse(names(V(var_one)) == paste0(v_sel), nodecolor[names(V(var_one))], "white"),
     layout = layout_as_star(var_one, center = which(names(V(var_one)) == paste0(v_sel))),
     vertex.size = 25, edge.width = 3.2 * (E(var_one)$weight - min(E(var_one)$weight)) + 0.1)
dev.off()

start_date <- as.Date('2015-07-01')

snapshot <- all_pairs_net[analysis_workdate == start_date, .SD, .SDcols = c(id_cols, "num_emp1")]
cols_keep <- as.character(snapshot$num_emp1)
qgraph_mat <- as.matrix(snapshot[, .SD, .SDcols = cols_keep])
stopifnot(diag(qgraph_mat) == 0)

rownames(qgraph_mat) <- as.character(snapshot$num_emp1)
colnames(qgraph_mat) <- colnames(snapshot[, .SD, .SDcols = cols_keep])
stopifnot(ncol(qgraph_mat) == nrow(qgraph_mat))
qgraph_obj <- qgraph(qgraph_mat, DoNotPlot = TRUE)

cent_size <- centrality(qgraph_mat)$InDegree
igraph_mat <- graph_from_adjacency_matrix(qgraph_mat, mode = "undirected", weighted = TRUE)

var_one <- induced_subgraph(igraph_mat, c(which(names(V(igraph_mat)) == paste0(v_sel)), neighbors(igraph_mat, which(names(V(igraph_mat)) == paste0(v_sel)))))
var_one <- delete.edges(var_one, E(var_one)[-incident(var_one, which(names(V(var_one)) == paste0(v_sel)))])

png(file.path(CONFIG$figures_dir, "03_01_timevarying_230_20150701.png"), width = 800, height = 800, units = "px")
plot(var_one, vertex.label.cex = 2, vertex.color = ifelse(names(V(var_one)) == paste0(v_sel), nodecolor[names(V(var_one))], "white"),
     layout = layout_as_star(var_one, center = which(names(V(var_one)) == paste0(v_sel))),
     vertex.size = 25, edge.width = 3.2 * (E(var_one)$weight - min(E(var_one)$weight)) + 0.1)
dev.off()

start_date <- as.Date('2016-01-01')

snapshot <- all_pairs_net[analysis_workdate == start_date, .SD, .SDcols = c(id_cols, "num_emp1")]
cols_keep <- as.character(snapshot$num_emp1)
qgraph_mat <- as.matrix(snapshot[, .SD, .SDcols = cols_keep])
stopifnot(diag(qgraph_mat) == 0)

rownames(qgraph_mat) <- as.character(snapshot$num_emp1)
colnames(qgraph_mat) <- colnames(snapshot[, .SD, .SDcols = cols_keep])
stopifnot(ncol(qgraph_mat) == nrow(qgraph_mat))
qgraph_obj <- qgraph(qgraph_mat, DoNotPlot = TRUE)

cent_size <- centrality(qgraph_mat)$InDegree
igraph_mat <- graph_from_adjacency_matrix(qgraph_mat, mode = "undirected", weighted = TRUE)

var_one <- induced_subgraph(igraph_mat, c(which(names(V(igraph_mat)) == paste0(v_sel)), neighbors(igraph_mat, which(names(V(igraph_mat)) == paste0(v_sel)))))
var_one <- delete.edges(var_one, E(var_one)[-incident(var_one, which(names(V(var_one)) == paste0(v_sel)))])

png(file.path(CONFIG$figures_dir, "03_01_timevarying_230_20160101.png"), width = 800, height = 800, units = "px")
plot(var_one, vertex.label.cex = 3, vertex.color = ifelse(names(V(var_one)) == paste0(v_sel), nodecolor[names(V(var_one))], "white"),
     layout = layout_as_star(var_one, center = which(names(V(var_one)) == paste0(v_sel))),
     vertex.size = 25, edge.width = 3.2 * (E(var_one)$weight - min(E(var_one)$weight)) + 0.1)
dev.off()

#' ---------------------------------------------------------------------------
#' POTENTIAL SUPPLIER DISPERSION
#' ---------------------------------------------------------------------------

log_message("Plotting potential supplier dispersion")
q_degree <- quantile(all_pairs$l_wheel_degree, c(0.25, 0.75), na.rm = TRUE)
iqr_degree <- as.numeric(q_degree["75%"] - q_degree["25%"])
raw_sd <- sd(all_pairs[!is.na(l_wheel_degree)]$l_wheel_degree)
supplier_fe_res <- feols(l_wheel_degree ~ 0 | num_emp1, all_pairs[!is.na(l_wheel_degree)])
supplier_resid_sd_ratio <- sd(resid(supplier_fe_res)) / raw_sd
supplier_fe_variation_share <- 1 - supplier_resid_sd_ratio^2
log_message(sprintf("Share of variation in potential supplier count captured by officer fixed effects: %.3f",
                    supplier_fe_variation_share))
log_message(sprintf("Potential supplier count IQR: %.3f (P25 = %.3f, P75 = %.3f)",
                    iqr_degree, as.numeric(q_degree["25%"]), as.numeric(q_degree["75%"])))
print(q_degree["75%"] / q_degree["25%"])

print(supplier_resid_sd_ratio)

ggplot(all_pairs, aes(x = l_wheel_degree)) +
  geom_histogram(fill = "black") + xlab("Potential Supplier Count") + ylab("Officer Count") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title = element_text(size = 30), axis.text = element_text(size = 15))

ggsave(file.path(CONFIG$figures_dir, "03_01_potential_supplier_hist.png"), width = 12, height = 8, units = "in")

all_pairs[, bar_degree := mean(l_wheel_degree), by = "num_emp1"]
ggplot(all_pairs, aes(x = l_wheel_degree - bar_degree)) +
  geom_histogram(fill = "black") + xlab("Potential Supplier Count Deviations from Individual Mean") + ylab("Officer Count") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title = element_text(size = 30), axis.text = element_text(size = 15))

ggsave(file.path(CONFIG$figures_dir, "03_01_potential_supplier_hist_idiosyncratic.png"), width = 12, height = 8, units = "in")

#' ---------------------------------------------------------------------------
#' SUPPLIER COUNT AND OVERTIME
#' ---------------------------------------------------------------------------

log_message("Computing supplier count and overtime relationship")
q25_degree <- as.numeric(quantile(all_pairs$l_wheel_degree, 0.25, na.rm = TRUE))
q75_degree <- as.numeric(quantile(all_pairs$l_wheel_degree, 0.75, na.rm = TRUE))
q10_degree <- as.numeric(quantile(all_pairs$l_wheel_degree, 0.1, na.rm = TRUE))
q90_degree <- as.numeric(quantile(all_pairs$l_wheel_degree, 0.9, na.rm = TRUE))

bottom_quartile_ot_prob <- mean(all_pairs[!is.na(l_wheel_degree) & l_wheel_degree <= q25_degree]$ot_work)
top_quartile_ot_prob <- mean(all_pairs[!is.na(l_wheel_degree) & l_wheel_degree >= q75_degree]$ot_work)
log_message(sprintf("Average overtime probability in bottom quartile of potential supplier count: %.3f", bottom_quartile_ot_prob))
log_message(sprintf("Average overtime probability in top quartile of potential supplier count: %.3f", top_quartile_ot_prob))

## 75 vs 25th percentile
print(top_quartile_ot_prob)
print(bottom_quartile_ot_prob)

## 10 vs 90th percentile
print(mean(all_pairs[!is.na(l_wheel_degree) & l_wheel_degree >= q90_degree]$ot_work))
print(mean(all_pairs[!is.na(l_wheel_degree) & l_wheel_degree <= q10_degree]$ot_work))

ggplot(all_pairs[!is.na(l_wheel_degree)], aes(x = l_wheel_degree, y = as.numeric(ot_work))) +
  stat_summary_bin(fun = match.fun(mean), bins = 40,
                   color = 'black', geom = 'point', size = 2) +
  stat_summary_bin(fun.data = 'mean_cl_boot', bins = 40,
                   color = 'black', geom = 'errorbar', position = position_dodge(1), width = .05) +
  ylab("Fraction Officer-Days with Overtime") + xlab("Potential Supplier Count") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title = element_text(size = 30), axis.text = element_text(size = 15))

ggsave(file.path(CONFIG$figures_dir, "03_01_potential_supplier_count.png"), width = 12, height = 8, units = "in")

#' ---------------------------------------------------------------------------
#' RESIDUALIZED RELEVANCE PLOT
#' ---------------------------------------------------------------------------

log_message("Computing residualized relevance plot")
work_res <- feols(ot_work ~ opp_dist + normal_work + ot_rate + seniority_rank | num_emp1 + analysis_workdate,
                  data = all_pairs[!is.na(l_wheel_degree), ], cluster = "num_emp1")
instrument_res <- feols(l_wheel_degree ~ opp_dist + normal_work + ot_rate + seniority_rank | num_emp1 + analysis_workdate,
                        data = all_pairs[!is.na(l_wheel_degree), ], cluster = "num_emp1")

all_pairs[!is.na(l_wheel_degree), resid_work := resid(work_res)]
all_pairs[!is.na(l_wheel_degree), resid_instrument := resid(instrument_res)]

## note that winsorizing is necessary to see the internal pattern.
ggplot(data = all_pairs[!is.na(l_wheel_degree)][resid_instrument <= quantile(resid_instrument, 0.99) & resid_instrument >= quantile(resid_instrument, 0.01)],
       aes(x = resid_instrument, y = as.numeric(resid_work))) +
  stat_summary_bin(fun = match.fun(mean), bins = 40,
                   color = 'black', geom = 'point', size = 2) +
  stat_summary_bin(fun.data = 'mean_cl_boot', bins = 40,
                   color = 'black', geom = 'errorbar', position = position_dodge(1), width = .05) +
  ylab("Residualized Overtime") + xlab("Residualized Potential Supplier Count") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title = element_text(size = 30), axis.text = element_text(size = 15))

ggsave(file.path(CONFIG$figures_dir, "03_01_relevance_resid.png"), width = 12, height = 8, units = "in")

log_message("03_01_facts.R completed successfully")
log_complete(success = TRUE)
