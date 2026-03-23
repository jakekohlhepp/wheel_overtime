#' =============================================================================
#' DISPLAY ESTIMATION RESULTS
#' =============================================================================
#' Input:  data/04_01_estimate.Rdata, data/04_01_estimate_probit.Rdata
#'         data/02_01_estimation_sample.rds
#' Output: out/tables/02_01_main_estimates.tex
#'         out/figures/02_01_officer_fe.png
#'         out/figures/02_01_date_fe.png
#' =============================================================================

library('ggplot2')
library('data.table')
library('alpaca')
library('kableExtra')
library('lubridate')

source('config.R')
source('utils/logging.R')

log_init("05_01_display.R")
log_message("Starting display of estimation results")

#' ---------------------------------------------------------------------------
#' LOAD DATA AND ESTIMATES
#' ---------------------------------------------------------------------------

load(file.path(CONFIG$data_dir, "04_01_estimate.Rdata"))
load(file.path(CONFIG$data_dir, "04_01_estimate_probit.Rdata"))

all_pairs <- readRDS(file.path(CONFIG$data_dir, "02_01_estimation_sample.rds"))

## compute average ot hours.
avg_othours <- mean(all_pairs[ot_work == 1]$varot_hours)

#' ---------------------------------------------------------------------------
#' MAIN ESTIMATES TABLE
#' ---------------------------------------------------------------------------

log_message("Computing APEs and building estimates table")

### displaying the estimates - both the valuation distribution and the coefficients.
## get apes
ape_logit <- summary(getAPEs(mod_mod), type = "clustered", cluster = ~num_emp1)$cm[, 1:2]
ape_probit <- summary(getAPEs(mod_probit_mod), type = "clustered", cluster = ~num_emp1)$cm[, 1:2]
output <- data.table(summary(mod_mod, type = "clustered", cluster = ~num_emp1)$cm[, 1:2])
output <- cbind(output, ape_logit, data.table(summary(mod_probit_mod, type = "clustered", cluster = ~num_emp1)$cm[, 1:2], ape_probit))
output <- rbind(output[, c(1, 3, 5, 7)], output[, c(2, 4, 6, 8)], use.names = FALSE)
output <- cbind(rep(rownames(ape_logit), 2), c(rep("coef", 5), rep("se", 5)), output)

colnames(output) <- c("Variable", "type", "Estimate (Logit)", "APE (Logit)", "Estimate (Probit)", "APE (Probit)")
output[Variable == "suppliers_interacted", Variable := "Supplier Count x Distance from Wheel"]
output[Variable == "opp_dist", Variable := "Distance from Wheel"]
output[Variable == "ot_rate", Variable := "Overtime Wage"]
output[Variable == "seniority_rank", Variable := "Seniority Rank"]
output[Variable == "normal_work", Variable := "Normal Work"]
setorder(output, "Variable", "type")
output[, `Estimate (Logit)` := paste0(as.character(sprintf("%.3f", round(`Estimate (Logit)`, 4))))]
output[type == "se", `Estimate (Logit)` := paste0("(", `Estimate (Logit)`, ")")]
output[, `APE (Logit)` := paste0(as.character(sprintf("%.3f", round(`APE (Logit)`, 4))))]
output[type == "se", `APE (Logit)` := paste0("(", `APE (Logit)`, ")")]
output[, `Estimate (Probit)` := paste0(as.character(sprintf("%.3f", round(`Estimate (Probit)`, 4))))]
output[type == "se", `Estimate (Probit)` := paste0("(", `Estimate (Probit)`, ")")]

output[, `APE (Probit)` := paste0(as.character(sprintf("%.3f", round(`APE (Probit)`, 4))))]
output[type == "se", `APE (Probit)` := paste0("(", `APE (Probit)`, ")")]
output[type == "se", Variable := ""]

ensure_directory(CONFIG$tables_dir)
kable(output[, -"type"], "latex", align = "c", booktabs = TRUE, linesep = c(""), escape = F, caption = NA, label = NA) %>%
  cat(., file = file.path(CONFIG$tables_dir, "02_01_main_estimates.tex"))

log_message("Saved main estimates table")

#' ---------------------------------------------------------------------------
#' FIXED EFFECT DISTRIBUTIONS
#' ---------------------------------------------------------------------------

log_message("Computing and plotting fixed effect distributions")

### displaying the graph of fes - convert to dollars.
officer_fe <- data.table(officer_fe = getFEs(mod_mod)$num_emp1 / coef(mod_mod)['ot_rate'] * avg_othours, num_emp1 = as.numeric(names(getFEs(mod_mod)$num_emp1)))
date_fe <- data.table(date_fe = getFEs(mod_mod)$analysis_workdate / coef(mod_mod)['ot_rate'] * avg_othours, analysis_workdate = as.Date(names(getFEs(mod_mod)$analysis_workdate)))

print(quantile(officer_fe$officer_fe)['75%'] - quantile(officer_fe$officer_fe)['25%'])
print(quantile(date_fe$date_fe)['75%'] - quantile(date_fe$date_fe)['25%'])

ensure_directory(CONFIG$figures_dir)

ggplot(officer_fe, aes(x = officer_fe)) +
  geom_histogram(fill = "black") + xlab("Earnings Equivalent Officer Fixed Effect ($)") + ylab("Officer Count") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_text(size = 30), axis.text = element_text(size = 30))

ggsave(file.path(CONFIG$figures_dir, "02_01_officer_fe.png"), width = 10, height = 10, units = "in")
log_message("Saved officer FE histogram")

ggplot(date_fe, aes(x = date_fe)) +
  geom_histogram(fill = "black") + xlab("Earnings Equivalent Date Fixed Effect ($)") + ylab("Date Count") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_text(size = 30), axis.text = element_text(size = 30))

ggsave(file.path(CONFIG$figures_dir, "02_01_date_fe.png"), width = 10, height = 10, units = "in")
log_message("Saved date FE histogram")

#' ---------------------------------------------------------------------------
#' ACCESS COST SUMMARY
#' ---------------------------------------------------------------------------

log_message("Computing access cost summary")

## access cost sum
all_pairs[, access_cost := ot_work * (coef(mod_mod)['opp_dist'] * opp_dist + coef(mod_mod)['suppliers_interacted'] * suppliers_interacted)]
print(sum(all_pairs$access_cost) / coef(mod_mod)['ot_rate'] * avg_othours)

log_complete(success = TRUE)
