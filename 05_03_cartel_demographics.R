#' =============================================================================
#' CARTEL AND DEMOGRAPHIC VALUATION ANALYSIS
#' =============================================================================
#' Input:  data/02_01_estimation_sample.rds
#'         data/04_01_estimate.Rdata
#' Output: out/tables/05_03_cartel.tex
#'         out/figures/05_03_age_valuation.png
#'         out/figures/05_03_family_leave_valuation.png
#' =============================================================================

## Cartel officers are those observed working more than 300 overtime shifts.

library("data.table")
library("alpaca")
library("ggplot2")
library("kableExtra")

source("config.R")
source("utils/logging.R")

log_init("05_03_cartel_demographics.R")
log_message("Starting cartel and demographic valuation analysis")

#' ---------------------------------------------------------------------------
#' LOAD DATA
#' ---------------------------------------------------------------------------

all_pairs <- readRDS(file.path(CONFIG$data_dir, "02_01_estimation_sample.rds"))
load(file.path(CONFIG$data_dir, "04_01_estimate.Rdata"))
avg_othours <- mean(all_pairs[ot_work == 1]$varot_hours)

#' ---------------------------------------------------------------------------
#' PREPARE OFFICER-LEVEL VARIABLES
#' ---------------------------------------------------------------------------

log_message("Computing officer-level valuation inputs")

## Family leave status is defined as ever being observed on family leave.
all_pairs[, has_family_leave := as.integer(any(family_leave == 1L)), by = "num_emp1"]

## Anchor age to a common calendar date so valuations can be compared across officers.
all_pairs[, age_on_20150101 := an_age - (analysis_workdate - as.Date("2015-01-01")) / 365.25,
          by = "num_emp1"]
stopifnot(
  all_pairs[, .(check = uniqueN(round(age_on_20150101, digits = 8))), by = "num_emp1"]$check == 1
)
all_pairs[, age_on_20150101 := as.numeric(max(age_on_20150101)), by = "num_emp1"]

officer_fe <- data.table(
  valuation = getFEs(mod_mod)$num_emp1,
  num_emp1 = as.numeric(names(getFEs(mod_mod)$num_emp1))
)
all_pairs <- merge(all_pairs, officer_fe, by = "num_emp1", all.x = TRUE)

valuation_by_emp <- unique(
  all_pairs[, .(num_emp1, age_on_20150101, has_family_leave, valuation)]
)
valuation_by_emp[, age_on_20150101 := as.numeric(age_on_20150101)]
valuation_by_emp[, valuation_dollars := valuation / coef(mod_mod)["ot_rate"] * avg_othours]

#' ---------------------------------------------------------------------------
#' OFFICER SUMMARY STATISTICS
#' ---------------------------------------------------------------------------

log_message("Building officer summary statistics")

miss_cor <- function(x, y) {
  if (sd(x) > 0 & sd(y) > 0) {
    return(cor(x, y))
  }
  as.double(NA)
}

by_emp <- all_pairs[, .(
  observed_ot_count = sum(ot_work),
  valuation = unique(valuation),
  avg_degree = mean(l_degree),
  avg_suppliers = mean(l_wheel_degree),
  barrier_avg = mean(
    coef(mod_mod)["opp_dist"] * opp_dist +
      coef(mod_mod)["suppliers_interacted"] * suppliers_interacted
  ),
  cor_both = miss_cor(dist_from_med, l_wheel_degree),
  avg_seniority = mean(seniority_rank)
), by = c("num_emp1", "has_family_leave")]

by_emp[, rank_ot := frank(observed_ot_count, ties.method = "dense")]
by_emp[, rank_degree := frank(-avg_degree, ties.method = "dense")]
by_emp[, rank_valuation := frank(-valuation, ties.method = "dense")]
by_emp[, rank_suppliers := frank(-avg_suppliers, ties.method = "dense")]
by_emp[, rank_barriers := frank(-barrier_avg, ties.method = "dense")]
by_emp[, rank_cor := frank(-cor_both, ties.method = "dense")]

#' ---------------------------------------------------------------------------
#' BARRIER-VALUATION CORRELATION
#' ---------------------------------------------------------------------------

log_message("Computing barrier-valuation correlation")
print(cor(by_emp[, c("valuation", "barrier_avg")], use = "complete"))
ggplot(
  by_emp,
  aes(
    x = valuation / coef(mod_mod)["ot_rate"] * avg_othours,
    y = barrier_avg / coef(mod_mod)["ot_rate"] * avg_othours,
    color = log(observed_ot_count)
  )
) +
  geom_point(size = 3)

#' ---------------------------------------------------------------------------
#' CARTEL TABLE
#' ---------------------------------------------------------------------------

log_message("Building cartel table")

cartel <- copy(by_emp)
cartel[, valuation := paste0("\\$", round(valuation / coef(mod_mod)["ot_rate"] * avg_othours, 2))]
cartel[, avg_degree := paste0(round(avg_degree, 2))]
cartel[, avg_suppliers := paste0(round(avg_suppliers, 2))]
setorder(cartel, "rank_ot", "num_emp1")
cartel <- cartel[
  observed_ot_count > 300,
  c("num_emp1", "observed_ot_count", "rank_ot", "avg_degree", "rank_degree",
    "avg_suppliers", "rank_suppliers", "valuation", "rank_valuation")
]
colnames(cartel) <- c(
  "Officer ID", "Value", "Rank", "Value", "Rank",
  "Value", "Rank", "Value", "Rank"
)

ensure_directory(CONFIG$tables_dir)
kable(
  cartel, "latex", align = "c", booktabs = TRUE, linesep = c(""),
  escape = FALSE, caption = NA, label = NA
) %>%
  add_header_above(c(
    " " = 1, "OT Shifts" = 2, "Avg. Connectedness" = 2,
    "Avg. Suppliers" = 2, "Fixed Effect" = 2
  )) %>%
  cat(., file = file.path(CONFIG$tables_dir, "05_03_cartel.tex"))
log_message("Saved cartel table")

#' ---------------------------------------------------------------------------
#' AGE-VALUATION ANALYSIS
#' ---------------------------------------------------------------------------

log_message("Building age-valuation plot")

age_plot_data <- valuation_by_emp[!is.na(valuation_dollars)]
age_plot_data[, num_age := as.numeric(age_on_20150101)]
print(cor(age_plot_data[, c("num_age", "valuation_dollars")]))
print(lm(valuation_dollars ~ num_age, data = age_plot_data[, c("num_age", "valuation_dollars")]))

ensure_directory(CONFIG$figures_dir)
age_plot <- ggplot(age_plot_data, aes(x = num_age, y = valuation_dollars)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  xlab("Age on January 1, 2015 (Years)") +
  ylab("Officer Fixed Effect ($)") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 30),
    axis.text = element_text(size = 20)
  )
ggsave(
  file.path(CONFIG$figures_dir, "05_03_age_valuation.png"),
  plot = age_plot,
  width = 12,
  height = 8,
  units = "in"
)
log_message("Saved age-valuation plot")

#' ---------------------------------------------------------------------------
#' FAMILY-LEAVE VALUATION ANALYSIS
#' ---------------------------------------------------------------------------

log_message("Building family-leave valuation distribution plot")

family_plot_data <- valuation_by_emp[!is.na(valuation_dollars)]
family_plot_data[, family_leave_status := fifelse(
  has_family_leave == 1L,
  "Ever observed on family leave",
  "Never observed on family leave"
)]

family_leave_means <- family_plot_data[, .(
  mean_valuation = mean(valuation_dollars),
  officer_count = .N
), by = has_family_leave]

format_mean <- function(x) {
  if (length(x) == 0 || is.na(x)) {
    return("NA")
  }
  sprintf("%.2f", x)
}

mean_with_family <- family_leave_means[has_family_leave == 1L, mean_valuation]
mean_without_family <- family_leave_means[has_family_leave == 0L, mean_valuation]
count_with_family <- family_leave_means[has_family_leave == 1L, officer_count]
count_without_family <- family_leave_means[has_family_leave == 0L, officer_count]

if (length(count_with_family) == 0) count_with_family <- 0L
if (length(count_without_family) == 0) count_without_family <- 0L

log_message(sprintf(
  "Officer count ever observed on family leave: %d",
  count_with_family
))
log_message(sprintf(
  "Officer count never observed on family leave: %d",
  count_without_family
))
log_message(sprintf(
  "Mean valuation among officers ever observed on family leave: %s",
  format_mean(mean_with_family)
))
log_message(sprintf(
  "Mean valuation among officers never observed on family leave: %s",
  format_mean(mean_without_family)
))

family_plot <- ggplot(
  family_plot_data,
  aes(
    x = valuation_dollars,
    color = family_leave_status,
    fill = family_leave_status
  )
) +
  geom_density(alpha = 0.25, linewidth = 1.1, adjust = 1.1) +
  scale_color_manual(values = c("#1b9e77", "#d95f02")) +
  scale_fill_manual(values = c("#1b9e77", "#d95f02")) +
  theme_bw() +
  xlab("Officer Fixed Effect ($)") +
  ylab("Density") +
  labs(color = NULL, fill = NULL) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 24),
    axis.text = element_text(size = 18),
    legend.text = element_text(size = 16)
  )
ggsave(
  file.path(CONFIG$figures_dir, "05_03_family_leave_valuation.png"),
  plot = family_plot,
  width = 12,
  height = 8,
  units = "in"
)
log_message("Saved family-leave valuation distribution plot")

log_complete(success = TRUE)
