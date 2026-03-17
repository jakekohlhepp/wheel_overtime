#' =============================================================================
#' CARTEL AND AGE ANALYSIS
#' =============================================================================
#' Input:  data/00_01_estimation_sample.rds
#'         data/02_00_estimate.Rdata
#' Output: out/tables/02_03_cartel.tex
#'         out/figures/02_03_age_valuation.png
#' =============================================================================

## cartel are those observed working more than 400 overtime shifts.

library('data.table')
library('alpaca')
library('ggplot2')
library('kableExtra')

source('config.R')
source('utils/logging.R')

log_init("02_03_cartel_age.R")
log_message("Starting cartel and age analysis")

#' ---------------------------------------------------------------------------
#' LOAD DATA
#' ---------------------------------------------------------------------------

all_pairs <- readRDS(file.path(CONFIG$data_dir, "00_01_estimation_sample.rds"))
load(file.path(CONFIG$data_dir, "02_00_estimate.Rdata"))
avg_othours <- mean(all_pairs[ot_work == 1]$varot_hours)

#' ---------------------------------------------------------------------------
#' PREPARE OFFICER-LEVEL VARIABLES
#' ---------------------------------------------------------------------------

log_message("Computing officer-level variables")

## has family is based on fml at any point.
all_pairs[, has_family := max(family_leave, na.rm = TRUE), by = "num_emp1"]

# make age on 01012015
all_pairs[, age_on_20150101 := an_age - (analysis_workdate - as.Date('2015-01-01')) / 365.25, by = "num_emp1"]

stopifnot(all_pairs[, .(check = uniqueN(round(age_on_20150101, digits = 8))), by = "num_emp1"]$check == 1)
all_pairs[, age_on_20150101 := as.numeric(max(age_on_20150101)), by = "num_emp1"]
officer_fe <- data.table(valuation = getFEs(mod_mod)$num_emp1, num_emp1 = as.numeric(names(getFEs(mod_mod)$num_emp1)))
all_pairs <- merge(all_pairs, officer_fe, by = "num_emp1", all.x = TRUE)

#' ---------------------------------------------------------------------------
#' OFFICER SUMMARY STATISTICS
#' ---------------------------------------------------------------------------

log_message("Building officer summary statistics")

### count ot, get valuation, get average network position
miss_cor <- function(x, y) {
  if (sd(x) > 0 & sd(y) > 0) {
    return(cor(x, y))
  } else {
    return(as.double(NA))
  }
}

by_emp <- all_pairs[, .(observed_ot_count = sum(ot_work), valuation = unique(valuation),
                        avg_degree = mean(l_degree),
                        avg_suppliers = mean(l_wheel_degree), barrier_avg = mean(coef(mod_mod)['opp_dist'] * opp_dist + coef(mod_mod)['suppliers_interacted'] * suppliers_interacted),
                        cor_both = miss_cor(dist_from_med, l_wheel_degree),
                        avg_seniority = mean(seniority_rank)), by = c("num_emp1", "has_family")]

## convert fe to wage equivalents and display format.

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

### correlation between avg barrier and valuation
print(cor(by_emp[, c("valuation", "barrier_avg")], use = "complete"))
ggplot(by_emp, aes(x = valuation / coef(mod_mod)['ot_rate'] * avg_othours, y = barrier_avg / coef(mod_mod)['ot_rate'] * avg_othours, color = log(observed_ot_count))) + geom_point(size = 3)

#' ---------------------------------------------------------------------------
#' CARTEL TABLE
#' ---------------------------------------------------------------------------

log_message("Building cartel table")

## cartel valuations
cartel <- copy(by_emp)
cartel[, valuation := paste0("\\$", round(valuation / (coef(mod_mod)["ot_rate"]) * avg_othours, 2))]
cartel[, avg_degree := paste0(round(avg_degree, 2))]
cartel[, avg_suppliers := paste0(round(avg_suppliers, 2))]
setorder(cartel, "rank_ot", "num_emp1")
cartel <- cartel[observed_ot_count > 300, c("num_emp1", "observed_ot_count", "rank_ot", "avg_degree", "rank_degree", "avg_suppliers", "rank_suppliers", "valuation", "rank_valuation")]
colnames(cartel) <- c("Officer ID", "Value", "Rank", "Value", "Rank", "Value", "Rank", "Value", "Rank")

ensure_directory(CONFIG$tables_dir)
kable(cartel, "latex", align = "c", booktabs = TRUE, linesep = c(""), escape = F, caption = NA, label = NA) %>%
  add_header_above(c(" " = 1, "OT Shifts" = 2, "Avg. Connectedness" = 2, "Avg. Suppliers" = 2, "Fixed Effect" = 2)) %>%
  cat(., file = file.path(CONFIG$tables_dir, "02_03_cartel.tex"))
log_message("Saved cartel table")

#' ---------------------------------------------------------------------------
#' AGE-VALUATION ANALYSIS
#' ---------------------------------------------------------------------------

log_message("Building age-valuation plot")

## age valuations
by_emp <- unique(all_pairs[, c("num_emp1", "age_on_20150101", "valuation", "has_family")])
by_emp[, valuation := valuation / coef(mod_mod)["ot_rate"] * avg_othours]
by_emp[, num_age := as.numeric(age_on_20150101)]
cor(by_emp[!is.na(valuation), c("num_age", "valuation")])
print(lm(valuation ~ num_age, data = by_emp[!is.na(valuation), c("num_age", "valuation")]))

ensure_directory(CONFIG$figures_dir)
ggplot(data = by_emp, aes(x = age_on_20150101, y = valuation)) + geom_point(size = 2) + geom_smooth(method = 'lm', se = FALSE) + theme_bw() + xlab("Age on January 1, 2015 (Years)") + ylab("Officer Fixed Effect ($)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_text(size = 30), axis.text = element_text(size = 20))
ggsave(file.path(CONFIG$figures_dir, "02_03_age_valuation.png"), width = 12, height = 8, units = "in")
log_message("Saved age-valuation plot")

log_complete(success = TRUE)
