#' =============================================================================
#' FAMILY MEDICAL LEAVE EVENT STUDY - Callaway & Sant'Anna (2021) Group-Time ATT (Own Effects)
#' =============================================================================
#' Input:  data/02_01_estimation_sample.rds
#' Output: out/figures/03_06_own_fmla_cs.png
#' =============================================================================

source('config.R')
source('utils/logging.R')
log_init("03_06_own_fmla_cs.R")

#' ---------------------------------------------------------------------------
#' LOAD PACKAGES
#' ---------------------------------------------------------------------------

library('fixest')
library('did')
library('data.table')
library('ggplot2')

#' ---------------------------------------------------------------------------
#' LOAD DATA
#' ---------------------------------------------------------------------------

log_message("Loading estimation sample")
all_pairs <- readRDS(file.path(CONFIG$data_dir, "02_01_estimation_sample.rds"))

#' ---------------------------------------------------------------------------
#' IDENTIFY FAMILY LEAVE EVENTS
#' ---------------------------------------------------------------------------

log_message("Identifying family leave events for own-effect analysis")
all_pairs[is.na(family_leave), family_leave := 0]
all_pairs[family_leave == 1, first_time := min(analysis_workdate), by = "num_emp1"]
all_pairs[family_leave == 1, is_leave := first_time == analysis_workdate]
all_pairs[family_leave == 0, is_leave := 0]
all_pairs[, does_leave := max(is_leave), by = "num_emp1"]

#' ---------------------------------------------------------------------------
#' ASSIGN OWN TREATMENT
#' ---------------------------------------------------------------------------

# set treatment to be 1 at most.
all_pairs[, treat := 0]
all_pairs[is_leave == 1, treat := 1]
all_pairs[treat == 1, first_treat := analysis_workdate]
all_pairs[, first_treat := ifelse(max(treat) == 1, min(first_treat, na.rm = TRUE), Inf), by = "num_emp1"]
all_pairs[, rel_time := analysis_workdate - first_treat]

#' ---------------------------------------------------------------------------
#' ESTIMATE WITH CALLAWAY & SANT'ANNA (2021)
#' ---------------------------------------------------------------------------

log_message("Estimating Callaway & Sant'Anna group-time ATT")
est_data <- copy(all_pairs)

# Create numeric time period and group variable for did package
est_data[, time_period := as.numeric(analysis_workdate)]
est_data[, g := as.numeric(first_treat)]
est_data[is.infinite(g), g := 0]

## NOTE: att_gt with daily panel data can be computationally intensive.
## Consider collapsing to weekly periods if runtime is prohibitive.
out <- att_gt(yname = "wheel_degree",
              tname = "time_period",
              idname = "num_emp1",
              gname = "g",
              data = as.data.frame(est_data),
              control_group = "nevertreated",
              allow_unbalanced_panel = TRUE)

# Aggregate to event-study
es <- aggte(out, type = "dynamic", na.rm = TRUE)

#' ---------------------------------------------------------------------------
#' PLOT
#' ---------------------------------------------------------------------------

log_message("Saving event-study plot")
es_df <- data.frame(time = es$egt, att = es$att.egt,
                     lower = es$att.egt - 1.96 * es$se.egt,
                     upper = es$att.egt + 1.96 * es$se.egt)

ensure_directory(CONFIG$figures_dir)
png(file.path(CONFIG$figures_dir, "03_06_own_fmla_cs.png"), width = 900, height = 500)
print(
  ggplot(es_df) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_ribbon(aes(x = time, ymin = lower, ymax = upper), alpha = 0.2, fill = "steelblue") +
    geom_line(aes(x = time, y = att), color = "steelblue", linewidth = 0.8) +
    geom_point(aes(x = time, y = att), color = "steelblue", size = 1.5) +
    coord_cartesian(xlim = c(-29, 29)) +
    labs(x = "Time to Family Medical Leave (Days)", y = "Connectedness") +
    theme_minimal(base_size = 14)
)
dev.off()

#' ---------------------------------------------------------------------------
#' UNCONDITIONAL WITH TREATMENT STARTING DAY PRIOR
#' ---------------------------------------------------------------------------

log_message("Estimating unconditional TWFE with prior-day treatment")
all_pairs[, first_treat := NULL]
all_pairs[, treat := 0]
all_pairs[, treat := rel_time == -1]
all_pairs[treat == 1, first_treat := analysis_workdate]
all_pairs[, first_treat := ifelse(max(treat) == 1, min(first_treat, na.rm = TRUE), Inf), by = "num_emp1"]

twfe <- feols(wheel_degree ~ treat | num_emp1 + analysis_workdate, data = all_pairs)
summary(twfe)

log_message("03_06_own_fmla_cs.R completed successfully")
log_complete(success = TRUE)
