#' =============================================================================
#' STAGGERED DID ATT SUMMARY TABLES
#' =============================================================================
#' Input:  data/03_03_termination_sunab_att.rds
#'         data/03_03_termination_twfe_att.rds
#'         data/03_04_new_hire_sunab_att.rds
#'         data/03_04_new_hire_twfe_att.rds
#'         data/03_05_fmla_sunab_att.rds
#'         data/03_05_fmla_twfe_att.rds
#'         data/03_06_own_fmla_sunab_att.rds
#'         data/03_06_own_fmla_twfe_att.rds
#'         data/03_07_bereave_sunab_att.rds
#'         data/03_07_bereave_twfe_att.rds
#'         data/03_08_own_bereave_sunab_att.rds
#'         data/03_08_own_bereave_twfe_att.rds
#' Output: out/tables/03_10_staggered_att_peer.tex
#'         out/tables/03_10_staggered_att_own.tex
#' =============================================================================

source('config.R')
source('utils/logging.R')
source('utils/sunab_utils.R')

library('data.table')
library('kableExtra')

log_init("03_10_sunab_summary.R")
log_message("Building staggered DiD ATT summary tables")

att_files <- data.table(
  specification = c(
    "Termination (Peer)", "Termination (Peer)",
    "New Hire (Peer)", "New Hire (Peer)",
    "FMLA (Peer)", "FMLA (Peer)",
    "Bereavement (Peer)", "Bereavement (Peer)",
    "FMLA (Own)", "FMLA (Own)",
    "Bereavement (Own)", "Bereavement (Own)"
  ),
  display_specification = c(
    "Termination", "Termination",
    "New Hire", "New Hire",
    "FMLA", "FMLA",
    "Bereavement", "Bereavement",
    "FMLA", "FMLA",
    "Bereavement", "Bereavement"
  ),
  effect_type = c(
    "peer", "peer",
    "peer", "peer",
    "peer", "peer",
    "peer", "peer",
    "own", "own",
    "own", "own"
  ),
  estimator = c(
    "TWFE", "Sun & Abraham",
    "TWFE", "Sun & Abraham",
    "TWFE", "Sun & Abraham",
    "TWFE", "Sun & Abraham",
    "TWFE", "Sun & Abraham",
    "TWFE", "Sun & Abraham"
  ),
  path = c(
    file.path(CONFIG$data_dir, "03_03_termination_twfe_att.rds"),
    file.path(CONFIG$data_dir, "03_03_termination_sunab_att.rds"),
    file.path(CONFIG$data_dir, "03_04_new_hire_twfe_att.rds"),
    file.path(CONFIG$data_dir, "03_04_new_hire_sunab_att.rds"),
    file.path(CONFIG$data_dir, "03_05_fmla_twfe_att.rds"),
    file.path(CONFIG$data_dir, "03_05_fmla_sunab_att.rds"),
    file.path(CONFIG$data_dir, "03_07_bereave_twfe_att.rds"),
    file.path(CONFIG$data_dir, "03_07_bereave_sunab_att.rds"),
    file.path(CONFIG$data_dir, "03_06_own_fmla_twfe_att.rds"),
    file.path(CONFIG$data_dir, "03_06_own_fmla_sunab_att.rds"),
    file.path(CONFIG$data_dir, "03_08_own_bereave_twfe_att.rds"),
    file.path(CONFIG$data_dir, "03_08_own_bereave_sunab_att.rds")
  )
)

assert_required_files(att_files$path)

results <- rbindlist(lapply(seq_len(nrow(att_files)), function(i) {
  dt <- as.data.table(readRDS(att_files$path[i]))
  dt[, `:=`(
    specification = att_files$specification[i],
    display_specification = att_files$display_specification[i],
    effect_type = att_files$effect_type[i],
    estimator = att_files$estimator[i]
  )]
  dt
}), fill = TRUE)

format_estimate <- function(x) {
  ifelse(is.na(x), "", sprintf("%.3f", x))
}

render_att_table <- function(effect_value, output_file, specification_order) {
  table_results <- copy(results[effect_type == effect_value])
  table_results[, specification := factor(specification, levels = specification_order)]
  table_results[, estimator := factor(estimator, levels = c("TWFE", "Sun & Abraham"))]
  setorder(table_results, specification, estimator)

  table_dt <- table_results[, .(
    Specification = ifelse(seq_len(.N) == 1L, display_specification, ""),
    Estimator = gsub("&", "\\\\&", as.character(estimator), fixed = TRUE),
    ATT = format_estimate(estimate),
    `Std. Error` = format_estimate(std_error),
    `p-value` = format_p_value(p_value),
    Observations = format(nobs, big.mark = ",")
  ), by = specification][, specification := NULL]

  kable(
    table_dt,
    "latex",
    align = "llcccc",
    booktabs = TRUE,
    linesep = c(""),
    escape = FALSE,
    caption = NA,
    label = NA
  ) %>%
    cat(., file = output_file)

  invisible(output_file)
}

ensure_directory(CONFIG$tables_dir)
render_att_table(
  effect_value = "peer",
  output_file = file.path(CONFIG$tables_dir, "03_10_staggered_att_peer.tex"),
  specification_order = c(
    "Termination (Peer)",
    "New Hire (Peer)",
    "FMLA (Peer)",
    "Bereavement (Peer)"
  )
)
render_att_table(
  effect_value = "own",
  output_file = file.path(CONFIG$tables_dir, "03_10_staggered_att_own.tex"),
  specification_order = c(
    "FMLA (Own)",
    "Bereavement (Own)"
  )
)

log_message("Saved staggered DiD ATT summary tables")
log_complete(success = TRUE)

