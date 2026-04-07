#' =============================================================================
#' STAGGERED DID UTILITIES
#' =============================================================================
#' Shared helpers for extracting compact ATT summaries from fixest models and
#' producing consistently formatted event-study plots.
#' =============================================================================

extract_fixest_att <- function(model, term, summary_args = list()) {
  model_summary <- do.call(summary, c(list(object = model), summary_args))
  att_matrix <- unclass(model_summary)$coeftable

  if (is.null(att_matrix) || !term %in% rownames(att_matrix)) {
    stop("Could not locate coefficient '", term, "' in the model summary.")
  }

  p_value_col <- grep("^Pr\\(", colnames(att_matrix), value = TRUE)
  if (length(p_value_col) != 1) {
    stop("Could not identify the p-value column in the model summary.")
  }

  att_row <- att_matrix[term, , drop = FALSE]
  nobs_value <- unclass(model_summary)$nobs
  if (is.null(nobs_value)) {
    nobs_value <- stats::nobs(model)
  }

  data.table::data.table(
    term = term,
    estimate = unname(att_row[1, "Estimate"]),
    std_error = unname(att_row[1, "Std. Error"]),
    t_value = unname(att_row[1, "t value"]),
    p_value = unname(att_row[1, p_value_col]),
    nobs = nobs_value,
    vcov_type = attr(att_matrix, "type") %||% NA_character_
  )
}

save_att_result <- function(att_dt, output_path, specification, effect_type, estimator) {
  att_dt[, `:=`(
    specification = specification,
    effect_type = effect_type,
    estimator = estimator
  )]

  ensure_directory(dirname(output_path))
  saveRDS(att_dt, output_path)

  invisible(att_dt)
}

extract_sunab_att <- function(model) {
  extract_fixest_att(model, term = "ATT", summary_args = list(agg = "att"))
}

save_sunab_att <- function(model, output_path, specification, effect_type) {
  save_att_result(
    extract_sunab_att(model),
    output_path = output_path,
    specification = specification,
    effect_type = effect_type,
    estimator = "Sun & Abraham"
  )
}

extract_twfe_att <- function(model, term = "treat") {
  extract_fixest_att(model, term = term)
}

save_twfe_att <- function(model, output_path, specification, effect_type, term = "treat") {
  save_att_result(
    extract_twfe_att(model, term = term),
    output_path = output_path,
    specification = specification,
    effect_type = effect_type,
    estimator = "TWFE"
  )
}

plot_sunab_event_study <- function(model, output_path, xlab, ylab,
                                   drop_pattern = "([5-9]\\d{0}|[1-9]\\d{1,})",
                                   width = 1100, height = 700, pointsize = 16,
                                   cex_axis = 1.3, cex_lab = 1.45) {
  ensure_directory(dirname(output_path))

  png(output_path, width = width, height = height, pointsize = pointsize)
  old_par <- par(no.readonly = TRUE)
  on.exit({
    par(old_par)
    dev.off()
  }, add = TRUE)

  par(mar = c(5.4, 5.8, 1.2, 1.2))
  iplot(
    model,
    main = "",
    xlab = xlab,
    ylab = ylab,
    drop = drop_pattern,
    lab.fit = "simple",
    cex.axis = cex_axis,
    cex.lab = cex_lab
  )

  invisible(output_path)
}

format_p_value <- function(p_value) {
  ifelse(is.na(p_value), "", ifelse(p_value < 0.001, "<0.001", sprintf("%.3f", p_value)))
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
