local({
  if (!requireNamespace("renv", quietly = TRUE)) {
    message(
      "renv is not installed locally; skipping automatic project activation. ",
      "Run install.packages('renv') and renv::restore() when downloads are available."
    )
    return(invisible(NULL))
  }

  tryCatch(
    renv::load(project = getwd()),
    error = function(e) {
      message(
        "renv activation failed; continuing without the project library. ",
        "Run renv::restore() after fixing package download access.\n",
        "Original error: ", conditionMessage(e)
      )
    }
  )
})