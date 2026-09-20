# Keep input size, complete cases and backend observations distinct. In
# particular a psychonetrics sample@nobs slot counts statistics, not people.
quicknet_analysis_sample <- function(data, raw, meta, model) {
  if (!is.null(meta$analysis_sample)) return(meta$analysis_sample)
  out <- list(input_rows = if (!is.null(data)) nrow(data) else NA_integer_)
  raw_model <- if (is.list(raw)) raw$model %||% raw else raw
  if (identical(meta$data_type, "meta")) {
    out$study_sample_sizes <- meta$nobs
    if (!is.null(data) && !is.null(meta$studyvar)) out$study_input_rows <- table(data[[meta$studyvar]])
    out$counts_source <- "Study sample sizes supplied to the meta-analysis; raw input rows are recorded separately."
    return(out)
  }
  if (!is.null(data)) {
    vars <- meta$vars %||% colnames(data)
    vars <- intersect(vars, colnames(data))
    out$complete_rows <- sum(stats::complete.cases(data[, vars, drop = FALSE]))
    if (anyNA(data[, vars, drop = FALSE])) {
      observed <- !is.na(as.matrix(data[, vars, drop = FALSE]))
      out$pairwise_observations <- crossprod(observed)
    }
  }
  if (inherits(raw_model, "psychonetrics")) {
    out$backend_nobs <- raw_model@sample@groups[, c("label", "nobs"), drop = FALSE]
    out$analyzed_observations <- sum(out$backend_nobs$nobs)
    out$counts_source <- "psychonetrics sample@groups$nobs; complete input rows and pairwise counts are descriptive."
  } else if (inherits(raw_model, "lavaan") && requireNamespace("lavaan", quietly = TRUE)) {
    counts <- as.numeric(lavaan::lavInspect(raw_model, "nobs"))
    out$backend_nobs <- data.frame(group = seq_along(counts), nobs = counts)
    out$analyzed_observations <- sum(counts)
    out$counts_source <- "lavaan lavInspect(nobs); complete input rows and pairwise counts are descriptive."
    if (identical(meta$data_type, "panel")) out$analyzed_subjects <- sum(counts)
  } else {
    out$counts_source <- "Input retained by the fit; pairwise counts can differ when observations are missing."
  }
  out
}
