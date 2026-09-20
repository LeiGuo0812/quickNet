# Extract documented backend status without replacing its estimator or inventing
# a numerical convergence tolerance. Missing status remains explicitly unknown.
quicknet_backend_diagnostics <- function(raw) {
  if (is.list(raw) && !inherits(raw, c("lavaan", "psychonetrics")) &&
      !is.null(raw$model) && !is.character(raw$model)) raw_model <- raw$model else raw_model <- raw
  row <- function(component, converged = NA, admissible = NA, finite = NA,
                  code = NA_character_, message = "Backend convergence status was not recorded.") {
    failed <- identical(converged, FALSE) || identical(admissible, FALSE) || identical(finite, FALSE)
    data.frame(component = component, converged = converged, admissible = admissible,
      finite_parameters = finite, code = as.character(code), message = message,
      status = if (failed) "failed" else if (!is.na(converged)) "ok" else "unknown",
      stringsAsFactors = FALSE)
  }
  if (inherits(raw_model, "psychonetrics")) {
    opt <- raw_model@optim
    code <- opt$convergence
    converged <- if (length(code) == 1L && is.finite(code)) code == 0 else NA
    if (identical(raw_model@computed, FALSE)) converged <- FALSE
    finite <- all(is.finite(raw_model@parameters$est))
    if (length(raw_model@objective)) finite <- finite && all(is.finite(raw_model@objective))
    out <- row("psychonetrics", converged, finite = finite,
      code = if (length(code)) code else NA_character_,
      message = paste(c(opt$message %||% "Optimizer status was not recorded.",
                        attr(raw_model, "quicknet_backend_warnings")), collapse = " "))
    return(out)
  }
  if (inherits(raw_model, "lavaan") && requireNamespace("lavaan", quietly = TRUE)) {
    converged <- tryCatch(isTRUE(lavaan::lavInspect(raw_model, "converged")), error = function(e) NA)
    admissible <- if (isTRUE(converged)) suppressWarnings(tryCatch(
      lavaan::lavInspect(raw_model, "post.check"), error = function(e) NA)) else NA
    finite <- tryCatch(all(is.finite(lavaan::coef(raw_model))), error = function(e) NA)
    return(row("lavaan", converged, admissible, finite,
      message = if (isFALSE(converged)) "lavaan reported non-convergence."
        else if (isFALSE(admissible)) "lavaan post.check reported an inadmissible solution."
        else if (isTRUE(converged)) "lavaan convergence and post-estimation checks inspected."
        else "lavaan convergence status is unavailable."))
  }
  if (is.list(raw) && is.list(raw$glmnet$fits) && length(raw$glmnet$fits)) {
    out <- lapply(seq_along(raw$glmnet$fits), function(i) {
      f <- raw$glmnet$fits[[i]]
      underlying <- f$glmnet.fit %||% f
      code <- underlying$jerr
      result <- row(paste0("glmnet:", names(raw$glmnet$fits)[i] %||% i),
        converged = if (length(code) == 1L && is.finite(code) && code >= 0) code == 0 else NA,
        finite = length(underlying$a0) > 0L && all(is.finite(underlying$a0)) && all(is.finite(underlying$beta)),
        code = if (length(code)) code else NA_character_,
        message = if (length(code) == 1L && is.finite(code) && code != 0)
          if (code < 0) "glmnet returned a partial regularization path (nonfatal jerr < 0); available larger-lambda solutions are retained."
          else "glmnet reported a fatal regularization-path error (jerr > 0)."
          else "glmnet path status inspected.")
      if (length(code) == 1L && is.finite(code) && code < 0 && result$status != "failed") result$status <- "partial"
      result
    })
    return(do.call(rbind, out))
  }
  row("backend")
}

quicknet_capture_backend_warnings <- function(expr) {
  messages <- character()
  value <- withCallingHandlers(force(expr), warning = function(w) {
    messages <<- c(messages, conditionMessage(w))
  })
  if (length(messages)) attr(value, "quicknet_backend_warnings") <- unique(messages)
  value
}

quicknet_fit_backend_warnings <- function(raw) {
  raw_model <- if (is.list(raw)) raw$model %||% raw else raw
  attr(raw_model, "quicknet_backend_warnings") %||% character()
}

quicknet_fit_diagnostics <- function(fit) {
  if (!is.null(fit$diagnostics)) return(fit$diagnostics)
  quicknet_backend_diagnostics(fit$fit)
}

quicknet_fit_is_valid <- function(fit) {
  diagnostics <- quicknet_fit_diagnostics(fit)
  !any(diagnostics$status == "failed") &&
    all(vapply(fit$networks, function(x) all(is.finite(x)), logical(1)))
}

quicknet_fit_failure_reason <- function(fit) {
  diagnostics <- quicknet_fit_diagnostics(fit)
  failed <- diagnostics[diagnostics$status == "failed", , drop = FALSE]
  if (nrow(failed)) return(paste(paste0(failed$component, ": ", failed$message), collapse = "; "))
  if (!all(vapply(fit$networks, function(x) all(is.finite(x)), logical(1)))) return("The fitted network contains non-finite entries.")
  NULL
}
