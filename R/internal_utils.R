quicknet_to_qgraph_matrix <- function(weight_matrix, directed = FALSE) {
  mat <- as.matrix(weight_matrix)
  if (isTRUE(directed)) t(mat) else mat
}

quicknet_is_directed <- function(x, network = "default") {
  if (inherits(x, "quicknet_fit")) {
    if (identical(network, "default") && !"default" %in% names(x$networks)) {
      network <- names(x$networks)[[1L]]
    }
    return(quicknet_network_summary_is_directed(x$model, x$meta, network))
  }
  if (inherits(x, "qgraph")) return(any(x$Edgelist$directed))
  FALSE
}

quicknet_align_network <- function(reference, other) {
  reference <- as.matrix(reference)
  other <- as.matrix(other)
  if (!identical(dim(reference), dim(other))) {
    stop("Networks must have identical dimensions.", call. = FALSE)
  }
  reference_names <- colnames(reference)
  other_names <- colnames(other)
  if (!is.null(reference_names) && !is.null(other_names)) {
    if (anyDuplicated(reference_names) || anyDuplicated(other_names) ||
        !setequal(reference_names, other_names)) {
      stop("Networks must contain the same unique node names.", call. = FALSE)
    }
    order <- match(reference_names, other_names)
    other <- other[order, order, drop = FALSE]
  }
  other
}

quicknet_from_qgraph_matrix <- function(weight_matrix, directed = FALSE) {
  quicknet_to_qgraph_matrix(weight_matrix, directed = directed)
}

quicknet_apply_signs <- function(weights, signs = NULL) {
  weights <- as.matrix(weights)
  if (is.null(signs)) return(weights)

  signs <- as.matrix(signs)
  if (!identical(dim(weights), dim(signs))) {
    stop("weights and signs must have identical dimensions.", call. = FALSE)
  }

  sign_multiplier <- ifelse(is.finite(signs) & signs < 0, -1, 1)
  abs(weights) * sign_multiplier
}

quicknet_missing_mode <- function(missing) {
  if (identical(tolower(as.character(missing)[[1]]), "listwise")) {
    "listwise"
  } else {
    "none"
  }
}

quicknet_is_positive_integer <- function(x) {
  is.numeric(x) && length(x) == 1 && is.finite(x) && x > 0 && x == floor(x)
}

quicknet_validate_lags <- function(lags) {
  valid <- is.numeric(lags) && length(lags) > 0 && all(is.finite(lags)) &&
    all(vapply(lags, quicknet_is_positive_integer, logical(1))) &&
    !anyDuplicated(lags)
  if (!valid) {
    stop("lags must contain unique positive integers.", call. = FALSE)
  }
  as.integer(lags)
}

quicknet_additive_formula <- function(response, predictors, environment = parent.frame()) {
  if (!is.character(response) || length(response) != 1 || is.na(response) ||
      !is.character(predictors) || length(predictors) == 0 || anyNA(predictors)) {
    stop("response and predictors must be non-missing variable names.", call. = FALSE)
  }
  predictor_terms <- lapply(predictors, as.name)
  right_hand_side <- Reduce(
    function(left, right) call("+", left, right),
    predictor_terms
  )
  stats::as.formula(
    call("~", as.name(response), right_hand_side),
    env = environment
  )
}

quicknet_safe_mean <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) NA_real_ else mean(x)
}

quicknet_safe_sd <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 2) NA_real_ else stats::sd(x)
}

quicknet_safe_quantile <- function(x, probability) {
  x <- x[is.finite(x)]
  if (length(x) == 0) {
    NA_real_
  } else {
    unname(stats::quantile(x, probs = probability, names = FALSE))
  }
}

quicknet_check_failed_iterations <- function(failed, context) {
  failed <- as.logical(failed)
  total <- length(failed)
  failed_n <- sum(failed, na.rm = TRUE)
  if (total == 0 || failed_n == 0) return(invisible(TRUE))

  message <- paste0(
    failed_n, " of ", total, " ", context,
    " failed; failure counts are retained in the returned summaries."
  )
  if (failed_n == total) {
    stop("All ", context, " failed; no valid result can be reported.", call. = FALSE)
  }
  warning(message, call. = FALSE)
  invisible(FALSE)
}

quicknet_plot_to_device <- function(filename,
                                    device = c("pdf", "svg"),
                                    width,
                                    height,
                                    plot_function,
                                    ...) {
  device <- match.arg(device)
  if (!is.function(plot_function)) {
    stop("plot_function must be a function.", call. = FALSE)
  }

  if (device == "pdf") {
    grDevices::pdf(file = filename, width = width, height = height, ...)
  } else {
    grDevices::svg(filename = filename, width = width, height = height, ...)
  }
  on.exit(grDevices::dev.off(), add = TRUE)
  plot_function()
  invisible(filename)
}
