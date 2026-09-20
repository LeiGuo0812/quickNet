# Shared EBIC policy. NULL means that EBIC gamma is not applicable.
quicknet_default_gamma <- function(model) {
  switch(model,
    EBICglasso = 0.5, graphicalVAR = 0.5,
    ising = 0.25, mgm = 0.25, mixedVAR = 0.25, time_varying_mvar = 0.25,
    NULL
  )
}

quicknet_resolve_gamma <- function(model, gamma = NULL, lambdaSel = "EBIC") {
  if (!is.null(gamma) && (!is.numeric(gamma) || length(gamma) != 1L ||
      !is.finite(gamma) || gamma < 0 || gamma > 1)) {
    stop("gamma must be NULL or one finite number between 0 and 1.", call. = FALSE)
  }
  default <- quicknet_default_gamma(model)
  if (is.null(default) || !identical(lambdaSel, "EBIC")) return(NULL)
  as.numeric(gamma %||% default)
}

# Read evidence from the fitted backend before falling back to saved metadata.
# Never infer the setting of an existing fit from today's default.
quicknet_fit_gamma <- function(fit) {
  if (is.null(quicknet_default_gamma(fit$model))) return(NULL)
  raw <- if (is.list(fit$fit)) fit$fit else list()
  backend_call <- if (is.list(raw$call)) raw$call else list()
  selection <- backend_call$lambdaSel %||% fit$meta$lambdaSel %||% "EBIC"
  if (!identical(selection, "EBIC")) return(NULL)
  backend_gamma <- switch(fit$model,
    EBICglasso = raw$arguments$tuning,
    ising = raw$gamma,
    mgm = backend_call$lambdaGam,
    mixedVAR = backend_call$lambdaGam,
    time_varying_mvar = backend_call$lambdaGam,
    NULL
  )
  gamma <- backend_gamma %||% fit$meta$gamma
  if (is.null(gamma) || !is.numeric(gamma) || length(gamma) != 1L ||
      !is.finite(gamma)) return(NULL)
  quicknet_resolve_gamma(fit$model, gamma, selection)
}

quicknet_refit_gamma <- function(fit) {
  gamma <- quicknet_fit_gamma(fit)
  if (!is.null(quicknet_default_gamma(fit$model)) && is.null(gamma)) {
    stop("The fitted object's EBIC gamma is unknown; refit the original data first.", call. = FALSE)
  }
  gamma
}

quicknet_nct_fit_settings <- function(fit) {
  if (!fit$model %in% c("EBICglasso", "ising", "mgm", "correlation", "partial", "ordinal")) {
    stop("NetCompare requires cross-sectional exploratory fits.", call. = FALSE)
  }
  gamma <- quicknet_fit_gamma(fit)
  if (!is.null(quicknet_default_gamma(fit$model)) && is.null(gamma)) {
    stop("The fitted object's EBIC gamma is unknown; refit it before comparison.", call. = FALSE)
  }
  list(model = fit$model, gamma = gamma,
    cor_method = fit$meta$cor_method %||% "pearson",
    ordinal_method = fit$meta$ordinal_method %||% "polychoric",
    missing = fit$meta$missing %||% "listwise",
    AND = fit$meta$AND %||% TRUE, types = fit$meta$types, levels = fit$meta$levels)
}

quicknet_nct_refit <- function(x, settings) {
  do.call(quicknet_fit_cross_sectional, c(list(data = x), settings))$graph
}

quicknet_nct_estimator_gamma <- function(estimator, args) {
  if (identical(estimator, quicknet_nct_refit)) return(args$settings$gamma)
  if (identical(estimator, NCT_estimator_Ising)) {
    return(quicknet_resolve_gamma("ising", args$gamma))
  }
  if (identical(estimator, NCT_estimator_GGM)) {
    return(quicknet_resolve_gamma("EBICglasso", args$gamma))
  }
  bootnet_models <- c(EBICglasso = "EBICglasso", IsingFit = "ising",
                     mgm = "mgm", graphicalVAR = "graphicalVAR")
  for (backend in names(bootnet_models)) {
    if (identical(estimator, get(paste0("bootnet_", backend), asNamespace("bootnet")))) {
      criterion <- args$criterion %||% "EBIC"
      return(quicknet_resolve_gamma(bootnet_models[[backend]], args$tuning, criterion[[1]]))
    }
  }
  # A custom estimator's 'gamma' can mean something other than EBIC.
  NULL
}
