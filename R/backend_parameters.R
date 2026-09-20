# Keep estimation arguments separate from plotting arguments and reject typos
# before a backend's ... can silently consume them.
quicknet_backend_args <- function(args, fun, reserved = character(), extra = character()) {
  if (!is.list(args) || (length(args) && (is.null(names(args)) ||
      anyNA(names(args)) || any(!nzchar(names(args))) || anyDuplicated(names(args))))) {
    stop("Additional backend arguments must have unique, non-empty names.", call. = FALSE)
  }
  conflicts <- intersect(names(args), reserved)
  if (length(conflicts)) {
    stop("These backend arguments are controlled by the model interface: ",
         paste(conflicts, collapse = ", "), ".", call. = FALSE)
  }
  allowed <- unique(c(setdiff(names(formals(fun)), "..."), extra))
  unknown <- setdiff(names(args), allowed)
  if (length(unknown)) {
    stop("Unknown or unsupported backend argument(s): ", paste(unknown, collapse = ", "),
         ".", call. = FALSE)
  }
  args
}

quicknet_merge_args <- function(defaults, overrides) {
  # Unlike modifyList(), this preserves explicit NULL and replaces nested lists.
  defaults[names(overrides)] <- overrides
  defaults
}

quicknet_backend_default <- function(fun, name, first = FALSE) {
  value <- eval(formals(fun)[[name]], envir = environment(fun))
  if (first) value[[1L]] else value
}

quicknet_backend_provenance <- function(package, fun, args, settings = args,
                                         presets = list()) {
  # Defaults are source expressions, not claims about resolved fit settings.
  f <- get(fun, asNamespace(package))
  list(backend = paste0(package, "::", fun),
       backend_version = as.character(utils::packageVersion(package)),
       backend_args = args,
       backend_settings = settings,
       backend_defaults = lapply(formals(f), function(x) paste(deparse(x), collapse = " ")),
       method_presets = presets)
}

quicknet_cross_missing <- function(model, missing = NULL) {
  default <- switch(model,
    EBICglasso = quicknet_backend_default(bootnet::bootnet_EBICglasso, "missing", TRUE),
    ordinal = "pairwise", "none")
  value <- missing %||% default
  allowed <- if (model == "EBICglasso") c("pairwise", "listwise", "fiml", "stop", "none") else
    c("listwise", "pairwise", "none", "stop")
  match.arg(value, allowed)
}

quicknet_check_row_args <- function(args, operation) {
  row_args <- intersect(names(args), c("weights", "foldid", "offset", "consec", "beepvar", "dayvar"))
  if (length(row_args)) {
    stop(operation, " cannot automatically realign observation-specific backend arguments: ",
         paste(row_args, collapse = ", "), ". Refit with an estimator that resamples these arguments together with the rows.",
         call. = FALSE)
  }
  invisible(TRUE)
}

quicknet_cross_backend_names <- function(model, ordinal_method = "polychoric") {
  fun <- switch(model, EBICglasso = bootnet::bootnet_EBICglasso,
    ising = IsingFit::IsingFit, mgm = mgm::mgm,
    ordinal = if (ordinal_method == "polychoric") psych::polychoric else stats::cor,
    stats::cor)
  out <- setdiff(names(formals(fun)), c("...", "data", "x", "y", "unlock"))
  if (model == "EBICglasso") out <- c(out, setdiff(names(formals(qgraph::EBICglasso)), c("S", "n", "gamma", "...")))
  unique(out)
}

quicknet_psychonetrics_args <- function(fun, args, dots) {
  delegates <- switch(fun, ggm = "varcov", lnm = "lvm", rnm = "lvm", lrnm = "lvm",
    ri_clpm = "lvm", panelgvar = "panelvar", gvar = "var1", meta_gvar = "meta_var1", character())
  extra <- unique(unlist(lapply(delegates, function(nm) names(formals(get(nm, asNamespace("psychonetrics")))))))
  locked <- switch(fun, ggm = "type", lnm = c("latent", "residual"), rnm = c("latent", "residual"),
    lrnm = c("latent", "residual"), gvar = "contemporaneous", meta_gvar = "contemporaneous", character())
  if (length(intersect(names(args), locked))) stop("The selected model fixes ", paste(locked, collapse = ", "), "; select the general model interface to change its parameterization.", call. = FALSE)
  dots <- quicknet_backend_args(dots, get(fun, asNamespace("psychonetrics")),
    reserved = c(setdiff(names(args), "verbose"), locked), extra = extra)
  quicknet_merge_args(args, dots)
}

quicknet_psychonetrics_settings <- function(fit, args) {
  args[c("data", "vars", "cors", "covs", "nobs", "means")] <- NULL
  args$estimator <- fit@estimator
  args$optimizer <- fit@optimizer
  args$identification <- fit@identification
  args
}

quicknet_qgraph_argument_names <- function() {
  out <- setdiff(names(formals(qgraph::qgraph)), "...")
  # qgraph itself validates ... against this literal whitelist. Reuse it so
  # spelling errors cannot be silently diverted from estimation to plotting.
  for (expr in as.list(body(qgraph::qgraph))[-1L]) {
    if (is.call(expr) && length(expr) == 3L && identical(expr[[1L]], quote(`<-`)) &&
        identical(expr[[2L]], quote(allArgs))) {
      out <- c(out, eval(expr[[3L]], envir = asNamespace("qgraph")))
    }
  }
  aliases <- get0("qgraphArgumentAliases", envir = asNamespace("qgraph"), inherits = FALSE)
  unique(c(out, names(aliases)))
}


quicknet_lavaan_estimator <- function(fit) {
  estimator <- fit@Options$estimator.orig %||% fit@Options$estimator
  if (identical(estimator, "default")) fit@Options$estimator else estimator
}
