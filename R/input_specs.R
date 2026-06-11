#' Show input requirements for quickNet models
#'
#' @param model Optional model or function name. If \code{NULL}, requirements
#' for all supported model families are returned.
#'
#' @return A data frame describing required input format, required arguments,
#' coding expectations, and common issues.
#' @export
input_requirements <- function(model = NULL) {
  specs <- quicknet_input_specs()
  if (!is.null(model)) {
    key <- quicknet_input_model_key(model)
    if (!key %in% names(specs)) {
      stop("Unknown model/function: ", model, call. = FALSE)
    }
    specs <- specs[key]
  }
  do.call(rbind, lapply(names(specs), function(name) {
    spec <- specs[[name]]
    data.frame(
      model = name,
      data_shape = spec$data_shape,
      required_arguments = spec$required_arguments,
      variable_coding = spec$variable_coding,
      missing_data = spec$missing_data,
      common_issues = spec$common_issues,
      stringsAsFactors = FALSE
    )
  }))
}

#' Check whether inputs match a quickNet model
#'
#' @param data Input data. Some checks, such as \code{model = "power"}, do not
#' require data.
#' @param model Model or function name.
#' @param ... Model-specific arguments, such as \code{nodes}, \code{waves},
#' \code{vars}, \code{types}, \code{levels}, \code{omega}, or \code{fit}.
#' @param quiet If \code{FALSE}, print a short diagnostic summary.
#'
#' @return A \code{quicknet_input_check} object with \code{ok},
#' \code{errors}, \code{warnings}, and \code{requirements}.
#' @export
check_input <- function(data = NULL, model, ..., quiet = FALSE) {
  args <- list(...)
  out <- quicknet_check_input(data = data, model = model, args = args)
  if (!quiet) print(out)
  invisible(out)
}

#' @export
print.quicknet_input_check <- function(x, ...) {
  cat("<quicknet_input_check>\n")
  cat("Model: ", x$model, "\n", sep = "")
  cat("Status: ", if (isTRUE(x$ok)) "OK" else "FAILED", "\n", sep = "")
  if (length(x$errors) > 0) {
    cat("Errors:\n")
    for (msg in x$errors) cat("- ", msg, "\n", sep = "")
  }
  if (length(x$warnings) > 0) {
    cat("Warnings:\n")
    for (msg in x$warnings) cat("- ", msg, "\n", sep = "")
  }
  invisible(x)
}

quicknet_validate_input <- function(data = NULL, model, ..., warn = TRUE) {
  check <- quicknet_check_input(data = data, model = model, args = list(...))
  if (length(check$errors) > 0) {
    stop(
      "Input does not match requirements for ", check$model, ":\n- ",
      paste(check$errors, collapse = "\n- "),
      call. = FALSE
    )
  }
  if (isTRUE(warn) && length(check$warnings) > 0) {
    warning(
      "Input warnings for ", check$model, ":\n- ",
      paste(check$warnings, collapse = "\n- "),
      call. = FALSE
    )
  }
  invisible(check)
}

quicknet_check_input <- function(data, model, args = list()) {
  key <- quicknet_input_model_key(model)
  specs <- quicknet_input_specs()
  if (!key %in% names(specs)) {
    stop("Unknown model/function: ", model, call. = FALSE)
  }
  messages <- list(errors = character(), warnings = character())
  validator <- switch(
    key,
    EBICglasso = quicknet_check_cross_continuous(data, args),
    correlation = quicknet_check_cross_continuous(data, args),
    partial = quicknet_check_cross_continuous(data, args),
    ordinal = quicknet_check_ordinal(data, args),
    ising = quicknet_check_ising(data, args),
    mgm = quicknet_check_mgm(data, args),
    clpn = quicknet_check_panel(data, args),
    panel_sem = quicknet_check_panel(data, args),
    graphicalVAR = quicknet_check_longitudinal(data, args),
    mlVAR = quicknet_check_longitudinal(data, args),
    confirmatory_ggm = quicknet_check_confirmatory(data, args),
    latent_network = quicknet_check_latent(data, args),
    mixedVAR = quicknet_check_dynamic(data, args, time_varying = FALSE),
    time_varying_mvar = quicknet_check_dynamic(data, args, time_varying = TRUE),
    power = quicknet_check_power(args),
    perturbation = quicknet_check_perturbation(args),
    messages
  )
  errors <- validator$errors
  warnings <- validator$warnings
  structure(
    list(
      model = key,
      ok = length(errors) == 0,
      errors = errors,
      warnings = warnings,
      requirements = input_requirements(key)
    ),
    class = "quicknet_input_check"
  )
}

quicknet_input_specs <- function() {
  list(
    EBICglasso = quicknet_input_spec("wide data.frame/matrix", "data", "continuous numeric columns", "listwise by default", "non-numeric columns; too few complete rows"),
    correlation = quicknet_input_spec("wide data.frame/matrix", "data", "continuous numeric columns", "listwise by default", "non-numeric columns; constant variables"),
    partial = quicknet_input_spec("wide data.frame/matrix", "data", "continuous numeric columns", "listwise by default", "singular correlation matrices; too few rows"),
    ordinal = quicknet_input_spec("wide data.frame/matrix", "data", "ordered numeric category codes", "listwise by default", "too many/few categories; sparse categories"),
    ising = quicknet_input_spec("wide data.frame/matrix", "data", "binary 0/1 columns with both values present", "listwise by default", "values not coded 0/1; no variation"),
    mgm = quicknet_input_spec("wide data.frame/matrix", "data, types, levels", "numeric columns; types/levels match columns", "listwise by default", "wrong type/level length; categorical levels not coded numerically"),
    clpn = quicknet_input_spec("wide panel data.frame", "nodes, waves, optional id", "columns named node + prefix + wave", "complete cases used", "missing wave columns; fewer than two waves"),
    panel_sem = quicknet_input_spec("wide panel data.frame", "nodes, waves, optional id", "columns named node + prefix + wave", "complete cases used", "missing wave columns; fewer than two waves"),
    graphicalVAR = quicknet_input_spec("long ESM data.frame", "vars, id, day, beep", "numeric node variables; repeated measures sorted by id/day/beep", "backend handles selected missing rule", "missing id/day/beep; too few observations per subject"),
    mlVAR = quicknet_input_spec("long ESM data.frame", "vars, id, day, beep", "numeric node variables; repeated measures sorted by id/day/beep", "backend handles selected missing rule", "missing id/day/beep; too few observations per subject"),
    confirmatory_ggm = quicknet_input_spec("wide data.frame/matrix", "vars, optional omega", "continuous numeric columns; omega is square template", "listwise by default", "omega dimensions/names do not match vars"),
    latent_network = quicknet_input_spec("wide data.frame/matrix", "lavaan CFA model syntax", "numeric manifest indicators", "listwise by default", "invalid CFA syntax; factors with too few indicators"),
    mixedVAR = quicknet_input_spec("time-ordered data.frame/matrix", "vars, types, levels", "numeric continuous/category codes in temporal order", "complete cases recommended", "types/levels mismatch; unordered rows"),
    time_varying_mvar = quicknet_input_spec("time-ordered data.frame/matrix", "vars, types, levels, timepoints/estpoints", "numeric continuous/category codes in temporal order", "complete cases recommended", "timepoints length mismatch; invalid estpoints"),
    power = quicknet_input_spec("no raw data required", "nodes, density, sample_sizes, replications", "simulation design parameters", "not applicable", "unrealistic true-network assumptions; too few replications"),
    perturbation = quicknet_input_spec("quicknet_fit object", "fit and method", "supported fitted model for chosen perturbation method", "not applicable", "method not supported for fit$model")
  )
}

quicknet_input_spec <- function(data_shape, required_arguments, variable_coding, missing_data, common_issues) {
  list(
    data_shape = data_shape,
    required_arguments = required_arguments,
    variable_coding = variable_coding,
    missing_data = missing_data,
    common_issues = common_issues
  )
}

quicknet_input_model_key <- function(model) {
  aliases <- c(
    EBICglassoNet = "EBICglasso",
    quickNet = "EBICglasso",
    PanelNet = "clpn",
    LongitudinalNet = "graphicalVAR",
    PanelSEMNet = "panel_sem",
    ConfirmatoryNet = "confirmatory_ggm",
    LatentNet = "latent_network",
    MixedVARNet = "mixedVAR",
    TimeVaryingNet = "time_varying_mvar",
    NetworkPower = "power",
    SampleSize = "power",
    Perturbation = "perturbation"
  )
  model <- as.character(model)[[1]]
  if (model %in% names(aliases)) aliases[[model]] else model
}

quicknet_check_data_frame <- function(data, errors, warnings) {
  if (is.null(data)) {
    errors <- c(errors, "data is required.")
    return(list(data = NULL, errors = errors, warnings = warnings))
  }
  if (!is.data.frame(data) && !is.matrix(data)) {
    errors <- c(errors, "data must be a data.frame or matrix.")
    return(list(data = NULL, errors = errors, warnings = warnings))
  }
  dat <- as.data.frame(data)
  if (ncol(dat) < 2) errors <- c(errors, "data must contain at least two variables.")
  if (nrow(dat) < 3) errors <- c(errors, "data must contain at least three rows.")
  list(data = dat, errors = errors, warnings = warnings)
}

quicknet_check_numeric_columns <- function(dat, cols, errors, warnings) {
  missing_cols <- setdiff(cols, colnames(dat))
  if (length(missing_cols) > 0) {
    errors <- c(errors, paste0("Missing column(s): ", paste(missing_cols, collapse = ", ")))
    return(list(errors = errors, warnings = warnings))
  }
  numeric_cols <- vapply(dat[, cols, drop = FALSE], is.numeric, logical(1))
  if (!all(numeric_cols)) {
    errors <- c(errors, paste0("Non-numeric column(s): ", paste(cols[!numeric_cols], collapse = ", ")))
  }
  complete_n <- sum(stats::complete.cases(dat[, cols, drop = FALSE]))
  if (complete_n < 3) errors <- c(errors, "Fewer than three complete rows are available.")
  constant <- vapply(dat[, cols, drop = FALSE], function(x) length(unique(stats::na.omit(x))) < 2, logical(1))
  if (any(constant)) {
    errors <- c(errors, paste0("No variation in column(s): ", paste(cols[constant], collapse = ", ")))
  }
  list(errors = errors, warnings = warnings)
}

quicknet_check_cross_continuous <- function(data, args) {
  errors <- warnings <- character()
  checked <- quicknet_check_data_frame(data, errors, warnings)
  dat <- checked$data
  errors <- checked$errors
  warnings <- checked$warnings
  if (!is.null(dat)) {
    out <- quicknet_check_numeric_columns(dat, colnames(dat), errors, warnings)
    errors <- out$errors
    warnings <- out$warnings
    if (nrow(dat) <= ncol(dat) + 2) {
      warnings <- c(warnings, "Rows are close to or fewer than the number of variables; estimates may be unstable.")
    }
  }
  list(errors = errors, warnings = warnings)
}

quicknet_check_ordinal <- function(data, args) {
  out <- quicknet_check_cross_continuous(data, args)
  if (length(out$errors) == 0) {
    dat <- as.data.frame(data)
    categories <- vapply(dat, function(x) length(unique(stats::na.omit(x))), integer(1))
    if (any(categories < 3)) out$warnings <- c(out$warnings, "Some ordinal variables have fewer than three observed categories.")
    if (any(categories > 10)) out$warnings <- c(out$warnings, "Some ordinal variables have many categories; consider continuous models if appropriate.")
  }
  out
}

quicknet_check_ising <- function(data, args) {
  errors <- warnings <- character()
  checked <- quicknet_check_data_frame(data, errors, warnings)
  dat <- checked$data
  errors <- checked$errors
  warnings <- checked$warnings
  if (!is.null(dat)) {
    numeric <- vapply(dat, is.numeric, logical(1))
    if (!all(numeric)) errors <- c(errors, paste0("Ising variables must be numeric 0/1. Non-numeric: ", paste(colnames(dat)[!numeric], collapse = ", ")))
    binary <- vapply(dat, function(x) all(stats::na.omit(unique(x)) %in% c(0, 1)), logical(1))
    if (!all(binary)) errors <- c(errors, paste0("Ising variables must be coded 0/1. Invalid: ", paste(colnames(dat)[!binary], collapse = ", ")))
    variation <- vapply(dat, function(x) length(unique(stats::na.omit(x))) == 2, logical(1))
    if (!all(variation)) errors <- c(errors, paste0("Each Ising variable must contain both 0 and 1. No variation: ", paste(colnames(dat)[!variation], collapse = ", ")))
    prevalence <- vapply(dat, function(x) mean(x == 1, na.rm = TRUE), numeric(1))
    rare <- prevalence < 0.05 | prevalence > 0.95
    if (any(rare, na.rm = TRUE)) warnings <- c(warnings, paste0("Very imbalanced binary variable(s): ", paste(colnames(dat)[rare], collapse = ", ")))
  }
  list(errors = errors, warnings = warnings)
}

quicknet_check_mgm <- function(data, args) {
  out <- quicknet_check_cross_continuous(data, args)
  dat <- if (!is.null(data)) as.data.frame(data) else NULL
  types <- args$types
  levels <- args$levels
  if (is.null(dat)) return(out)
  if (is.null(types)) {
    out$warnings <- c(out$warnings, "types is NULL; all variables will be treated as Gaussian.")
  } else if (length(types) != ncol(dat)) {
    out$errors <- c(out$errors, "types must have one entry per variable.")
  }
  if (is.null(levels)) {
    out$warnings <- c(out$warnings, "levels is NULL; levels will be inferred.")
  } else if (length(levels) != ncol(dat)) {
    out$errors <- c(out$errors, "levels must have one entry per variable.")
  }
  out
}

quicknet_check_panel <- function(data, args) {
  errors <- warnings <- character()
  checked <- quicknet_check_data_frame(data, errors, warnings)
  dat <- checked$data
  errors <- checked$errors
  warnings <- checked$warnings
  nodes <- args$nodes
  waves <- args$waves
  prefix <- args$prefix %||% "_t"
  if (is.null(nodes) || length(nodes) < 1) errors <- c(errors, "nodes must be provided.")
  if (is.null(waves) || length(waves) < 2) errors <- c(errors, "At least two waves are required.")
  if (!is.null(dat) && !is.null(nodes) && !is.null(waves)) {
    required <- unlist(lapply(waves, function(wave) paste0(nodes, prefix, wave)))
    out <- quicknet_check_numeric_columns(dat, required, errors, warnings)
    errors <- out$errors
    warnings <- out$warnings
  }
  list(errors = errors, warnings = warnings)
}

quicknet_check_longitudinal <- function(data, args) {
  errors <- warnings <- character()
  checked <- quicknet_check_data_frame(data, errors, warnings)
  dat <- checked$data
  errors <- checked$errors
  warnings <- checked$warnings
  vars <- args$vars
  id <- args$id %||% "id"
  day <- args$day %||% "day"
  beep <- args$beep %||% "beep"
  if (is.null(vars) || length(vars) < 2) errors <- c(errors, "vars must contain at least two node variables.")
  if (!is.null(dat) && !is.null(vars)) {
    required <- c(vars, id, day, beep)
    out <- quicknet_check_numeric_columns(dat, vars, errors, warnings)
    errors <- out$errors
    warnings <- out$warnings
    missing_required <- setdiff(required, colnames(dat))
    if (length(missing_required) > 0) errors <- c(errors, paste0("Missing required column(s): ", paste(missing_required, collapse = ", ")))
    if (id %in% colnames(dat)) {
      obs_by_id <- table(dat[[id]])
      if (any(obs_by_id < 3)) warnings <- c(warnings, "Some subjects have fewer than three observations.")
    }
  }
  list(errors = errors, warnings = warnings)
}

quicknet_check_confirmatory <- function(data, args) {
  vars <- args$vars
  dat <- as.data.frame(data)
  if (is.null(vars) && !is.null(data)) vars <- colnames(dat)
  out <- quicknet_check_cross_continuous(if (!is.null(vars)) dat[, vars, drop = FALSE] else data, args)
  omega <- args$omega
  if (!is.null(omega) && !is.null(vars)) {
    omega <- as.matrix(omega)
    if (!all(dim(omega) == c(length(vars), length(vars)))) {
      out$errors <- c(out$errors, "omega must have one row and one column per variable.")
    }
  }
  out
}

quicknet_check_latent <- function(data, args) {
  vars <- args$vars
  dat <- if (!is.null(data)) as.data.frame(data) else NULL
  if (is.null(vars) && !is.null(dat)) vars <- colnames(dat)
  out <- quicknet_check_cross_continuous(if (!is.null(vars) && !is.null(dat)) dat[, vars, drop = FALSE] else data, args)
  syntax <- args$syntax %||% args$model
  if (is.null(syntax) || !is.character(syntax) || !grepl("=~", syntax)) {
    out$errors <- c(out$errors, "A lavaan CFA model syntax containing '=~' is required.")
  }
  out
}

quicknet_check_dynamic <- function(data, args, time_varying) {
  vars <- args$vars
  dat <- if (!is.null(data)) as.data.frame(data) else NULL
  if (is.null(vars) && !is.null(dat)) vars <- colnames(dat)
  out <- quicknet_check_cross_continuous(if (!is.null(vars) && !is.null(dat)) dat[, vars, drop = FALSE] else data, args)
  if (is.null(vars)) return(out)
  types <- args$types
  levels <- args$levels
  if (is.null(types) || length(types) != length(vars)) out$errors <- c(out$errors, "types must have one entry per variable.")
  if (is.null(levels) || length(levels) != length(vars)) out$errors <- c(out$errors, "levels must have one entry per variable.")
  if (isTRUE(time_varying)) {
    timepoints <- args$timepoints
    estpoints <- args$estpoints %||% c(0.25, 0.50, 0.75)
    if (!is.null(timepoints) && length(timepoints) != nrow(dat)) out$errors <- c(out$errors, "timepoints must have one value per row.")
    if (any(estpoints < 0 | estpoints > 1)) out$errors <- c(out$errors, "estpoints should be between 0 and 1.")
  }
  out
}

quicknet_check_power <- function(args) {
  errors <- warnings <- character()
  nodes <- args$nodes %||% 8
  density <- args$density %||% 0.30
  sample_sizes <- args$sample_sizes %||% c(100, 200, 400)
  replications <- args$replications %||% 100
  if (nodes < 3) errors <- c(errors, "nodes must be at least 3.")
  if (density <= 0 || density > 1) errors <- c(errors, "density must be in (0, 1].")
  if (any(sample_sizes < 5)) errors <- c(errors, "sample_sizes must be at least 5.")
  if (replications < 10) warnings <- c(warnings, "Very few replications; use larger values for applied studies.")
  list(errors = errors, warnings = warnings)
}

quicknet_check_perturbation <- function(args) {
  errors <- warnings <- character()
  fit <- args$fit
  method <- args$method
  if (!inherits(fit, "quicknet_fit")) {
    errors <- c(errors, "fit must be a quicknet_fit object.")
  } else if (!is.null(method)) {
    if (method == "ising_threshold" && fit$model != "ising") errors <- c(errors, "method = 'ising_threshold' requires an Ising fit.")
    if (method != "ising_threshold" && fit$model == "ising") warnings <- c(warnings, "Ising fits should generally use method = 'ising_threshold'.")
  }
  list(errors = errors, warnings = warnings)
}
