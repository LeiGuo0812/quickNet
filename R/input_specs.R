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
    ri_clpm = quicknet_check_panel(data, args),
    panel_gvar = quicknet_check_panel(data, args),
    panel_var = quicknet_check_panel(data, args),
    panel_sem = quicknet_check_panel(data, args),
    graphicalVAR = quicknet_check_longitudinal(data, args),
    mlVAR = quicknet_check_longitudinal(data, args),
    psychonetrics_gvar = quicknet_check_longitudinal(data, args),
    confirmatory_ggm = quicknet_check_confirmatory(data, args),
    confirmatory_ising = quicknet_check_confirmatory_ising(data, args),
    confirmatory_cor = quicknet_check_confirmatory(data, args),
    confirmatory_covariance = quicknet_check_confirmatory(data, args),
    confirmatory_precision = quicknet_check_confirmatory(data, args),
    latent_network = quicknet_check_latent(data, c(args, list(latent_model = key))),
    lvm = quicknet_check_latent(data, c(args, list(latent_model = key))),
    lnm = quicknet_check_latent(data, c(args, list(latent_model = key))),
    rnm = quicknet_check_latent(data, c(args, list(latent_model = key))),
    lrnm = quicknet_check_latent(data, c(args, list(latent_model = key))),
    meta_ggm = quicknet_check_meta(data, c(args, list(meta_model = key))),
    meta_cor = quicknet_check_meta(data, c(args, list(meta_model = key))),
    meta_gvar = quicknet_check_meta(data, c(args, list(meta_model = key))),
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
    ri_clpm = quicknet_input_spec("wide panel data.frame", "nodes, waves, optional id", "columns named node + prefix + wave", "complete cases used", "requires at least three waves for stable RI-CLPM interpretation"),
    panel_gvar = quicknet_input_spec("wide panel data.frame", "nodes, waves, optional id", "columns named node + prefix + wave", "complete cases used", "missing wave columns; unstable between-person network with small samples"),
    panel_var = quicknet_input_spec("wide panel data.frame", "nodes, waves, optional id", "columns named node + prefix + wave", "complete cases used", "missing wave columns; contemporaneous layer is covariance-based"),
    panel_sem = quicknet_input_spec("wide panel data.frame", "nodes, waves, optional id", "columns named node + prefix + wave", "selected lavaan missing-data rule", "missing wave columns; fewer than two waves"),
    graphicalVAR = quicknet_input_spec("long ESM data.frame", "vars, id, day, beep", "numeric node variables; repeated measures sorted by id/day/beep", "backend handles selected missing rule", "missing id/day/beep; too few observations per subject"),
    mlVAR = quicknet_input_spec("long ESM data.frame", "vars, id, day, beep", "numeric node variables; repeated measures sorted by id/day/beep", "backend handles selected missing rule", "missing id/day/beep; too few observations per subject"),
    psychonetrics_gvar = quicknet_input_spec("long ESM data.frame", "vars, id, day, beep", "numeric node variables; repeated measures sorted by id/day/beep", "psychonetrics handles selected missing rule", "non-consecutive time indices; too few observations per subject"),
    confirmatory_ggm = quicknet_input_spec("wide data.frame/matrix", "vars, optional omega", "continuous numeric columns; omega is square template", "listwise by default", "omega dimensions/names do not match vars"),
    confirmatory_ising = quicknet_input_spec("wide data.frame/matrix", "vars, optional omega", "binary 0/1 columns; omega is square template", "listwise by default", "values not coded 0/1; omega dimensions/names do not match vars"),
    confirmatory_cor = quicknet_input_spec("wide data.frame/matrix", "vars, optional rho", "continuous numeric columns; rho is square template", "listwise by default", "rho dimensions/names do not match vars"),
    confirmatory_covariance = quicknet_input_spec("wide data.frame/matrix", "vars, optional sigma", "continuous numeric columns; sigma is square template", "listwise by default", "sigma dimensions/names do not match vars"),
    confirmatory_precision = quicknet_input_spec("wide data.frame/matrix", "vars, optional kappa", "continuous numeric columns; kappa is square template", "listwise by default", "kappa dimensions/names do not match vars"),
    latent_network = quicknet_input_spec("wide data.frame/matrix", "lavaan CFA model syntax", "numeric manifest indicators", "listwise by default", "invalid CFA syntax; factors with too few indicators"),
    lvm = quicknet_input_spec("wide data.frame/matrix", "model = 'lvm', lambda", "numeric manifest indicators; lambda rows match vars", "listwise by default", "lambda dimensions/names do not match variables"),
    lnm = quicknet_input_spec("wide data.frame/matrix", "model = 'lnm', lambda", "numeric manifest indicators; latent network estimated in omega_zeta", "listwise by default", "lambda dimensions/names do not match variables"),
    rnm = quicknet_input_spec("wide data.frame/matrix", "model = 'rnm', lambda", "numeric manifest indicators; residual network estimated in omega_epsilon", "listwise by default", "lambda dimensions/names do not match variables"),
    lrnm = quicknet_input_spec("wide data.frame/matrix", "model = 'lrnm', lambda", "numeric manifest indicators; latent and residual networks estimated", "listwise by default", "lambda dimensions/names do not match variables"),
    meta_ggm = quicknet_input_spec("list of correlation/covariance matrices or raw study data", "cors/covs + nobs, or data + studyvar", "square matrices with common variable names", "handled by psychonetrics meta-analytic SEM", "missing nobs; matrices with inconsistent dimensions"),
    meta_cor = quicknet_input_spec("list of correlation/covariance matrices or raw study data", "cors/covs + nobs, or data + studyvar", "square matrices with common variable names", "handled by psychonetrics meta-analytic SEM", "missing nobs; matrices with inconsistent dimensions"),
    meta_gvar = quicknet_input_spec("multi-study intensive longitudinal data or Toeplitz covariances", "data + studyvar + id/day/beep + vars, or covs + nobs + vars", "numeric repeated-measures variables by study", "handled by psychonetrics meta-analytic VAR", "missing study/time identifiers; insufficient studies"),
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
    MetaNet = "meta_ggm",
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

quicknet_check_numeric_columns <- function(dat, cols, errors, warnings, require_complete = TRUE) {
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
  if (isTRUE(require_complete) && complete_n < 3) {
    errors <- c(errors, "Fewer than three complete rows are available.")
  }
  constant <- vapply(dat[, cols, drop = FALSE], function(x) length(unique(stats::na.omit(x))) < 2, logical(1))
  if (any(constant)) {
    errors <- c(errors, paste0("No variation in column(s): ", paste(cols[constant], collapse = ", ")))
  }
  list(errors = errors, warnings = warnings)
}

quicknet_check_selected_data <- function(data, vars) {
  errors <- warnings <- character()
  checked <- quicknet_check_data_frame(data, errors, warnings)
  dat <- checked$data
  errors <- checked$errors
  warnings <- checked$warnings
  if (is.null(dat)) {
    return(list(data = NULL, vars = vars, errors = errors, warnings = warnings))
  }

  if (is.null(vars)) vars <- colnames(dat)
  if (length(vars) < 2) {
    errors <- c(errors, "vars must contain at least two variables.")
  }
  missing_vars <- setdiff(vars, colnames(dat))
  if (length(missing_vars) > 0) {
    errors <- c(errors, paste0("Missing variable(s): ", paste(missing_vars, collapse = ", ")))
    return(list(data = NULL, vars = vars, errors = unique(errors), warnings = unique(warnings)))
  }

  list(
    data = dat[, vars, drop = FALSE],
    vars = vars,
    errors = unique(errors),
    warnings = unique(warnings)
  )
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
  resolved_types <- types %||% rep("g", ncol(dat))
  resolved_levels <- levels
  if (is.null(resolved_levels) && length(resolved_types) == ncol(dat)) {
    resolved_levels <- ifelse(
      resolved_types == "c",
      vapply(dat, function(x) length(unique(x)), integer(1)),
      1L
    )
  }
  if (length(resolved_types) == ncol(dat) && length(resolved_levels) == ncol(dat)) {
    mgm_error <- tryCatch(
      {
        quicknet_dynamic_validate(
          dat,
          colnames(dat),
          resolved_types,
          resolved_levels
        )
        NULL
      },
      error = function(error) conditionMessage(error)
    )
    if (!is.null(mgm_error)) out$errors <- c(out$errors, mgm_error)
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
    out <- quicknet_check_numeric_columns(
      dat,
      required,
      errors,
      warnings,
      require_complete = quicknet_missing_mode(args$missing %||% "listwise") == "listwise"
    )
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
  selected <- quicknet_check_selected_data(data, args$vars)
  vars <- selected$vars
  out <- list(errors = selected$errors, warnings = selected$warnings)
  if (!is.null(selected$data)) {
    if (quicknet_missing_mode(args$missing %||% "listwise") == "listwise") {
      checked <- quicknet_check_cross_continuous(selected$data, args)
    } else {
      checked <- quicknet_check_numeric_columns(
        selected$data,
        colnames(selected$data),
        character(),
        character(),
        require_complete = FALSE
      )
    }
    out$errors <- unique(c(out$errors, checked$errors))
    out$warnings <- unique(c(out$warnings, checked$warnings))
  }
  templates <- list(omega = args$omega, sigma = args$sigma, kappa = args$kappa, rho = args$rho)
  for (template_name in names(templates)) {
    template <- templates[[template_name]]
    if (!is.null(template) && !is.null(vars)) {
      template <- as.matrix(template)
      if (!all(dim(template) == c(length(vars), length(vars)))) {
        out$errors <- c(out$errors, paste0(template_name, " must have one row and one column per variable."))
      }
    }
  }
  out
}

quicknet_check_confirmatory_ising <- function(data, args) {
  selected <- quicknet_check_selected_data(data, args$vars)
  vars <- selected$vars
  out <- list(errors = selected$errors, warnings = selected$warnings)
  if (!is.null(selected$data)) {
    checked <- quicknet_check_ising(selected$data, args)
    if (quicknet_missing_mode(args$missing %||% "listwise") != "listwise") {
      checked$errors <- checked$errors[
        checked$errors != "Fewer than three complete rows are available."
      ]
    }
    out$errors <- unique(c(out$errors, checked$errors))
    out$warnings <- unique(c(out$warnings, checked$warnings))
  }
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
  selected <- quicknet_check_selected_data(data, args$vars)
  vars <- selected$vars
  out <- list(errors = selected$errors, warnings = selected$warnings)
  if (!is.null(selected$data)) {
    if (quicknet_missing_mode(args$missing %||% "listwise") == "listwise") {
      checked <- quicknet_check_cross_continuous(selected$data, args)
    } else {
      checked <- quicknet_check_numeric_columns(
        selected$data,
        colnames(selected$data),
        character(),
        character(),
        require_complete = FALSE
      )
    }
    out$errors <- unique(c(out$errors, checked$errors))
    out$warnings <- unique(c(out$warnings, checked$warnings))
  }
  latent_model <- args$latent_model %||% "latent_network"
  if (latent_model == "latent_network") {
    syntax <- args$syntax %||% args$model
    if (is.null(syntax) || !is.character(syntax) || !grepl("=~", syntax)) {
      out$errors <- c(out$errors, "A lavaan CFA model syntax containing '=~' is required.")
    }
  } else {
    lambda <- args$lambda
    if (is.null(lambda)) {
      out$errors <- c(out$errors, "lambda must be provided for psychonetrics latent models.")
    } else {
      lambda <- as.matrix(lambda)
      if (!is.null(vars) && nrow(lambda) != length(vars)) {
        out$errors <- c(out$errors, "lambda must have one row per manifest variable in vars.")
      }
      if (ncol(lambda) < 1) {
        out$errors <- c(out$errors, "lambda must contain at least one latent variable column.")
      }
      if (!is.null(vars) && !is.null(rownames(lambda)) && !all(vars %in% rownames(lambda))) {
        out$errors <- c(out$errors, "lambda row names must include all vars when row names are supplied.")
      }
    }
  }
  out
}

quicknet_check_dynamic <- function(data, args, time_varying) {
  selected <- quicknet_check_selected_data(data, args$vars)
  vars <- selected$vars
  dat <- selected$data
  out <- list(errors = selected$errors, warnings = selected$warnings)
  if (!is.null(dat)) {
    checked <- quicknet_check_cross_continuous(dat, args)
    out$errors <- unique(c(out$errors, checked$errors))
    out$warnings <- unique(c(out$warnings, checked$warnings))
  }
  if (is.null(vars)) return(out)
  types <- args$types
  levels <- args$levels
  if (is.null(types) || length(types) != length(vars)) out$errors <- c(out$errors, "types must have one entry per variable.")
  if (is.null(levels) || length(levels) != length(vars)) out$errors <- c(out$errors, "levels must have one entry per variable.")
  if (!is.null(dat) && length(types) == length(vars) && length(levels) == length(vars)) {
    dynamic_error <- tryCatch(
      {
        quicknet_dynamic_validate(dat, vars, types, levels)
        NULL
      },
      error = function(error) conditionMessage(error)
    )
    if (!is.null(dynamic_error)) out$errors <- c(out$errors, dynamic_error)
  }
  lags <- args$lags %||% 1
  valid_lags <- is.numeric(lags) && length(lags) > 0 && all(is.finite(lags)) &&
    all(vapply(lags, quicknet_is_positive_integer, logical(1))) && !anyDuplicated(lags)
  if (!valid_lags) out$errors <- c(out$errors, "lags must contain unique positive integers.")
  if (isTRUE(time_varying)) {
    timepoints <- args$timepoints
    estpoints <- args$estpoints %||% c(0.25, 0.50, 0.75)
    if (!is.null(timepoints) && (is.null(dat) || length(timepoints) != nrow(dat))) {
      out$errors <- c(out$errors, "timepoints must have one value per row.")
    }
    if (!is.null(timepoints) && (
      !is.numeric(timepoints) || any(!is.finite(timepoints)) ||
      is.unsorted(timepoints, strictly = TRUE)
    )) {
      out$errors <- c(out$errors, "timepoints must be finite and strictly increasing.")
    }
    if (!is.numeric(estpoints) || length(estpoints) == 0 || any(!is.finite(estpoints)) ||
        any(estpoints < 0 | estpoints > 1) ||
        is.unsorted(estpoints, strictly = TRUE)) {
      out$errors <- c(
        out$errors,
        "estpoints should be strictly increasing finite values between 0 and 1."
      )
    }
  }
  out
}

quicknet_check_meta <- function(data, args) {
  errors <- warnings <- character()
  meta_model <- args$meta_model %||% "meta_ggm"
  cors <- args$cors
  covs <- args$covs
  nobs <- args$nobs
  vars <- args$vars
  studyvar <- args$studyvar
  id <- args$id %||% "id"
  day <- args$day %||% "day"
  beep <- args$beep %||% "beep"

  has_matrices <- !is.null(cors) || !is.null(covs)
  has_data <- !is.null(data)
  if (!has_matrices && !has_data) {
    errors <- c(errors, "Provide cors/covs with nobs, or raw data with studyvar.")
  }

  matrices <- cors %||% covs
  if (!is.null(matrices)) {
    if (!is.list(matrices) || length(matrices) < 2) {
      errors <- c(errors, "cors/covs must be a list containing at least two study matrices.")
    } else {
      dims <- lapply(matrices, dim)
      square <- vapply(dims, function(x) length(x) == 2 && x[[1]] == x[[2]], logical(1))
      if (!all(square)) errors <- c(errors, "Each study matrix must be square.")
      if (length(unique(vapply(dims, paste, collapse = "x", character(1)))) > 1) {
        errors <- c(errors, "Study matrices must have common dimensions.")
      }
      if (is.null(vars) && is.null(colnames(matrices[[1]]))) {
        warnings <- c(warnings, "vars could not be inferred from matrix column names.")
      }
    }
    if (is.null(nobs) || length(nobs) != length(matrices)) {
      errors <- c(errors, "nobs must provide one sample size per study matrix.")
    }
  }

  if (has_data) {
    dat <- as.data.frame(data)
    if (is.null(studyvar) || !studyvar %in% colnames(dat)) {
      errors <- c(errors, "studyvar must identify a study column in data.")
    }
    if (!is.null(studyvar) && studyvar %in% colnames(dat) && length(unique(dat[[studyvar]])) < 2) {
      errors <- c(errors, "At least two studies are required.")
    }
    if (meta_model == "meta_gvar") {
      required <- c(vars, studyvar, id, day, beep)
      missing_required <- setdiff(required, colnames(dat))
      if (length(missing_required) > 0) errors <- c(errors, paste0("Missing required column(s): ", paste(missing_required, collapse = ", ")))
      if (!is.null(vars)) {
        out <- quicknet_check_numeric_columns(dat, vars, errors, warnings)
        errors <- out$errors
        warnings <- out$warnings
      }
    } else if (!is.null(vars)) {
      out <- quicknet_check_numeric_columns(dat, vars, errors, warnings)
      errors <- out$errors
      warnings <- out$warnings
    }
  }

  list(errors = errors, warnings = warnings)
}

quicknet_check_power <- function(args) {
  errors <- warnings <- character()
  nodes <- args$nodes %||% 8
  density <- args$density %||% 0.30
  sample_sizes <- args$sample_sizes
  if (is.null(sample_sizes)) {
    sample_sizes <- if (quicknet_is_positive_integer(nodes)) {
      quicknet_power_default_sample_sizes(nodes)
    } else {
      numeric()
    }
  }
  replications <- args$replications %||% 100
  if (!quicknet_is_positive_integer(nodes) || nodes < 3) {
    errors <- c(errors, "nodes must be an integer of at least 3.")
  }
  if (!is.numeric(density) || length(density) != 1 || !is.finite(density) ||
      density <= 0 || density > 1) {
    errors <- c(errors, "density must be a finite number in (0, 1].")
  }
  valid_sample_sizes <- is.numeric(sample_sizes) && length(sample_sizes) > 0 &&
    all(is.finite(sample_sizes)) &&
    all(vapply(sample_sizes, quicknet_is_positive_integer, logical(1))) &&
    all(sample_sizes >= 5)
  if (!valid_sample_sizes) {
    errors <- c(errors, "sample_sizes must contain positive integers of at least 5.")
  }
  if (!quicknet_is_positive_integer(replications)) {
    errors <- c(errors, "replications must be a positive integer.")
  } else if (replications < 10) {
    warnings <- c(warnings, "Very few replications; use larger values for applied studies.")
  }
  list(errors = errors, warnings = warnings)
}

quicknet_check_perturbation <- function(args) {
  errors <- warnings <- character()
  fit <- args$fit
  method <- args$method
  if (!inherits(fit, "quicknet_fit")) {
    errors <- c(errors, "fit must be a quicknet_fit object.")
  } else if (!is.null(method)) {
    is_ising <- quicknet_is_ising_model(fit$model)
    if (method == "ising_threshold" && !is_ising) errors <- c(errors, "method = 'ising_threshold' requires an Ising fit.")
    if (method != "ising_threshold" && !quicknet_supports_continuous_perturbation(fit$model)) {
      errors <- c(errors, paste0(
        "method = '", method,
        "' requires an EBICglasso, correlation, partial, or ordinal fit; use method = 'ising_threshold' for Ising fits."
      ))
    }
  }
  list(errors = errors, warnings = warnings)
}
