# A descriptive recovery simulation using the authors' original implementation.
# Do not turn netSimulator's recovery curves into powerly's probability criterion.
quicknet_power_netsimulator_args <- function(model_matrix = NULL,
                                             sample_sizes = NULL,
                                             replications = NULL,
                                             native_args = list()) {
  if (!is.list(native_args) || (length(native_args) &&
      (is.null(names(native_args)) || anyNA(names(native_args)) ||
       any(!nzchar(names(native_args))) || anyDuplicated(names(native_args))))) {
    stop("netSimulator arguments must have unique, non-empty names.", call. = FALSE)
  }
  if (!is.null(model_matrix) && "input" %in% names(native_args)) {
    stop("Supply either model_matrix or netSimulator's input, not both.", call. = FALSE)
  }
  if (!is.null(sample_sizes) && "nCases" %in% names(native_args)) {
    stop("Supply either sample_sizes or netSimulator's nCases, not both.", call. = FALSE)
  }
  if (!is.null(replications) && "nReps" %in% names(native_args)) {
    stop("Supply either replications or netSimulator's nReps, not both.", call. = FALSE)
  }
  args <- native_args
  if (!is.null(model_matrix)) args$input <- model_matrix
  if (!"input" %in% names(args) || is.null(args$input)) {
    stop("netSimulator requires an assumed network: supply model_matrix or input.", call. = FALSE)
  }
  input <- args$input
  if (!is.function(input)) {
    graph <- if (is.list(input)) input$graph else input
    if (!is.matrix(graph) || !is.numeric(graph) || nrow(graph) != ncol(graph) ||
        nrow(graph) < 2L || any(!is.finite(graph))) {
      stop("netSimulator input must be a numeric square network matrix, a list containing graph, or a network-generating function.", call. = FALSE)
    }
  } else {
    graph <- NULL
  }
  if (!is.null(sample_sizes)) args$nCases <- sample_sizes
  if (!is.null(replications)) args$nReps <- replications
  for (name in intersect(c("nCases", "nReps", "nCores"), names(args))) {
    value <- args[[name]]
    if (!is.numeric(value) || !length(value) || any(!is.finite(value)) ||
        any(value <= 0) || any(value != round(value)) ||
        (name != "nCases" && length(value) != 1L)) {
      stop(name, " must contain positive integers",
           if (name == "nCases") "." else " and must be scalar.", call. = FALSE)
    }
  }
  args
}

quicknet_power_netsimulator <- function(model_matrix = NULL,
                                        sample_sizes = NULL,
                                        replications = NULL,
                                        seed = NULL,
                                        native_args = list()) {
  args <- quicknet_power_netsimulator_args(model_matrix, sample_sizes,
                                           replications, native_args)
  input <- args$input
  graph <- if (is.function(input)) NULL else if (is.list(input)) input$graph else input
  # Missing arguments stay missing: the native function resolves the estimator,
  # generator, grid, repetitions and cores, including custom estimator cases.
  if (!is.null(seed)) set.seed(seed)
  fit <- do.call(bootnet::netSimulator, args)

  # summary.netSimulator returns its native Mean (SD) table invisibly. Capture
  # printing without altering values. An all-failed run remains inspectable.
  summary_error <- NULL
  native_summary <- tryCatch({
    utils::capture.output(value <- base::summary(fit))
    value
  }, error = function(e) {
    summary_error <<- conditionMessage(e)
    NULL
  })
  native_defaults <- formals(bootnet::netSimulator)
  default <- if ("default" %in% names(fit)) unique(as.character(fit$default)) else args$default
  if (is.null(default) && !"default" %in% names(args)) {
    estimator_conditions <- setdiff(names(args), names(native_defaults))
    default <- if (!"dataGenerator" %in% names(args) ||
                   !length(estimator_conditions)) "EBICglasso" else "none"
  }
  model <- if (any(default %in% c("IsingFit", "IsingSampler"))) "ising" else if (
    all(default %in% c("EBICglasso", "glasso", "pcor", "adalasso", "huge",
                       "ggmModSelect", "LoGo"))) "ggm" else "custom"
  settings <- list(
    backend = "bootnet::netSimulator",
    backend_version = as.character(utils::packageVersion("bootnet")),
    backend_args = args,
    backend_defaults = lapply(native_defaults, function(value) paste(deparse(value), collapse = " ")),
    sample_sizes = if ("nCases" %in% names(args)) args$nCases else
      eval(native_defaults$nCases, envir = environment(bootnet::netSimulator)),
    replications = if ("nReps" %in% names(args)) args$nReps else
      eval(native_defaults$nReps, envir = environment(bootnet::netSimulator)),
    nCores = if ("nCores" %in% names(args)) args$nCores else
      eval(native_defaults$nCores, envir = environment(bootnet::netSimulator)),
    nodes = if (is.null(graph)) NULL else nrow(graph),
    estimator = default,
    seed = seed,
    reference_type = if (is.function(input)) "network_generator" else "fixed_network",
    recovery_definition = "native_netSimulator_metrics",
    recommendation_rule = "not_applicable_descriptive_recovery_simulation",
    reference = "Epskamp & Fried (2018), doi:10.1037/met0000167"
  )
  recommendation <- data.frame(
    status = "not_applicable",
    recommended_n = NA_real_,
    reached = NA,
    stringsAsFactors = FALSE
  )
  report <- paste0(
    "Native bootnet::netSimulator recovery simulation. ",
    if (is.function(input)) "The input function generates the assumed network for each replication. " else
      "Results are conditional on the supplied assumed network. ",
    "Estimator: ", paste(default, collapse = ", "), ". ",
    "Sensitivity, specificity, edge-weight and centrality recovery are preserved using the native definitions. ",
    "This method describes recovery across the supplied conditions; it does not automatically recommend a sample size or apply a target-achievement probability criterion.",
    if (!is.null(summary_error)) paste0(" Native summary unavailable: ", summary_error) else ""
  )
  object <- quicknet_power_object(
    method = "netSimulator", model = model, settings = settings,
    true_network = graph, generating_network = graph,
    results = fit, summary = native_summary,
    recommendation = recommendation, fit = fit, report = report
  )
  object$raw <- fit
  object$summary_error <- summary_error
  object
}
