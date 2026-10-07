#!/usr/bin/env Rscript
# Run from package root; optional first argument is a new artifact directory.
# Compare public quickNet interfaces with the unmodified native implementations.
# Fixed-design budgets verify implementation; they do not establish a general N.
Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1")
args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args)) args[[1]] else "../output/audit/power-native"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
pkgload::load_all(quiet = TRUE)

assert_equal <- function(actual, expected, label) {
  comparison <- all.equal(actual, expected)
  if (!isTRUE(comparison)) {
    stop(label, ": ", paste(comparison, collapse = "; "), call. = FALSE)
  }
}

truth <- matrix(0, 5, 5, dimnames = list(paste0("v", 1:5), paste0("v", 1:5)))
truth[cbind(1:4, 2:5)] <- 0.30
truth <- truth + t(truth)

# Native netSimulator: continuous multi-condition and five-level ordinal designs.
simulator_designs <- list(
  continuous_conditions = list(seed = 88101L, nCases = c(80L, 120L), nReps = 2L,
    native_args = list(default = "EBICglasso", corMethod = "cor", tuning = c(.25, .5),
      dataGenerator = bootnet::ggmGenerator(ordinal = FALSE), nCores = 1L)),
  ordinal_five_levels = list(seed = 88102L, nCases = c(80L, 120L), nReps = 2L,
    native_args = list(default = "EBICglasso", corMethod = "cor_auto", tuning = .5,
      dataGenerator = bootnet::ggmGenerator(ordinal = TRUE, nLevels = 5), nCores = 1L))
)
simulator_rows <- lapply(names(simulator_designs), function(name) {
  design <- simulator_designs[[name]]
  set.seed(design$seed)
  native <- do.call(bootnet::netSimulator, c(
    list(input = truth, nCases = design$nCases, nReps = design$nReps), design$native_args))
  wrapped <- do.call(NetworkPower, c(
    list(model_matrix = truth, sample_sizes = design$nCases,
      replications = design$nReps, seed = design$seed), design$native_args))
  alias <- do.call(SampleSize, c(
    list(model_matrix = truth, sample_sizes = design$nCases,
      replications = design$nReps, seed = design$seed), design$native_args))
  assert_equal(wrapped$fit, native, paste(name, "native results"))
  assert_equal(alias$fit, native, paste(name, "SampleSize alias results"))
  assert_equal(wrapped$results, native, paste(name, "raw results"))
  utils::capture.output(native_summary <- summary(native))
  utils::capture.output(wrapped_summary <- summary(wrapped))
  assert_equal(wrapped_summary, native_summary, paste(name, "summary"))
  stopifnot(identical(wrapped$method, "netSimulator"),
    identical(wrapped$recommendation$status, "not_applicable"),
    is.na(wrapped$recommendation$recommended_n))
  saveRDS(list(native = native, quickNet = wrapped, design = design, truth = truth),
    file.path(out_dir, paste0("netsimulator-", name, ".rds")))
  write.csv(as.data.frame(native),
    file.path(out_dir, paste0("netsimulator-", name, ".csv")), row.names = FALSE)
  data.frame(design = name, seed = design$seed, native_rows = nrow(native),
    failed_rows = sum(native$error), repetitions_per_condition = design$nReps,
    native_results_equal = TRUE, native_summary_equal = TRUE)
})
simulator_summary <- do.call(rbind, simulator_rows)
write.csv(simulator_summary, file.path(out_dir, "netsimulator-native-parity.csv"), row.names = FALSE)
print(simulator_summary)

# Native powerly: identical population assumptions, random seed and three stages.
source_args <- list(range_lower = 50L, range_upper = 500L, samples = 8L, replications = 20L,
  model_matrix = truth, measure = "sen", statistic = "power", measure_value = .6,
  statistic_value = .8, boots = 80L, iterations = 1L, tolerance = 50L,
  cores = 1L, verbose = FALSE)
set.seed(88021L)
native <- do.call(powerly::powerly, source_args)
wrapped <- do.call(NetworkPower, c(list(method = "powerly", seed = 88021L), source_args))
assert_equal(wrapped$true_network, native$step_1$true_model_parameters, "powerly population graph")
assert_equal(wrapped$fit$step_1$measures, native$step_1$measures, "powerly recovery measures")
assert_equal(wrapped$fit$step_1$statistics, native$step_1$statistics, "powerly attainment statistics")
assert_equal(wrapped$fit$step_2$interpolation, native$step_2$interpolation, "powerly fitted curve")
assert_equal(wrapped$fit$step_3$ci, native$step_3$ci, "powerly bootstrap curves")
assert_equal(wrapped$fit$recommendation, native$recommendation, "powerly native recommendation")
assert_equal(wrapped$recommendation$algorithm_converged, native$converged, "powerly convergence")
assert_equal(wrapped$recommendation$algorithm_iterations, native$iteration, "powerly iterations")
median_n <- native$recommendation[["50%"]]
index <- match(median_n, native$step_2$interpolation$x)
assert_equal(wrapped$recommendation$achieved_probability,
  as.numeric(native$step_3$ci[index, "50%"]), "powerly median-curve probability")
assert_equal(wrapped$recommendation$backend_n_lower,
  unname(native$recommendation[["2.5%"]]), "powerly lower sample-size bound")
assert_equal(wrapped$recommendation$backend_n_upper,
  unname(native$recommendation[["97.5%"]]), "powerly upper sample-size bound")

# New simulation stream: direct native validation and the public validation API.
set.seed(88022L)
native_validation <- powerly::validate(native, replications = 300L, cores = 1L, verbose = FALSE)
validation <- ValidateNetworkPower(wrapped, replications = 300L, seed = 88022L,
  cores = 1L, verbose = FALSE)
assert_equal(validation$fit$sample, native_validation$sample, "validation sample size")
assert_equal(validation$fit$measures, native_validation$measures, "validation recovery measures")
assert_equal(validation$fit$statistic, native_validation$statistic, "validation native probability")
assert_equal(validation$fit$percentile_value, native_validation$percentile_value, "validation percentile")
measures <- as.numeric(native_validation$measures)
successes <- sum(is.finite(measures) & measures >= .6)
repetitions <- length(measures)
probability <- successes / repetitions
interval <- as.numeric(stats::binom.test(successes, repetitions)$conf.int)
assert_equal(validation$summary$achieved_probability, probability, "validation counted probability")
assert_equal(c(validation$summary$probability_ci_lower, validation$summary$probability_ci_upper),
  interval, "independent exact-binomial interval")
assert_equal(validation$summary$probability_mcse,
  sqrt(probability * (1 - probability) / repetitions), "independent Monte Carlo standard error")
expected_status <- if (interval[[1L]] >= .8) "supported" else if (interval[[2L]] < .8) "below_target" else "uncertain"
stopifnot(identical(validation$status, expected_status))

powerly_summary <- data.frame(
  method = "powerly", sample = as.numeric(native_validation$sample),
  native_recommended_n = unname(median_n), wrapper_reached = wrapped$recommendation$reached,
  algorithm_converged = native$converged, algorithm_iterations = native$iteration,
  recommendation_interval_width = wrapped$recommendation$recommendation_interval_width,
  bootstrap_median_probability = wrapped$recommendation$achieved_probability,
  point_curve_probability = wrapped$recommendation$fitted_probability,
  validation_replications = repetitions, validation_successes = successes,
  validation_probability = probability,
  validation_mcse = sqrt(probability * (1 - probability) / repetitions),
  validation_ci_lower = interval[[1L]], validation_ci_upper = interval[[2L]],
  validation_status = validation$status, target_metric = "sensitivity",
  target_value = .6, target_probability = .8)
write.csv(powerly_summary, file.path(out_dir, "powerly-native-validation.csv"), row.names = FALSE)
write.csv(validation$summary, file.path(out_dir, "powerly-independent-validation.csv"), row.names = FALSE)
saveRDS(list(native = native, quickNet = wrapped, native_validation = native_validation,
  validation = validation, args = source_args, truth = truth),
  file.path(out_dir, "powerly-native-validation.rds"))
print(powerly_summary)

provenance <- list(
  source = tools::md5sum(c("R/power.R", "R/power_netsimulator.R", "R/power_validation.R",
    "tools/validate-network-power.R")),
  source_versions = sapply(c("bootnet", "powerly", "qgraph"), function(x) as.character(packageVersion(x))),
  simulator_designs = simulator_designs, source_args = source_args,
  powerly_seed = 88021L, native_validation_seed = 88022L,
  validation_repetitions = 300L, truth = truth, session = sessionInfo())
saveRDS(provenance, file.path(out_dir, "network-power-native-provenance.rds"))
writeLines(capture.output(str(provenance)), file.path(out_dir, "network-power-native-provenance.txt"))
cat("Native netSimulator, powerly and independent validation comparisons passed.\n")
