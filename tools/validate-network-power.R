#!/usr/bin/env Rscript
# Run from package root; optional first argument is the artifact directory.
# Uses native powerly and its validate() method first; independent Gaussian
# simulation subsequently validates quickNet's own conditional Monte Carlo design.
Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1")
args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args)) args[[1]] else "../output/audit/power"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
pkgload::load_all(quiet = TRUE)
make_truth <- function(edges, strength, p = 5L) {
  graph <- matrix(0, p, p, dimnames = list(paste0("v", seq_len(p)), paste0("v", seq_len(p))))
  for (i in seq_len(nrow(edges))) graph[edges[i, 1], edges[i, 2]] <- rep(strength, length.out = nrow(edges))[[i]]
  graph + t(graph)
}
chain <- cbind(1:4, 2:5)
truth <- make_truth(chain, .3)
source_args <- list(range_lower = 50L, range_upper = 500L, samples = 8L, replications = 20L,
  model_matrix = truth, measure = "sen", statistic = "power", measure_value = .6,
  statistic_value = .8, boots = 80L, iterations = 1L, tolerance = 50L, cores = 1L, verbose = FALSE)
set.seed(88021)
native <- do.call(powerly::powerly, source_args)
set.seed(88021)
wrapped <- do.call(NetworkPower, c(list(method = "powerly"), source_args))
stopifnot(identical(native$recommendation, wrapped$fit$recommendation),
  identical(native$step_1$statistics, wrapped$summary$achieved_probability),
  identical(native$step_1$true_model_parameters, wrapped$true_network),
  identical(native$step_1$measures, wrapped$fit$step_1$measures))
median_n <- native$recommendation[["50%"]]
index <- match(median_n, native$step_2$interpolation$x)
stopifnot(isTRUE(all.equal(wrapped$recommendation$achieved_probability,
                         as.numeric(native$step_3$ci[index, "50%"]))))
set.seed(88022)
validated <- powerly::validate(native, replications = 300L, cores = 1L, verbose = FALSE)
native_measures <- as.numeric(validated$measures)
native_successes <- sum(native_measures >= .6)
native_n <- length(native_measures)
native_probability <- native_successes / native_n
native_ci <- as.numeric(binom.test(native_successes, native_n)$conf.int)
native_summary <- data.frame(
  method = "powerly", sample = validated$sample,
  native_recommended_n = median_n, wrapper_reached = wrapped$recommendation$reached,
  bootstrap_median_probability = wrapped$recommendation$achieved_probability,
  point_curve_probability = wrapped$recommendation$fitted_probability,
  validation_replications = native_n, validation_successes = native_successes,
  validation_probability = native_probability,
  validation_mcse = sqrt(native_probability * (1 - native_probability) / native_n),
  validation_ci_lower = native_ci[[1]], validation_ci_upper = native_ci[[2]],
  target_metric = "sensitivity", target_value = .6, target_probability = .8)
stopifnot(isTRUE(all.equal(as.numeric(validated$statistic), native_probability)))
write.csv(native_summary, file.path(out_dir, "powerly-native-validation.csv"), row.names = FALSE)
saveRDS(list(native = native, quickNet = wrapped, validation = validated, args = source_args),
        file.path(out_dir, "powerly-native-validation.rds"))
print(native_summary)

# Independent held-out estimator: distinct base-R Cholesky sampler; direct
# qgraph call; independent confusion-count expression for MCC.
held_out <- function(graph, n, repetitions, seed) {
  set.seed(seed)
  precision <- diag(nrow(graph)) - graph
  covariance <- cov2cor(solve(precision))
  ch <- chol(covariance)
  truth_values <- graph[upper.tri(graph)]
  true_positive_mask <- abs(truth_values) > 1e-10
  do.call(rbind, lapply(seq_len(repetitions), function(i) {
    attempted <- tryCatch({
      data <- matrix(rnorm(n * nrow(graph)), nrow = n) %*% ch
      estimated <- suppressMessages(suppressWarnings(qgraph::EBICglasso(cor(data), n = n, gamma = .5, verbose = FALSE)))
      mask <- abs(estimated[upper.tri(estimated)]) > 1e-10
      a <- sum(mask & true_positive_mask); b <- sum(mask & !true_positive_mask)
      c <- sum(!mask & true_positive_mask); d <- sum(!mask & !true_positive_mask)
      denominator <- sqrt((a + b) * (a + c) * (d + b) * (d + c))
      data.frame(replication = i, metric = if (denominator > 0) (a * d - b * c) / denominator else NA_real_,
                 failed = FALSE, error_message = NA_character_)
    }, error = function(e) data.frame(replication = i, metric = NA_real_, failed = TRUE,
                                     error_message = conditionMessage(e)))
    attempted
  }))
}
scenarios <- list(
  chain_weak = make_truth(chain, .15),
  chain_strong = make_truth(chain, .3),
  star = make_truth(cbind(rep(1, 4), 2:5), .25),
  cycle = make_truth(rbind(chain, c(1, 5)), .25),
  signed = make_truth(rbind(chain, c(1, 3), c(2, 5)), c(.2, -.2, .2, -.2, .2, .2))
)
rows <- list()
for (j in seq_along(scenarios)) {
  name <- names(scenarios)[[j]]
  graph <- scenarios[[j]]
  values <- graph[upper.tri(graph)]; nonzero <- values != 0
  stopifnot(min(eigen(diag(5) - graph, symmetric = TRUE)$values) > 0)
  train <- quicknet_power_monte_carlo(nodes = 5, density = mean(nonzero),
    positive = mean(values[nonzero] > 0), edge_strength = range(abs(values[nonzero])),
    sample_sizes = c(60, 120, 240, 480, 960), replications = 100,
    target_metric = "mcc", target_value = .6, target_probability = .8,
    gamma = .5, estimator = "EBICglasso", seed = 71000L + j,
    threshold = 1e-10, true_network = graph)
  recommended_n <- train$recommendation$recommended_n
  check_n <- if (is.finite(recommended_n)) recommended_n else max(train$summary$sample_size)
  held <- held_out(graph, check_n, 300L, 72000L + j)
  achieved <- is.finite(held$metric) & held$metric >= .6 & !held$failed
  success <- sum(achieved); probability <- success / nrow(held)
  ci <- as.numeric(binom.test(success, nrow(held))$conf.int)
  train_row <- train$summary[train$summary$sample_size == check_n, ]
  rows[[name]] <- data.frame(scenario = name, recommended_n = recommended_n,
    checked_n = check_n, check_type = if (is.finite(recommended_n)) "recommended_candidate" else "upper_range_without_recommendation",
    training_repetitions = 100, training_probability = train_row$achieved_probability,
    training_ci_lower = train_row$probability_ci_lower, training_ci_upper = train_row$probability_ci_upper,
    validation_repetitions = nrow(held), validation_probability = probability,
    validation_mcse = sqrt(probability * (1 - probability) / nrow(held)),
    validation_ci_lower = ci[[1]], validation_ci_upper = ci[[2]],
    validation_failed = sum(held$failed), validation_undefined_metric = sum(!held$failed & !is.finite(held$metric)),
    validation_lower_supports_target = ci[[1]] >= .8,
    training_seed = 71000L + j, validation_seed = 72000L + j)
  saveRDS(list(truth = graph, training = train, held_out = held),
          file.path(out_dir, paste0("monte-carlo-", name, ".rds")))
  write.csv(held, file.path(out_dir, paste0("held-out-", name, ".csv")), row.names = FALSE)
  print(rows[[name]]); flush.console()
}
summary <- do.call(rbind, rows)
write.csv(summary, file.path(out_dir, "monte-carlo-held-out-summary.csv"), row.names = FALSE)
provenance <- list(source = tools::md5sum(c("R/power.R", "tools/validate-network-power.R")),
  source_versions = sapply(c("powerly", "qgraph", "MASS"), function(x) as.character(packageVersion(x))),
  source_args = source_args, native_seed = 88021L, native_validation_seed = 88022L,
  candidate_sample_sizes = c(60, 120, 240, 480, 960), training_repetitions = 100L,
  held_out_repetitions = 300L, target_metric = "mcc", target_value = .6,
  target_probability = .8, scenarios = scenarios, session = sessionInfo())
saveRDS(provenance, file.path(out_dir, "network-power-provenance.rds"))
writeLines(capture.output(str(provenance)), file.path(out_dir, "network-power-provenance.txt"))
