# Developer-only NIRA numerical compatibility validation.
#
# This script is intentionally not run by R CMD check. It uses only public
# quickNet and nodeIdentifyR APIs. nodeIdentifyR is optional and is never a
# quickNet installation dependency.

quicknet_available <- FALSE
if (file.exists("DESCRIPTION") &&
    requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(".", quiet = TRUE)
  quicknet_available <- exists(
    "NIRA",
    envir = asNamespace("quickNet"),
    inherits = FALSE
  )
} else {
  quicknet_available <- requireNamespace("quickNet", quietly = TRUE) &&
    exists("NIRA", envir = asNamespace("quickNet"), inherits = FALSE)
}
if (!quicknet_available) {
  stop("Install quickNet, or run this script from the quickNet source root.")
}
if (!requireNamespace("IsingSampler", quietly = TRUE)) {
  stop("IsingSampler is required for the literature-engine validation.")
}

node_names <- paste0("N", seq_len(5L))
weight_matrix <- matrix(
  0,
  nrow = 5L,
  ncol = 5L,
  dimnames = list(node_names, node_names)
)
weight_matrix[cbind(seq_len(4L), 2:5)] <- c(0.01, 0.02, 0.03, 0.04)
weight_matrix <- weight_matrix + t(weight_matrix)
thresholds <- stats::setNames(c(-2, -1.2, -0.3, 0.2, 3.15), node_names)
seed <- 2025L
n_samples <- 5000L
amount <- 2
threshold_delta <- amount * stats::sd(thresholds)

# Recorded from nodeIdentifyR 1.0.0, commit
# 22ceb4c9c19d6c95a4030ea4ce13d8545cbbfdb3, with R 4.5.3,
# IsingSampler 0.5.0, set.seed(2025), and its public simulateResponses()
# and calculateSumScores() functions.
reference <- data.frame(
  condition = c("original", node_names),
  n = rep(5000L, 6L),
  mean = c(2.2922, 2.1802, 2.0576, 1.9092, 1.7524, 1.6546),
  sd = c(
    0.904534574551637, 0.854794767660029, 0.799506148829641,
    0.761492832708788, 0.774089195646447, 1.002546687178205
  ),
  se = c(
    0.0127920506296630, 0.0120886235347037, 0.0113067243887556,
    0.0107691349166667, 0.0109472743896969, 0.0141781512191963
  ),
  stringsAsFactors = FALSE
)
reference_marginals <- matrix(
  c(
    0.1170, 0.2294, 0.4300, 0.5564, 0.9594,
    0.0026, 0.2360, 0.4194, 0.5604, 0.9618,
    0.1152, 0.0056, 0.4256, 0.5540, 0.9572,
    0.1292, 0.2414, 0.0160, 0.5584, 0.9642,
    0.1264, 0.2252, 0.4174, 0.0272, 0.9562,
    0.1174, 0.2284, 0.4330, 0.5596, 0.3162
  ),
  nrow = 6L,
  byrow = TRUE,
  dimnames = list(reference$condition, node_names)
)

# Complete dummy 0/1 observations are needed only to satisfy the quicknet_fit
# contract; moderation is disabled and the fixed fixture parameters drive all
# simulations.
analysis_data <- as.data.frame(
  do.call(rbind, replicate(13L, expand.grid(rep(list(0:1), 5L)), simplify = FALSE))
)
names(analysis_data) <- node_names
nodes <- data.frame(
  node = node_names,
  network = "default",
  strength = rowSums(abs(weight_matrix)),
  expected_influence = rowSums(weight_matrix),
  degree_nonzero = rowSums(weight_matrix != 0),
  threshold = as.numeric(thresholds),
  stringsAsFactors = FALSE
)
fit <- quickNet::quicknet_fit(
  model = "ising",
  data = analysis_data,
  networks = list(default = weight_matrix),
  nodes = nodes,
  fit = list(thresholds = thresholds),
  meta = list(AND = TRUE, data_type = "cross_sectional")
)

result <- suppressWarnings(quickNet::NIRA(
  fit,
  perturbation_type = "alleviating",
  amount_of_SDs_perturbation = amount,
  n_samples = n_samples,
  run_moderation = FALSE,
  run_permutation = FALSE,
  run_stability = FALSE,
  seed = seed,
  engine = "literature",
  store_samples = TRUE
))

quicknet_statistics <- data.frame(
  condition = c("original", result$interventions$node),
  mean = c(
    result$baseline$statistics$mean_total_score,
    result$interventions$mean_total_score
  ),
  se = c(
    result$baseline$statistics$se_total_score,
    result$interventions$se_total_score
  ),
  stringsAsFactors = FALSE
)
stopifnot(identical(quicknet_statistics$condition, reference$condition))
stopifnot(identical(nrow(result$interventions) + 1L, nrow(reference)))
stopifnot(isTRUE(all.equal(
  result$settings$threshold_delta,
  threshold_delta,
  tolerance = 0
)))
stopifnot(isTRUE(all.equal(
  result$interventions$intervened_threshold,
  as.numeric(thresholds - threshold_delta),
  tolerance = 0
)))

mean_difference <- abs(quicknet_statistics$mean - reference$mean)
combined_mcse <- sqrt(quicknet_statistics$se^2 + reference$se^2)
tolerance <- pmax(0.02, 5 * combined_mcse)
stopifnot(all(mean_difference <= tolerance))

reference_effect <- reference$mean[[1L]] - reference$mean[-1L]
quicknet_effect <- quicknet_statistics$mean[[1L]] - quicknet_statistics$mean[-1L]
reference_order <- order(reference_effect, decreasing = TRUE)
reference_rank <- reference$condition[-1L][reference_order]
quicknet_rank <- quicknet_statistics$condition[-1L][
  order(quicknet_effect, decreasing = TRUE)
]
reference_rank_gaps <- -diff(reference_effect[reference_order])
ordered_intervention_se <- reference$se[-1L][reference_order]
rank_gap_mcse <- sqrt(
  ordered_intervention_se[-length(ordered_intervention_se)]^2 +
    ordered_intervention_se[-1L]^2
)
stopifnot(all(reference_rank_gaps > 5 * rank_gap_mcse))
stopifnot(identical(reference_rank, quicknet_rank))
stopifnot(all(sign(reference_effect) == sign(quicknet_effect)))

quicknet_marginals <- rbind(
  original = colMeans(result$samples$original),
  do.call(rbind, lapply(result$samples$interventions, colMeans))
)
quicknet_marginals <- quicknet_marginals[
  reference$condition,
  node_names,
  drop = FALSE
]
marginal_difference <- abs(quicknet_marginals - reference_marginals)
combined_marginal_mcse <- sqrt(
  quicknet_marginals * (1 - quicknet_marginals) / n_samples +
    reference_marginals * (1 - reference_marginals) / n_samples
)
marginal_tolerance <- pmax(0.01, 5 * combined_marginal_mcse)
stopifnot(all(marginal_difference <= marginal_tolerance))

if (requireNamespace("nodeIdentifyR", quietly = TRUE)) {
  set.seed(seed)
  public_samples <- nodeIdentifyR::simulateResponses(
    weight_matrix,
    thresholds,
    "alleviating",
    amount
  )
  public_scores <- nodeIdentifyR::calculateSumScores(public_samples)
  stopifnot(identical(names(public_scores), reference$condition))
  stopifnot(all(vapply(public_scores, length, integer(1)) == n_samples))
  stopifnot(all(vapply(
    seq_along(public_samples),
    function(index) {
      identical(
        as.numeric(public_scores[[index]]),
        as.numeric(rowSums(public_samples[[index]]))
      )
    },
    logical(1)
  )))
  public_means <- vapply(public_scores, mean, numeric(1))
  stopifnot(max(abs(public_means - reference$mean)) <= 0.02)
  public_marginals <- do.call(rbind, lapply(public_samples, colMeans))
  rownames(public_marginals) <- names(public_samples)
  colnames(public_marginals) <- node_names
  stopifnot(max(abs(public_marginals - reference_marginals)) <= 0.02)
}

print(data.frame(
  condition = reference$condition,
  nodeIdentifyR_mean = reference$mean,
  quickNet_mean = quicknet_statistics$mean,
  absolute_difference = mean_difference,
  five_combined_mcse = 5 * combined_mcse,
  accepted_tolerance = tolerance,
  stringsAsFactors = FALSE
))
cat("Reference rank:", paste(reference_rank, collapse = " > "), "\n")
cat("quickNet rank:  ", paste(quicknet_rank, collapse = " > "), "\n")
cat(
  "Smallest reference adjacent-effect gap / joint MCSE:",
  min(reference_rank_gaps / rank_gap_mcse), "\n"
)
cat(
  "Maximum marginal-probability difference:",
  max(marginal_difference), "\n"
)
cat("NIRA reference compatibility validation passed.\n")
