# Developer-only NIRApost public-behavior validation.
#
# Normal quickNet installation and R CMD check do not run this script. Use an
# installed NIRApost package, or point NIRAPOST_SOURCE_DIR at a clean checkout
# of commit d8055a8806ba0c52bf46c36098a25c84f27f648e. In the latter mode the
# script installs that checkout into a temporary library and calls only its
# public exported functions; no GPL source is copied into quickNet.

expected_nirapost_commit <-
  "d8055a8806ba0c52bf46c36098a25c84f27f648e"

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
if (!requireNamespace("nodeIdentifyR", quietly = TRUE)) {
  stop("nodeIdentifyR 1.0.0 is required for NIRApost stability validation.")
}
if (!identical(as.character(utils::packageVersion("nodeIdentifyR")), "1.0.0")) {
  stop("This validation requires nodeIdentifyR 1.0.0 exactly.")
}

nirapost_source_dir <- Sys.getenv("NIRAPOST_SOURCE_DIR", unset = NA_character_)
if (!is.na(nirapost_source_dir) && nzchar(nirapost_source_dir)) {
  nirapost_source_dir <- normalizePath(
    nirapost_source_dir,
    mustWork = TRUE
  )
  description_file <- file.path(nirapost_source_dir, "DESCRIPTION")
  description <- read.dcf(description_file)
  stopifnot(
    identical(unname(description[1L, "Package"]), "NIRApost"),
    identical(unname(description[1L, "Version"]), "1.1.0"),
    grepl("GPL", unname(description[1L, "License"]), fixed = TRUE)
  )
  git_directory <- file.path(nirapost_source_dir, ".git")
  if (file.exists(git_directory) || dir.exists(git_directory)) {
    observed_commit <- system2(
      "git",
      c("-C", shQuote(nirapost_source_dir), "rev-parse", "HEAD"),
      stdout = TRUE
    )
    stopifnot(identical(observed_commit[[1L]], expected_nirapost_commit))
  }

  reference_library <- tempfile("quicknet-nirapost-library-")
  dir.create(reference_library)
  install_output <- system2(
    file.path(R.home("bin"), "R"),
    c(
      "CMD", "INSTALL", "--no-multiarch",
      "-l", shQuote(reference_library),
      shQuote(nirapost_source_dir)
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  install_status <- attr(install_output, "status")
  if (is.null(install_status)) install_status <- 0L
  if (!identical(as.integer(install_status), 0L)) {
    stop(
      paste(
        "Temporary NIRApost installation failed:",
        paste(tail(install_output, 20L), collapse = "\n"),
        sep = "\n"
      )
    )
  }
  .libPaths(c(reference_library, .libPaths()))
}

if (!requireNamespace("NIRApost", quietly = TRUE)) {
  stop(
    paste(
      "Install NIRApost, or set NIRAPOST_SOURCE_DIR to its clean checkout;",
      "all of NIRApost's declared Imports must be installed."
    )
  )
}
if (!identical(as.character(utils::packageVersion("NIRApost")), "1.1.0")) {
  stop("This validation requires NIRApost 1.1.0 exactly.")
}
nirapost_namespace <- asNamespace("NIRApost")

reference_function <- function(name) {
  getExportedValue("NIRApost", name)
}

# Permutation API and plus-one/p.adjust semantics.
original_scores <- rep(c(0, 1), 20L)
intervention_scores <- list(
  N1 = rep(c(4, 5), 20L),
  N2 = rep(c(3, 4), 20L)
)
long_scores <- data.frame(
  sumscore = c(original_scores, unlist(intervention_scores, use.names = FALSE)),
  sample = rep(c("original", "N1", "N2"), each = 40L),
  stringsAsFactors = FALSE
)
nirapost_permutation <- reference_function("permutationNIRAtest")(
  long_scores,
  method = "holm"
)
stopifnot(
  identical(names(nirapost_permutation), c("stat", "plot_data")),
  all(c(
    "mean_other", "sd_other", "se_other", "ciLower_other",
    "ciUpper_other", "cohen_d", "p", "p.adjust"
  ) %in% names(nirapost_permutation$stat)),
  all(nirapost_permutation$stat$p * 5001 == round(
    nirapost_permutation$stat$p * 5001
  )),
  isTRUE(all.equal(
    nirapost_permutation$stat$p.adjust,
    stats::p.adjust(nirapost_permutation$stat$p, method = "holm"),
    tolerance = 0
  ))
)

permutation_streams <- quickNet:::quicknet_nira_expand_stream(
  quickNet:::quicknet_nira_make_streams(2025L, 1L)[[1L]],
  length(intervention_scores)
)
quicknet_p <- vapply(seq_along(intervention_scores), function(index) {
  quickNet:::quicknet_nira_permutation_one(
    original_scores,
    intervention_scores[[index]],
    n_permutations = 5000L,
    stream = permutation_streams[[index]]
  )$p_value
}, numeric(1))
stopifnot(
  all(nirapost_permutation$stat$p == 1 / 5001),
  identical(unname(quicknet_p), rep(1 / 5001, 2L))
)

# Clear-rank stability fixture.
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

nirapost_stability <- reference_function("stabilityNIRAtest")(
  edge_weights = weight_matrix,
  thresholds = thresholds,
  perturbation_type = "alleviating",
  amount_of_SDs_perturbation = 2,
  nReps = 2L,
  parallel = FALSE,
  seed = 2025L
)
nirapost_ranks <- reference_function("findMaxN")(
  nirapost_stability,
  n = length(node_names)
)
stopifnot(
  identical(dim(nirapost_stability$mean), c(2L, 6L)),
  identical(dim(nirapost_stability$sd), c(2L, 6L)),
  all(rowSums(nirapost_ranks[, paste0("repeattop_", 1:5)]) == 2L)
)

analysis_data <- as.data.frame(
  do.call(
    rbind,
    replicate(
      13L,
      expand.grid(rep(list(0:1), 5L)),
      simplify = FALSE
    )
  )
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
quicknet_stability <- suppressWarnings(quickNet::NIRA(
  fit,
  n_samples = 5000L,
  run_moderation = FALSE,
  run_permutation = FALSE,
  run_stability = TRUE,
  stability_reps = 2L,
  top_n = length(node_names),
  seed = 2025L,
  engine = "literature"
))
quicknet_rank_counts <- xtabs(
  count ~ node + rank,
  data = quicknet_stability$stability$rank_frequencies
)
quicknet_rank_counts <- quicknet_rank_counts[
  rownames(nirapost_ranks),
  as.character(seq_len(5L)),
  drop = FALSE
]
nirapost_rank_counts <- as.matrix(
  nirapost_ranks[, paste0("repeattop_", seq_len(5L)), drop = FALSE]
)
stopifnot(
  identical(
    dim(quicknet_rank_counts),
    dim(nirapost_rank_counts)
  ),
  identical(
    as.integer(quicknet_rank_counts),
    as.integer(nirapost_rank_counts)
  ),
  identical(quicknet_stability$stability$valid_reps, 2L),
  identical(quicknet_stability$stability$failed_reps, 0L)
)

# Moderation public-output meaning. Two resamples are a smoke test only.
set.seed(18)
moderation_n <- 250L
first <- stats::rbinom(moderation_n, 1, 0.5)
second <- stats::rbinom(moderation_n, 1, 0.5)
third <- stats::rbinom(
  moderation_n,
  1,
  stats::plogis(2 - 4 * first * second)
)
moderation_data <- cbind(A = first, B = second, C = third)
nirapost_moderation <- suppressWarnings(
  reference_function("runMgmmAnalysis")(
    moderation_data,
    plotResults = FALSE,
    rule = "AND",
    lambdaGam = 0.25,
    nB = 2L
  )
)
quicknet_moderation <- quickNet:::quicknet_nira_run_moderation(
  data = moderation_data,
  node_names = colnames(moderation_data),
  rule = "AND",
  lambda = 0.25,
  nboot = 2L,
  stage_stream =
    quickNet:::quicknet_nira_make_streams(123L, 1L)[[1L]],
  use_parallel = FALSE,
  ncores = 1L
)
stopifnot(
  identical(names(nirapost_moderation), c(
    "all_results", "significant_moderators"
  )),
  length(nirapost_moderation$all_results) == ncol(moderation_data),
  is.list(nirapost_moderation$significant_moderators),
  length(nirapost_moderation$significant_moderators) ==
    ncol(moderation_data),
  all(c(
    "moderator", "moderated_node_1", "moderated_node_2",
    "mean_moderation_estimate", "ci_lower", "ci_upper",
    "same_sign_proportion", "stable_moderation",
    "valid_reps", "failed_reps"
  ) %in% names(quicknet_moderation$table)),
  identical(quicknet_moderation$valid_reps, 2L),
  identical(quicknet_moderation$failed_reps, 0L),
  isTRUE(quicknet_moderation$stable_detected),
  all(quicknet_moderation$table$stable_moderation),
  all(quicknet_moderation$table$estimate_scale == "signed"),
  all(quicknet_moderation$table$full_sample_estimate < 0),
  all(quicknet_moderation$table$mean_moderation_estimate < 0),
  all(quicknet_moderation$table$negative_proportion == 1),
  all(quicknet_moderation$table$same_sign_proportion == 1),
  all(quicknet_moderation$table$reference_sign == -1L),
  all(quicknet_moderation$table$direction_defined_reps == 2L)
)

cat("NIRApost permutation, stability, and moderation validation passed.\n")
