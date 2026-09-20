#!/usr/bin/env Rscript
# A single fresh-session benchmark; use benchmark-workflows.py for peak RSS.
args <- commandArgs(trailingOnly = TRUE)
method <- match.arg(args[[1]], c("nira", "symperturb"))
size <- match.arg(args[[2]], c("small", "medium"))
cores <- as.integer(args[[3]])
out <- args[[4]]
pkgload::load_all(quiet = TRUE)
set.seed(5623)
p <- if (size == "small") 6 else 8
n <- if (size == "small") 100 else 300
z <- rnorm(n)
data <- as.data.frame(replicate(p, z * .3 + rnorm(n)))
names(data) <- paste0("x", seq_len(p))
if (method == "nira") data[] <- lapply(seq_len(p), function(i) as.integer(data[[i]] > qnorm(.25 + .04 * i)))
fit <- quickNet(data, model = if (method == "nira") "ising" else "partial", pie = FALSE, DoNotPlot = TRUE)
gc()
started <- proc.time()[["elapsed"]]
if (method == "nira") {
  result <- suppressWarnings(NIRA(fit, run_moderation = FALSE,
    n_samples = if (size == "small") 300L else 500L,
    n_permutations = 99L, stability_reps = if (size == "small") 10L else 15L,
    engine = "literature", engine_iterations = 100L,
    parallel = cores > 1L, ncores = cores, seed = 6261L))
  stopifnot(nrow(result$rankings) == p)
  specification <- "Fixed-network NIRA benchmark; moderation prerequisite disabled for timing only."
} else {
  result <- Perturbation(fit, "symperturb", modules = setNames(rep(c("A", "B"), length.out = p), names(data)),
    config = list(bounds = NULL, sequence_length = 2L, bootstrap_replicates = if (size == "small") 5L else 10L))
  stopifnot(nrow(result$target_scores) == p)
  specification <- "Complete SymPerturb scoring, scenarios, beam sequence and participant bootstrap."
}
elapsed <- proc.time()[["elapsed"]] - started
write.csv(data.frame(method, size, cores, nodes = p, participants = n, elapsed_seconds = elapsed,
                     output_bytes = as.numeric(object.size(result)), specification), out, row.names = FALSE)
