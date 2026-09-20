#!/usr/bin/env Rscript
# Run from the package root. Optional arguments: replications permutations output.
# Numerical upstream parity comes first; simulation then scopes the assumptions.
args <- commandArgs(trailingOnly = TRUE)
replications <- if (length(args)) as.integer(args[[1]]) else 300L
permutations <- if (length(args) > 1L) as.integer(args[[2]]) else 199L
out_dir <- if (length(args) > 2L) args[[3]] else "../output/audit/inference"
stopifnot(replications >= 1L, permutations >= 19L)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
devtools::load_all(quiet = TRUE)
upstream_commit <- "f05b102f2fa5c64da2f41e39052acbce18cff633"
upstream <- new.env(parent = asNamespace("quickNet"))
base_url <- paste0("https://raw.githubusercontent.com/cvborkulo/NetworkComparisonTest/", upstream_commit, "/")
for (file in c("R/NCT.R", "R/estimators.R", "DESCRIPTION")) {
  local <- file.path(out_dir, paste0("upstream-NCT-", basename(file)))
  utils::download.file(paste0(base_url, file), local, quiet = TRUE)
  if (grepl("[.]R$", file)) sys.source(local, envir = upstream)
}
upstream_version <- read.dcf(file.path(out_dir, "upstream-NCT-DESCRIPTION"))[, "Version"]
writeLines(c(paste0("commit=", upstream_commit), paste0("version=", upstream_version),
  paste0("R=", R.version.string), paste0("replications=", replications),
  paste0("permutations=", permutations), "master_seed=20260920",
  capture.output(sessionInfo())), file.path(out_dir, "paired-nct-provenance.txt"))
# Its call recorder expects argument expressions, as in an ordinary R call.
# Evaluate symbols bound to values instead of embedding objects with do.call().
call_native <- function(fun, args) {
  env <- list2env(c(list(fun = fun), args), parent = parent.frame())
  expr <- as.call(c(list(as.name("fun")), stats::setNames(lapply(names(args), as.name), names(args))))
  eval(expr, envir = env)
}
cor_graph <- function(data) { graph <- stats::cor(data); diag(graph) <- 0; graph }
make_pairs <- function(n, rho = 0.35) {
  covariance <- matrix(rho, 3, 3); diag(covariance) <- 1
  ch <- chol(covariance)
  draw <- function() matrix(rnorm(n * 3), n) %*% ch
  common <- draw()
  x <- sqrt(0.6) * common + sqrt(0.4) * draw()
  y <- sqrt(0.6) * common + sqrt(0.4) * draw()
  colnames(x) <- colnames(y) <- letters[1:3]
  list(x = x, y = y)
}
set.seed(87133)
continuous <- make_pairs(65)
binary <- lapply(make_pairs(90), function(x) (x > 0) * 1)
parity_cases <- list(
  paired_GGM = list(data = continuous, options = list(paired = TRUE, gamma = 0.5, it = 31,
    test.edges = TRUE, test.centrality = TRUE, p.adjust.methods = "holm")),
  paired_Ising = list(data = binary, options = list(paired = TRUE, binary.data = TRUE,
    gamma = 0.25, it = 19, test.edges = TRUE)),
  paired_correlation = list(data = continuous, options = list(paired = TRUE,
    estimator = cor_graph, it = 63, test.edges = TRUE)),
  independent_correlation = list(data = continuous, options = list(paired = FALSE,
    estimator = cor_graph, it = 63, test.edges = TRUE))
)
parity <- lapply(names(parity_cases), function(name) {
  case <- parity_cases[[name]]
  call_args <- c(list(data1 = case$data$x, data2 = case$data$y,
    progressbar = FALSE, verbose = FALSE), case$options)
  set.seed(76231)
  expected <- suppressWarnings(suppressMessages(call_native(upstream$NCT, call_args)))
  set.seed(76231)
  actual <- suppressWarnings(suppressMessages(do.call(NCT_gl, call_args)))
  saveRDS(list(upstream = expected, quickNet = actual), file.path(out_dir, paste0(name, "-raw.rds")))
  fields <- c("nw1", "nw2", "nwinv.real", "nwinv.perm", "nwinv.pval", "glstrinv.real",
    "glstrinv.perm", "glstrinv.pval", "einv.real", "einv.perm", "diffcen.real", "diffcen.perm", "diffcen.pval")
  deltas <- vapply(fields, function(field) {
    if (is.null(expected[[field]]) && is.null(actual[[field]])) return(0)
    max(abs(as.numeric(expected[[field]]) - as.numeric(actual[[field]])))
  }, numeric(1))
  edge_delta <- max(abs(as.numeric(expected$einv.pvals$`p-value`) - actual$einv.pvals$`p-value`))
  data.frame(case = name, statistic = c(fields, "edge_pvalues"),
             maximum_absolute_difference = c(deltas, edge_delta))
})
parity <- do.call(rbind, parity)
write.csv(parity, file.path(out_dir, "paired-nct-upstream-parity.csv"), row.names = FALSE)
print(aggregate(maximum_absolute_difference ~ case, parity, max))
stopifnot(all(is.finite(parity$maximum_absolute_difference)),
          max(parity$maximum_absolute_difference) < 1e-10)

# Independent row-assignment reference for the participant-weight regression.
set.seed(8241)
wdata <- make_pairs(6)
weights <- c(1, 2, 3, 5, 8, 13)
weighted_graph <- function(data, weights) {
  graph <- crossprod(data * sqrt(weights)) / sum(weights); diag(graph) <- 0; graph
}
set.seed(4192)
swaps <- replicate(63, sample(c(1, 2), 6, replace = TRUE) == 2)
reference <- apply(swaps, 2, function(swap) {
  a <- wdata$x; b <- wdata$y
  for (i in which(swap)) { a[i, ] <- wdata$y[i, ]; b[i, ] <- wdata$x[i, ] }
  max(abs(weighted_graph(a, weights) - weighted_graph(b, weights))[upper.tri(diag(3))])
})
wargs <- list(data1 = wdata$x, data2 = wdata$y, paired = TRUE, it = 63,
  estimator = weighted_graph, estimatorArgs = list(weights = weights),
  progressbar = FALSE, verbose = FALSE)
set.seed(4192); wsource <- call_native(upstream$NCT, wargs)
set.seed(4192); wfixed <- do.call(NCT_gl, wargs)
weights_check <- data.frame(draw = seq_along(reference), row_assignment_oracle = reference,
                           upstream = wsource$nwinv.perm, quickNet = wfixed$nwinv.perm)
write.csv(weights_check, file.path(out_dir, "paired-nct-participant-weights.csv"), row.names = FALSE)
stopifnot(max(abs(wfixed$nwinv.perm - reference)) < 1e-12,
          max(abs(wsource$nwinv.perm - reference)) > 0.01)

set.seed(20260920)
scenarios <- c("exchangeable_equal_network", "equal_network_unequal_scale", "equal_network_mean_shift")
seeds <- matrix(sample.int(.Machine$integer.max, replications * length(scenarios)),
                replications, length(scenarios))
rows <- vector("list", length(scenarios))
for (scenario_index in seq_along(scenarios)) {
  scenario <- scenarios[[scenario_index]]
  result <- lapply(seq_len(replications), function(replication) {
    set.seed(seeds[replication, scenario_index])
    data <- make_pairs(50)
    if (scenario == "equal_network_unequal_scale") data$y <- sweep(data$y, 2, c(0.25, 1, 4), `*`)
    if (scenario == "equal_network_mean_shift") data$y <- sweep(data$y, 2, c(3, -3, 3), `+`)
    fit <- NCT_gl(data$x, data$y, paired = TRUE, estimator = cor_graph, it = permutations,
                  progressbar = FALSE, verbose = FALSE)
    data.frame(scenario = scenario, replication = replication,
               seed = seeds[replication, scenario_index], n_pairs = 50L,
               permutations = permutations, nwinv_p = fit$nwinv.pval, glstrinv_p = fit$glstrinv.pval)
  })
  rows[[scenario_index]] <- do.call(rbind, result)
  cat(scenario, "completed", replications, "replications\n")
  flush.console()
  write.csv(rows[[scenario_index]], file.path(out_dir, paste0("paired-nct-", scenario, ".csv")), row.names = FALSE)
}
raw <- do.call(rbind, rows)
write.csv(raw, file.path(out_dir, "paired-nct-calibration-raw.csv"), row.names = FALSE)
summary <- do.call(rbind, lapply(scenarios, function(scenario) {
  subset <- raw[raw$scenario == scenario, ]
  do.call(rbind, lapply(c("nwinv_p", "glstrinv_p"), function(statistic) {
    rejected <- sum(subset[[statistic]] <= 0.05)
    rate <- rejected / nrow(subset)
    ci <- binom.test(rejected, nrow(subset))$conf.int
    data.frame(scenario = scenario, statistic = statistic, replications = nrow(subset),
      rejections = rejected, rejection_rate = rate, mcse = sqrt(rate * (1 - rate) / nrow(subset)),
      ci_lower = ci[[1]], ci_upper = ci[[2]], exchangeable = scenario == scenarios[[1]])
  }))
}))
write.csv(summary, file.path(out_dir, "paired-nct-calibration-summary.csv"), row.names = FALSE)
print(summary)
