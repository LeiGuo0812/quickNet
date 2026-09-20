resampling_test_fit <- function() {
  set.seed(435)
  quickNet(data.frame(a = rnorm(20), b = rnorm(20), c = rnorm(20)), model = "correlation")
}

test_that("row bootstrap records each failed draw and retains valid conditional summaries", {
  fit <- resampling_test_fit()
  calls <- 0L
  local_mocked_bindings(quicknet_refit_like = function(...) {
    calls <<- calls + 1L
    if (calls == 1L) stop("fixture estimator failed")
    ans <- fit
    if (calls == 2L) ans$graph <- matrix(1, 2, 2)
    if (calls == 3L) ans$graph[1, 2] <- Inf
    ans
  })
  expect_warning(result <- quicknet_bootstrap_edge_stability(fit, 5), "3 of 5")
  expect_equal(result$requested_bootstraps, rep(5, 3))
  expect_equal(result$failed_bootstraps, rep(3, 3))
  expect_equal(result$valid_bootstraps, rep(2, 3))
  expect_equal(result$undefined_bootstraps, rep(0, 3))
  info <- attr(result, "resampling")
  expect_equal(info$failure_details$replication, 1:3)
  expect_match(info$failure_details$reason[1], "fixture estimator failed")
  expect_match(info$failure_details$reason[2], "dimensions")
  expect_match(info$failure_details$reason[3], "Non-finite")
  expect_true(info$conditional_on_success)
})

test_that("all-failure bootstrap retains the first cause in its error", {
  fit <- resampling_test_fit()
  local_mocked_bindings(quicknet_refit_like = function(...) stop("singular fixture"))
  expect_error(quicknet_bootstrap_edge_stability(fit, 3), "All.*First cause: singular fixture")
})

test_that("constant centrality is undefined, while the fitted zero graph is valid", {
  fit <- resampling_test_fit(); fit$graph[,] <- 0
  local_mocked_bindings(quicknet_refit_like = function(...) fit)
  result <- quicknet_case_drop_centrality_stability(fit, 4, proportions = .99)
  expect_equal(result$valid_reps, c(0, 0))
  expect_equal(result$failed_reps, c(0, 0))
  expect_equal(result$undefined_reps, c(4, 4))
  expect_equal(result$observations_retained, c(3, 3))
  expect_equal(result$actual_proportion_dropped, c(.85, .85))
  expect_true(all(is.na(result$median_correlation)))
  edges <- quicknet_bootstrap_edge_stability(fit, 3)
  expect_true(all(edges$selection_rate == 0))
  expect_true(all(is.na(edges$sign_stability)))
})

test_that("participant bootstrap preserves whole trajectories and distinguishes missing layers", {
  graph <- matrix(c(0, .2, .1, 0), 2, dimnames = list(c("a", "b"), c("a", "b")))
  fit <- list(data = data.frame(id = rep(c(100, 200, 300), c(2, 3, 4)),
                                marker = rep(c(100, 200, 300), c(2, 3, 4)), time = c(1:2, 1:3, 1:4)),
              meta = list(id = "id"))
  calls <- 0L; sampled <- list()
  refit <- function(data, i) {
    sampled[[i]] <<- data; calls <<- calls + 1L
    networks <- list(temporal = graph, within = graph)
    if (i == 1) networks$within <- NULL
    if (i == 2) networks$within <- matrix(0, 3, 3)
    list(networks = networks)
  }
  expect_warning(result <- quicknet_cluster_bootstrap_stability(fit, 5, 421, refit,
    list(temporal = graph, within = graph), c(temporal = TRUE, within = FALSE)), "2 of 5")
  for (draw in sampled) for (id in unique(draw$id)) {
    trajectory <- draw[draw$id == id, ]
    expect_length(unique(trajectory$marker), 1)
    expect_equal(trajectory$time, seq_len(nrow(trajectory)))
    expect_equal(nrow(trajectory), c(`100` = 2L, `200` = 3L, `300` = 4L)[as.character(trajectory$marker[1])], ignore_attr = TRUE)
  }
  expect_equal(result$temporal$valid_bootstraps, c(3, 3))
  expect_equal(result$within$valid_bootstraps, 3)
  expect_match(attr(result, "resampling")$failure_details$reason[1], "Missing network layers: within")
  fit$data <- fit$data[fit$data$id == 100, ]
  expect_error(quicknet_cluster_bootstrap_stability(fit, 5, 421, refit, list(default = graph), c(default = TRUE)), "at least two")
})

test_that("known backend failure is excluded but unknown convergence is retained", {
  fit <- resampling_test_fit(); calls <- 0L
  local_mocked_bindings(quicknet_refit_like = function(...) {
    calls <<- calls + 1L; ans <- fit
    if (calls == 1L) ans$diagnostics <- data.frame(component = "optimizer", status = "failed", message = "did not converge")
    ans
  })
  expect_warning(result <- quicknet_bootstrap_edge_stability(fit, 3), "1 of 3")
  expect_equal(result$valid_bootstraps, rep(2, 3))
  expect_match(attr(result, "resampling")$failure_details$reason, "did not converge")
})

test_that("NCT exhaustively matches independent edge and centrality statistics and correction families", {
  pooled <- cbind(a = c(9, 8, 7, 1, -1, 0), b = c(8, 9, 6, -1, 0, 1), c = c(-7, -6, -8, 0, 1, -1))
  estimator <- function(x) { g <- crossprod(x); diag(g) <- 0; g }
  splits <- combn(6, 3, simplify = FALSE)
  reference <- lapply(splits, function(indices) {
    first <- estimator(pooled[indices, ]); second <- estimator(pooled[-indices, ])
    c(global = abs(sum(abs(first[upper.tri(first)])) - sum(abs(second[upper.tri(second)]))),
      max = max(abs(first - second)), abs(first - second)[upper.tri(first)],
      rowSums(abs(first)) - rowSums(abs(second)), rowSums(first) - rowSums(second))
  })
  reference <- do.call(rbind, reference)
  oracle_p <- colMeans(abs(reference) >= matrix(abs(reference[1, ]), 20, ncol(reference), byrow = TRUE))
  expect_true(any(p.adjust(oracle_p[6:11], "holm")[1:3] != p.adjust(oracle_p[6:8], "holm")))
  for (adjust in c("none", "holm", "bonferroni", "BH", "BY", "hochberg", "hommel", "fdr")) {
    counter <- 1L
    exact_fun <- NCT_gl
    env <- new.env(parent = environment(exact_fun))
    env$sample <- function(...) { counter <<- counter + 1L; splits[[counter]] }
    environment(exact_fun) <- env
    actual <- exact_fun(pooled[1:3, ], pooled[4:6, ], estimator = estimator, it = 19,
      test.edges = TRUE, test.centrality = TRUE, centrality = c("strength", "expectedInfluence"),
      p.adjust.methods = adjust, progressbar = FALSE, verbose = FALSE)
    expect_equal(actual$glstrinv.pval, unname(oracle_p[1]))
    expect_equal(actual$nwinv.pval, unname(oracle_p[2]))
    expect_equal(actual$einv.pvals$`p-value`, unname(p.adjust(oracle_p[3:5], adjust)))
    expect_equal(as.numeric(actual$diffcen.pval), unname(p.adjust(oracle_p[6:11], adjust)))
  }
})

test_that("netCor uses ape's node permutations unchanged and rejects unsupported inputs", {
  set.seed(524); a <- cor(matrix(rnorm(200), 50)); b <- cor(matrix(rnorm(200), 50))
  for (alternative in c("two.sided", "less", "greater")) {
    set.seed(55); expected <- ape::mantel.test(a, b, nperm = 39, alternative = alternative)
    set.seed(55); actual <- netCor(a, b, nperm = 39, alternative = alternative)
    expect_equal(actual, expected)
  }
  b[1, 2] <- 4
  expect_error(netCor(a, b), "undirected")
  expect_error(netCor(a, a, nperm = 0), "positive integer")
  expect_equal(netCor(diag(2), diag(2), nperm = 9)$p, 1)
})

test_that("binary observed splits satisfy the same restrictions as conditional permutations", {
  first <- cbind(a = c(1, 0, 0, 0), b = c(0, 1, 0, 0))
  second <- 1 - first
  expect_error(NCT_gl(first, second, binary.data = TRUE, estimator = function(x) crossprod(x),
    it = 3, progressbar = FALSE, verbose = FALSE), "each original dataset")
})

test_that("non-finite upstream NCT centrality never becomes an artificial numeric p-value", {
  original_centrality <- qgraph::centrality_auto
  local_mocked_bindings(centrality_auto = function(...) {
    result <- original_centrality(...)
    result$node.centrality[, 2] <- NA_real_
    result
  }, .package = "qgraph")
  set.seed(919); x <- matrix(rnorm(60), 20, 3); colnames(x) <- letters[1:3]
  zero_estimator <- function(x) matrix(0, ncol(x), ncol(x), dimnames = list(colnames(x), colnames(x)))
  expect_warning(result <- NCT_gl(x, x + .1, estimator = zero_estimator, it = 9,
    centrality = c("closeness", "strength"), test.centrality = TRUE,
    progressbar = FALSE, verbose = FALSE), "undefined")
  expect_true(all(is.na(result$diffcen.pval[, "closeness"])))
  expect_true(all(result$diffcen.pval[, "strength"] == 1))
  expect_true(all(result$centrality_validity$undefined_permutations[result$centrality_validity$statistic == "closeness"] == 9))
})

test_that("native bootnet retry warnings are retained without changing its policy", {
  local_mocked_bindings(bootnet = function(...) {
    warning("7 bootstrap estimation(s) failed and were resampled.")
    list(boots = vector("list", 4))
  })
  expect_warning(result <- quicknet_bootnet_resampling(NULL, 4), "7 bootstrap")
  expect_equal(attr(result, "resampling")$reported_failed_attempts, 7)
  expect_equal(attr(result, "resampling")$returned, 4)
  expect_match(attr(result, "resampling")$warnings, "were resampled")
})
