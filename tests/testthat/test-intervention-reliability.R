test_that("state interventions preserve independently constructed Gaussian regression moments", {
  B <- matrix(c(.6, -.3, .2, .5, -.2, .4), 3, 2)
  target_cov <- matrix(c(1, .25, .25, 2), 2)
  residual <- diag(c(.7, .9, .8))
  loading <- rbind(diag(2), B)
  covariance <- loading %*% target_cov %*% t(loading)
  covariance[3:5, 3:5] <- covariance[3:5, 3:5] + residual
  mu <- c(.8, 1.4, 1.1, .6, 2); anchors <- c(.1, .2, 0, 0, 0)
  for (map in c("linked", "location_only", "scale_only", "independent")) {
    for (dose in c(0, .35, 1)) {
      dm <- if (map == "scale_only") 1 else (1-dose)^(if (map == "independent") 1.4 else 1)
      ds <- if (map == "location_only") 1 else (1-dose)^(if (map == "independent") .7 else 1)
      target_mean <- anchors[1:2] + dm * (mu[1:2]-anchors[1:2])
      expected_mean <- c(target_mean, mu[3:5] + drop(B %*% (target_mean-mu[1:2])))
      expected_cov <- loading %*% (ds^2*target_cov) %*% t(loading)
      expected_cov[3:5, 3:5] <- expected_cov[3:5, 3:5] + residual
      result <- quicknet_sym_moments(mu, covariance, 1:2, dose, anchors, map, 1.4, .7)
      expect_equal(result$mean, expected_mean, tolerance = 1e-12)
      expect_equal(result$covariance, expected_cov, tolerance = 1e-12)
      expect_gte(min(eigen(result$covariance, symmetric = TRUE)$values), -1e-12)
    }
  }
})

test_that("bounded-normal expectation agrees with an independent survival integral", {
  grid <- expand.grid(mu = c(-8, -.1, 0, 2, 4, 4.1, 12), sd = c(0, .01, .5, 4, 20))
  expected <- mapply(function(m, s) {
    if (s == 0) return(min(4, max(0, m)))
    integrate(function(t) pnorm(t, m, s, lower.tail = FALSE), 0, 4, rel.tol = 1e-12)$value
  }, grid$mu, grid$sd)
  actual <- quicknet_sym_observed(grid$mu, diag(grid$sd^2), c(0, 4))
  expect_equal(actual, expected, tolerance = 1e-10)
  expect_true(all(actual >= 0 & actual <= 4))
})

test_that("SymPerturb matches node names through permutations and non-ASCII renaming", {
  set.seed(6041)
  data <- matrix(rnorm(400), 80, 5) + rep(c(.8, 1.2, 1.5, .3, 1), each = 80)
  data[, 2] <- data[, 2] + .7*data[, 1]
  colnames(data) <- c("节点乙", "long symptom C", "A", "目标甲", "Z")
  modules <- setNames(c("one", "one", "two", "two", "two"), colnames(data))
  cfg <- quicknet_sym_config(list(bounds = NULL, combination_partner_k = 4L, run_robustness_scenarios = FALSE))
  ctx <- quicknet_sym_context(data, cfg, modules = modules)
  targets <- ctx$nodes
  expected <- quicknet_sym_scores(ctx, targets)$target_scores
  perm <- c(4, 1, 5, 2, 3)
  reordered <- quicknet_sym_context(data[, perm], cfg, modules = modules)
  actual <- quicknet_sym_scores(reordered, rev(targets))$target_scores
  actual <- actual[match(expected$target, actual$target), ]
  expect_equal(actual, expected, tolerance = 1e-10, ignore_attr = TRUE)
  post <- quicknet_sym_post(ctx, targets[c(1, 4)], .4)
  moved <- quicknet_sym_post(reordered, targets[c(4, 1)], .4)
  expect_equal(moved$mean[match(targets, reordered$nodes)], post$mean, tolerance = 1e-12)
  expect_equal(moved$covariance[match(targets, reordered$nodes), match(targets, reordered$nodes)], post$covariance, tolerance = 1e-12)
})

test_that("degenerate participant resamples retain tied minimum ranks and zero topology", {
  data <- matrix(rep(c(1, 2, 3, 4, 5), each = 12), 12)
  colnames(data) <- LETTERS[1:5]
  cfg <- quicknet_sym_config(list(bootstrap_replicates = 2L, bootstrap_top_k = 1L, run_robustness_scenarios = FALSE))
  ctx <- quicknet_sym_context(data, cfg, modules = setNames(c("m1", "m1", "m2", "m2", "m2"), colnames(data)))
  expect_true(ctx$network$used_pseudoinverse)
  expect_equal(ctx$network$adjacency, matrix(0, 5, 5, dimnames = list(colnames(data), colnames(data))))
  boot <- quicknet_sym_bootstrap(ctx, ctx$nodes, matrix(c(rep(1L, 12), 1:12), 2, byrow = TRUE))
  expect_equal(boot$draws$vpps, rep(50, 10))
  expect_equal(boot$draws$rank, rep(1L, 10))
  expect_equal(boot$summary$top_k_selection_probability, rep(1, 5))
  expect_equal(as.vector(tapply(boot$draws$top_k, boot$draws$replicate, sum)), c(5L, 5L))
  expect_equal(quicknet_sym_post(ctx, "A", 0)$covariance, ctx$network$covariance)
})

test_that("a complete beam agrees with an independent conditional-precision objective", {
  set.seed(100102)
  data <- matrix(rnorm(500), 100) %*% matrix(rnorm(25), 5) + matrix(runif(5, .5, 2), 100, 5, byrow = TRUE)
  colnames(data) <- LETTERS[1:5]
  cfg <- quicknet_sym_config(list(bounds = NULL, sequence_length = 3L, sequence_pool = 5L,
    sequence_beam_width = 60L, sequence_eta = .8, sequence_cost_lambda = .1, run_robustness_scenarios = FALSE))
  ctx <- quicknet_sym_context(data, cfg, modules = setNames(c("m1", "m1", "m2", "m2", "m2"), colnames(data)))
  ctx$costs <- setNames(runif(5), ctx$nodes)
  precision <- solve(ctx$network$covariance)
  paths <- as.matrix(expand.grid(rep(list(ctx$nodes), 3)))
  paths <- paths[apply(paths, 1, function(x) length(unique(x)) == 3), ]
  independent_benefit <- function(selected) {
    outcomes <- setdiff(ctx$nodes, selected)
    reduction <- solve(precision[outcomes, outcomes, drop = FALSE],
      precision[outcomes, selected, drop = FALSE] %*% (ctx$anchors[selected]-ctx$network$mu[selected]))
    mean(drop(reduction)/sqrt(diag(ctx$network$covariance)[outcomes]))
  }
  objective <- apply(paths, 1, function(path) {
    benefits <- vapply(1:3, function(k) independent_benefit(path[seq_len(k)]), numeric(1))
    sum(.8^(0:2)*diff(c(0, benefits))) - .1*sum(ctx$costs[path])
  })
  names(objective) <- apply(paths, 1, paste, collapse = " -> ")
  base <- quicknet_sym_scores(ctx, ctx$nodes)$target_scores
  wide <- quicknet_sym_sequence(ctx, base)$table
  expect_equal(nrow(wide), 60L)
  expect_equal(wide$objective, unname(objective[wide$sequence]), tolerance = 1e-10)
  ctx$config$sequence_beam_width <- 1L
  narrow <- quicknet_sym_sequence(ctx, base)$table
  expect_lt(narrow$objective, max(objective))
  expect_equal(max(objective)-narrow$objective, .145305634163903, tolerance = 1e-9)
})

test_that("NIRA infers moderation selection from the fitted backend before stale metadata", {
  W <- matrix(c(0, .3, -.2, .3, 0, .1, -.2, .1, 0), 3,
    dimnames = list(LETTERS[1:3], LETTERS[1:3]))
  thresholds <- c(A = -.6, B = .1, C = .7)
  data <- as.data.frame(as.matrix(expand.grid(A = 0:1, B = 0:1, C = 0:1))[rep(1:8, 4), ])
  fit <- quicknet_fit("ising", data = data, networks = list(default = W),
    nodes = data.frame(node = names(thresholds), threshold = unname(thresholds)),
    fit = list(thresholds = thresholds, AND = FALSE, gamma = .25), meta = list(AND = TRUE, gamma = .25))
  args <- list(fit = fit, n_samples = 20L, engine = "native", engine_iterations = 2L,
    run_moderation = FALSE, run_permutation = FALSE, run_stability = FALSE)
  result <- suppressWarnings(do.call(NIRA, args))
  expect_identical(result$settings$moderation_rule, "OR")
  explicit <- suppressWarnings(do.call(NIRA, c(args, list(moderation_rule = "AND"))))
  expect_identical(explicit$settings$moderation_rule, "AND")
})

test_that("NIRA simulated-score permutations agree with a complete allocation distribution", {
  original <- c(0, 0, 1); intervention <- c(1, 2, 3)
  pooled <- c(original, intervention)
  allocations <- combn(seq_along(pooled), length(intervention))
  null <- apply(allocations, 2, function(i) mean(pooled[i])-mean(pooled[-i]))
  observed <- mean(intervention)-mean(original)
  exact <- mean(abs(null) >= abs(observed)-1e-12)
  B <- 9999L
  result <- quicknet_nira_permutation_one(original, intervention, B, quicknet_nira_make_streams(6051L, 1L)[[1L]])
  expect_equal(result$observed_raw_difference, observed)
  expect_equal(result$p_value, (result$extreme_count+1)/(B+1))
  expect_lt(abs(result$p_value-exact), 6*sqrt(exact*(1-exact)/B))
  identical_scores <- quicknet_nira_permutation_one(rep(2, 3), rep(2, 3), 99L, quicknet_nira_make_streams(6052L, 1L)[[1L]])
  expect_equal(identical_scores$p_value, 1)
})

test_that("NIRA stability workers use the same native sampling law as condition simulations", {
  W <- matrix(c(0, .6, -.4, .6, 0, .2, -.4, .2, 0), 3)
  thresholds <- c(A = -.5, B = .1, C = .8)
  streams <- quicknet_nira_make_streams(6061L, 4L)
  task <- list(task_index = 1L, weight_matrix = W, thresholds = thresholds, beta = 1.3,
    n_samples = 40L, n_iter = 5L, engine = "native", perturbation_type = "alleviating",
    threshold_delta = .4, streams = streams)
  worker <- quicknet_nira_stability_worker(task)
  expected <- vapply(0:3, function(j) {
    changed <- thresholds
    if (j > 0) changed[j] <- changed[j]-.4
    samples <- quicknet_nira_simulate_condition(W, changed, 40L, "native", streams[[j+1L]], 1.3, 5L)
    mean(rowSums(samples))
  }, numeric(1))
  expect_true(worker$ok)
  expect_identical(worker$condition_means, expected)
  expect_equal(worker$absolute_differences, abs(expected[-1L]-expected[1L]))
})
