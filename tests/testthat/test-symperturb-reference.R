sym_reference <- function() {
  jsonlite::fromJSON(test_path("fixtures", "symperturb-reference.json"), simplifyVector = TRUE)
}

sym_fixture_fit <- function() {
  dat <- read.csv(test_path("fixtures", "symperturb-data.csv"))
  graph <- stats::cor(dat)
  diag(graph) <- 0
  quicknet_fit("correlation", data = dat, networks = list(default = graph))
}

sym_expect_table <- function(actual, expected, keys) {
  expect_identical(sort(names(actual)), sort(names(expected)))
  if (!nrow(expected)) return(invisible(NULL))
  ord <- function(x) do.call(order, x[keys])
  actual <- actual[ord(actual), names(expected), drop = FALSE]
  expected <- expected[ord(expected), , drop = FALSE]
  for (nm in names(expected)) {
    if (is.numeric(expected[[nm]])) expect_equal(as.numeric(actual[[nm]]), as.numeric(expected[[nm]]), tolerance = 1e-8, info = nm)
    else expect_equal(as.character(actual[[nm]]), as.character(expected[[nm]]), info = nm)
  }
}

test_that("complete analysis matches seven independently generated Python oracles", {
  ref <- sym_reference()
  fit <- sym_fixture_fit()
  for (label in names(ref$cases)) {
    expected <- ref$cases[[label]]
    cfg <- expected$config
    cfg$vpps_weights <- unlist(cfg$vpps_weights)
    args <- list(fit = fit, method = "symperturb", modules = unlist(ref$modules),
                 targets = expected$candidate_targets,
                 anchors = unlist(ref$anchors), symptom_weights = unlist(ref$symptom_weights),
                 costs = unlist(ref$costs), config = cfg)
    if (length(expected$bootstrap_indices)) args$bootstrap_indices <- expected$bootstrap_indices
    actual <- do.call(Perturbation, args)
    for (nm in names(expected$network)) expect_equal(unname(actual$network[[nm]]), expected$network[[nm]], tolerance = 1e-10, info = paste(label, nm))
    sym_expect_table(actual$target_scores, expected$target_scores, "target")
    sym_expect_table(actual$dose_response, expected$dose_response, c("target", "alpha"))
    sym_expect_table(actual$pair_scores, expected$pair_scores, c("target_a", "target_b"))
    sym_expect_table(actual$sequence, expected$sequence, "sequence_rank")
    sym_expect_table(actual$network_edges, expected$network_edges, c("source", "target"))
    if (is.data.frame(expected$robustness)) {
      sym_expect_table(actual$robustness, expected$robustness, "target")
      expect_equal(length(unique(actual$scenario_ranks$scenario)), 13L)
    }
    if (is.data.frame(expected$bootstrap)) {
      sym_expect_table(actual$bootstrap, expected$bootstrap, "target")
      sym_expect_table(actual$bootstrap_draws, expected$bootstrap_draws, c("replicate", "target"))
    }
    expect_false(actual$metadata$robustness_in_vpps)
    expect_length(actual$metadata$vpps_dimensions, 7L)
    ctx <- quicknet_sym_context(fit$data, quicknet_sym_config(cfg), unlist(ref$anchors),
                               unlist(ref$symptom_weights), unlist(ref$modules), unlist(ref$costs))
    for (i in seq_len(nrow(expected$moments))) {
      moment <- expected$moments[i, , drop = FALSE]
      post <- quicknet_sym_post(ctx, unlist(moment$targets), moment$alpha)
      expect_equal(unname(post$mean), unlist(moment$mean), tolerance = 1e-10, info = label)
      expect_equal(unname(post$covariance), moment$covariance[[1]], tolerance = 1e-10, info = label)
      expect_equal(unname(quicknet_sym_observed(post$mean, post$covariance, ctx$config$bounds)), unlist(moment$observed), tolerance = 1e-10, info = label)
    }
    for (i in seq_len(nrow(expected$topology))) {
      t <- expected$topology[i, ]
      tc <- ctx$config
      tc$adjacency_normalization <- t$normalization
      tc$propagation_absolute <- t$absolute
      W <- ctx$network$adjacency
      expect_equal(quicknet_sym_potential(W, tc), t$baseline, tolerance = 1e-10)
      expect_equal(quicknet_sym_potential(quicknet_sym_block(W, c(1, 2), t$q, TRUE), tc), t$edge, tolerance = 1e-10)
      expect_equal(quicknet_sym_potential(quicknet_sym_block(W, 1, t$q), tc), t$node, tolerance = 1e-10)
    }
  }
})

test_that("singular state blocks and normalization match Python", {
  ref <- sym_reference()
  s <- ref$singular
  post <- quicknet_sym_moments(s$mu, s$covariance, c(1L, 2L), c(.4, .7), c(.1, .2, .3))
  expect_equal(post$mean, s$post_mean, tolerance = 1e-10)
  expect_equal(post$covariance, s$post_covariance, tolerance = 1e-10)
  for (i in seq_len(nrow(ref$minmax_inputs))) expect_equal(quicknet_sym_minmax(ref$minmax_inputs[i, ]), ref$minmax_outputs[i, ], tolerance = 1e-12)
})

test_that("state endpoints, linear unbounded dosage and signed combinations obey the contract", {
  cov <- matrix(.5, 3, 3); diag(cov) <- 1
  mu <- c(A = 1, B = 1, C = 1)
  dimnames(cov) <- list(names(mu), names(mu))
  zero <- quicknet_sym_moments(mu, cov, 1L, 0)
  expect_equal(zero$mean, mu)
  expect_equal(zero$covariance, cov)
  ko <- quicknet_sym_moments(mu, cov, 1L, 1)
  expect_equal(ko$mean, c(A = 0, B = .5, C = .5))
  expect_equal(ko$covariance[1, ], c(A = 0, B = 0, C = 0))
  expect_equal(unname(ko$covariance[2:3, 2:3]), matrix(c(.75, .25, .25, .75), 2))
  all <- quicknet_sym_moments(mu, cov, 1:3, 1)
  expect_equal(as.numeric(all$mean), rep(0, 3))
  expect_equal(as.numeric(all$covariance), rep(0, 9))
  fit <- sym_fixture_fit()
  dosage <- Perturbation(fit, dose = c(0, .25, .5, 1), config = list(bounds = NULL), targets = "A")
  g <- dosage$metrics$system_benefit
  expect_equal(g, c(0, .25, .5, 1) * g[4], tolerance = 1e-10)
  kd <- Perturbation(fit, method = "knockdown", remaining_strength = 0, targets = "A", config = list(bounds = NULL))
  expect_equal(kd$metrics$system_benefit, g[4], tolerance = 1e-10)
  ctx <- quicknet_sym_context(fit$data, quicknet_sym_config(list(bounds = NULL)))
  ctx$network$mu <- mu; ctx$network$covariance <- cov
  ctx$nodes <- names(mu); ctx$anchors <- mu * 0; ctx$weights <- mu * 0 + 1
  ctx$observed <- mu; ctx$denominator <- mu * 0 + 1
  pair <- quicknet_sym_pair(ctx, "A", "B")
  expect_equal(pair$incremental_pair_value, 1/6, tolerance = 1e-12)
  ctx$network$mu <- ctx$observed <- c(A = 3, B = .1, C = 1)
  expect_lt(quicknet_sym_pair(ctx, "A", "B")$incremental_pair_value, 0)
})

test_that("bootstrap repeats the full pipeline and preserves caller RNG", {
  fit <- sym_fixture_fit()
  modules <- c(A = "m1", B = "m1", C = "m2", D = "m2", E = "m2")
  cfg <- list(bootstrap_replicates = 3, run_robustness_scenarios = FALSE)
  set.seed(77); state <- .Random.seed
  a <- Perturbation(fit, "symperturb", modules = modules, config = cfg)
  expect_identical(.Random.seed, state)
  b <- Perturbation(fit, "symperturb", modules = modules, config = cfg)
  expect_equal(a$bootstrap_draws, b$bootstrap_draws)
  subfit <- fit
  subfit$data <- fit$data[a$bootstrap_indices[1, ], , drop = FALSE]
  single <- Perturbation(subfit, "symperturb", modules = modules, config = list(run_robustness_scenarios = FALSE))
  raw <- a$bootstrap_draws[a$bootstrap_draws$replicate == 1, ]
  expect_equal(raw$vpps, single$target_scores$vpps)
  expect_equal(raw$rank, single$target_scores$rank)
})
