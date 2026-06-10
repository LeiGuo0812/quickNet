test_that("quickNet returns a quicknet_fit object for EBICglasso", {
  fit <- suppressWarnings(quickNet(mtcars[, 1:5], pie = FALSE, legend = FALSE))

  expect_s3_class(fit, "quicknet_fit")
  expect_equal(fit$model, "EBICglasso")
  expect_true("default" %in% names(fit$networks))
  expect_equal(dim(fit$graph), c(5L, 5L))
  expect_equal(fit$graph, fit$graphData$graph)
  expect_true(all(diag(fit$graph) == 0))
})

test_that("cross-sectional models share the same quicknet_fit interface", {
  cor_fit <- quickNet(mtcars[, 1:5], model = "correlation", pie = FALSE, legend = FALSE)
  partial_fit <- quickNet(mtcars[, 1:5], model = "partial", pie = FALSE, legend = FALSE)
  ebic_fit <- suppressWarnings(EBICglassoNet(mtcars[, 1:5]))

  expect_s3_class(cor_fit, "quicknet_fit")
  expect_s3_class(partial_fit, "quicknet_fit")
  expect_s3_class(ebic_fit, "quicknet_fit")
  expect_equal(cor_fit$model, "correlation")
  expect_equal(partial_fit$model, "partial")
  expect_named(cor_fit$networks, "default")
  expect_true(nrow(cor_fit$edges) > 0)
  expect_true(all(c("node", "strength", "expected_influence") %in% names(cor_fit$nodes)))
  expect_true("predictability_R2" %in% names(cor_fit$nodes))
  expect_true(all(c("nodes", "possible_edges", "density") %in% names(summary(cor_fit))))
})

test_that("legacy extractors accept quicknet_fit", {
  fit <- quickNet(mtcars[, 1:5], model = "correlation", pie = FALSE, legend = FALSE)

  edges <- get_edges(fit)
  edges_df <- get_edges_df(fit)
  globals <- globalCoeff(fit)

  expect_type(edges, "list")
  expect_true(nrow(edges_df) > 0)
  expect_true(all(c("from", "to", "weight") %in% names(edges_df)))
  expect_true(all(c("globalStrength", "AGS", "ASPL", "CC") %in% names(globals)))
})

test_that("new cross-sectional models return quicknet_fit objects", {
  set.seed(42)
  binary_data <- data.frame(
    b1 = rbinom(120, 1, 0.5),
    b2 = rbinom(120, 1, 0.45),
    b3 = rbinom(120, 1, 0.55),
    b4 = rbinom(120, 1, 0.5)
  )
  ordinal_data <- data.frame(
    o1 = sample(1:5, 120, replace = TRUE),
    o2 = sample(1:5, 120, replace = TRUE),
    o3 = sample(1:5, 120, replace = TRUE),
    o4 = sample(1:5, 120, replace = TRUE)
  )
  mixed_data <- data.frame(
    c1 = rnorm(120),
    c2 = rnorm(120),
    d1 = sample(1:2, 120, replace = TRUE),
    d2 = sample(1:2, 120, replace = TRUE)
  )

  ising_fit <- quickNet(binary_data, model = "ising", pie = FALSE, gamma = 0.25)
  ordinal_fit <- quickNet(ordinal_data, model = "ordinal", pie = FALSE)
  mgm_fit <- quickNet(
    mixed_data,
    model = "mgm",
    pie = FALSE,
    gamma = 0.25,
    types = c("g", "g", "c", "c"),
    levels = c(1, 1, 2, 2)
  )

  expect_s3_class(ising_fit, "quicknet_fit")
  expect_s3_class(ordinal_fit, "quicknet_fit")
  expect_s3_class(mgm_fit, "quicknet_fit")
  expect_equal(ising_fit$model, "ising")
  expect_equal(ordinal_fit$model, "ordinal")
  expect_equal(mgm_fit$model, "mgm")
  expect_true("threshold" %in% names(ising_fit$nodes))
  expect_true(all(c("prevalence", "accuracy", "accuracy_gain") %in% names(ising_fit$nodes)))
  expect_true("predictability_R2" %in% names(ordinal_fit$nodes))
  expect_true(all(c("type", "level") %in% names(mgm_fit$nodes)))
})

test_that("Stability returns model-agnostic stability tables", {
  fit <- quickNet(mtcars[, 1:5], model = "correlation", pie = FALSE, legend = FALSE)
  stability <- Stability(fit, nboot = 3, case.drop = 0.10)

  expect_true(all(c("edge_bootstrap_stability", "case_drop_centrality_stability") %in% names(stability)))
  expect_true(nrow(stability$edge_bootstrap_stability) > 0)
  expect_true(nrow(stability$case_drop_centrality_stability) > 0)
  expect_equal(stability$fit$model, "correlation")
  typo_pattern <- paste0("bri", "ge|stabil", "ty")
  expect_false(any(grepl(typo_pattern, names(stability))))
})
