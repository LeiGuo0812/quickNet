test_that("directed matrices consistently use rows as destinations", {
  mat <- matrix(
    c(0, 0.7, -0.2, 0),
    2,
    byrow = TRUE,
    dimnames = list(c("a", "b"), c("a", "b"))
  )
  fit <- quicknet_fit(
    model = "clpn",
    networks = list(default = mat),
    meta = list(directed = TRUE, row_is = "to", col_is = "from")
  )

  expect_equal(nrow(fit$edges), 2)
  expect_equal(
    fit$edges$weight[fit$edges$from == "a" & fit$edges$to == "b"],
    -0.2
  )
  expect_equal(fit$nodes$out_expected_influence[fit$nodes$node == "a"], -0.2)
  expect_equal(nrow(fit$Edgelist), 2)

  backend_matrix <- matrix(c(0, 0.4, -0.6, 0), 2, byrow = TRUE)
  internal <- quicknet_from_qgraph_matrix(backend_matrix, directed = TRUE)
  expect_equal(internal[2, 1], backend_matrix[1, 2])
  expect_equal(
    quicknet_to_qgraph_matrix(internal, directed = TRUE),
    backend_matrix
  )

  mlvar_internal <- quicknet_mlvar_standardize_net(
    backend_matrix,
    type = "temporal",
    vars = c("a", "b")
  )
  expect_equal(mlvar_internal[2, 1], 0.4)

  requested_indices <- integer()
  lag_networks <- quicknet_mlvar_temporal_networks(
    fit = NULL,
    vars = c("a", "b"),
    lags = c(1, 3),
    get_net = function(fit, type, vars, lag) {
      requested_indices <<- c(requested_indices, lag)
      matrix(lag, length(vars), length(vars), dimnames = list(vars, vars))
    }
  )
  expect_named(lag_networks, c("temporal_lag_1", "temporal_lag_3"))
  expect_equal(requested_indices, c(1L, 2L))
  expect_equal(unname(lag_networks$temporal_lag_3[1, 1]), 2)
})

test_that("directed plots use qgraph's source-by-destination convention", {
  skip_if_not_installed("qgraph")
  mat <- matrix(
    c(0, 0.7, -0.2, 0),
    2,
    byrow = TRUE,
    dimnames = list(c("a", "b"), c("a", "b"))
  )
  fit <- quicknet_fit(
    model = "clpn",
    networks = list(default = mat),
    meta = list(directed = TRUE, row_is = "to", col_is = "from")
  )
  graph <- plot(fit, DoNotPlot = TRUE)

  expect_true(graph$Arguments$directed)
  expect_equal(unname(qgraph::getWmat(graph)), unname(t(mat)))
})

test_that("MGM signs and every requested lag are retained", {
  fake_mvar <- list(
    wadj = array(c(0, 2, 3, 0, 0, 4, 5, 0), dim = c(2, 2, 2)),
    signs = array(c(NA, -1, 1, NA, NA, 1, -1, NA), dim = c(2, 2, 2))
  )
  layers <- quicknet_dynamic_extract_mvar_networks(
    fake_mvar,
    vars = c("a", "b"),
    lags = c(1, 3)
  )

  expect_named(layers, c("lag_1", "lag_3"))
  expect_equal(layers$lag_1[2, 1], -2)
  expect_equal(layers$lag_3[1, 2], -5)

  fake_tvmvar <- list(
    wadj = array(rep(fake_mvar$wadj, 2), dim = c(2, 2, 2, 2)),
    signs = array(rep(fake_mvar$signs, 2), dim = c(2, 2, 2, 2))
  )
  local_layers <- quicknet_dynamic_extract_tvmvar_networks(
    fake_tvmvar,
    vars = c("a", "b"),
    estpoints = c(0.25, 0.75),
    lags = c(1, 3)
  )
  expect_true(all(c(
    "estpoint_1", "estpoint_1_lag_1", "estpoint_1_lag_3",
    "estpoint_2", "estpoint_2_lag_1", "estpoint_2_lag_3"
  ) %in% names(local_layers)))
  expect_equal(local_layers$estpoint_1_lag_1[2, 1], -2)
  expect_equal(local_layers$estpoint_2_lag_3[1, 2], -5)

  invalid_dynamic <- check_input(
    data.frame(a = 1:6, b = c(1, 2, 1, 2, NA, 1)),
    model = "mixedVAR",
    types = c("g", "c"),
    levels = c(1, 2),
    quiet = TRUE
  )
  expect_false(invalid_dynamic$ok)
  expect_match(paste(invalid_dynamic$errors, collapse = " "), "missing")
})

test_that("negative MTD coupling uses an absolute two-sided permutation tail", {
  set.seed(123)
  trajectory <- cumsum(rep(c(1, 2, 4, 3), 5))
  result <- MTD.No.Smooth.Test(cbind(trajectory, -trajectory), nperm = 199)

  expect_lt(result$coupling_mean, 0)
  expect_lt(result$p.value, 0.05)
  expect_error(MTD.No.Smooth.Test(cbind(1:20, 1), nperm = 10), "nonzero")
})

test_that("power probabilities count failed replications in the denominator", {
  expect_equal(
    quicknet_power_achieved_probability(c(NA, 0.9), "mcc", 0.8),
    0.5
  )
  expect_equal(
    quicknet_power_achieved_probability(c(NA, NA), "mcc", 0.8),
    0
  )

  summary <- data.frame(
    sample_size = c(100, 200),
    achieved_probability = c(NA_real_, 0.70)
  )
  recommendation <- quicknet_power_recommend(summary, target_probability = 0.80)
  expect_false(recommendation$reached)
  expect_true(is.na(recommendation$recommended_n))
})

test_that("selected-variable validation does not subset missing columns prematurely", {
  data <- data.frame(
    id = seq_len(10),
    b1 = rep(0:1, 5),
    b2 = rep(1:0, 5)
  )
  valid <- check_input(
    data,
    model = "confirmatory_ising",
    vars = c("b1", "b2"),
    quiet = TRUE
  )
  invalid <- check_input(
    data,
    model = "confirmatory_ggm",
    vars = c("b1", "missing"),
    quiet = TRUE
  )

  expect_true(valid$ok)
  expect_false(invalid$ok)
  expect_match(paste(invalid$errors, collapse = " "), "Missing variable")
})

test_that("non-listwise missing rules retain incomplete observations", {
  data <- data.frame(
    x = c(1, 2, 3, NA, NA, NA),
    y = c(NA, NA, NA, 1, 2, 3)
  )
  check <- check_input(
    data,
    model = "confirmatory_ggm",
    vars = c("x", "y"),
    missing = "fiml",
    quiet = TRUE
  )
  retained <- quicknet_complete_numeric_data(data, missing = "none")

  expect_true(check$ok)
  expect_equal(nrow(retained), nrow(data))
  expect_equal(sum(is.na(retained)), sum(is.na(data)))
})

test_that("non-syntactic names and empty edge sets are handled", {
  set.seed(10)
  data <- data.frame(
    check.names = FALSE,
    "symptom 1" = rnorm(30),
    "symptom-2" = rnorm(30)
  )
  predictability <- quicknet_continuous_predictability(data)
  expect_true(all(is.finite(predictability$predictability_R2)))

  zero_network <- matrix(
    0,
    3,
    3,
    dimnames = list(letters[1:3], letters[1:3])
  )
  expect_length(get_edges(zero_network), 0)
  expect_equal(nrow(get_edges_df(zero_network)), 0)

  singleton <- matrix(1, 1, 1, dimnames = list("factor", "factor"))
  singleton_fit <- quicknet_fit(
    model = "latent_network",
    networks = list(default = singleton),
    meta = list(directed = FALSE)
  )
  expect_equal(nrow(singleton_fit$edges), 0)
  expect_equal(nrow(singleton_fit$Edgelist), 0)
  expect_equal(singleton_fit$network_summary$density, 0)
})

test_that("PanelNet stores generated IDs and groups transitions by subject", {
  skip_if_not_installed("glmnet")
  set.seed(20)
  data <- data.frame(
    x_t1 = rnorm(24),
    y_t1 = rnorm(24),
    x_t2 = rnorm(24),
    y_t2 = rnorm(24),
    x_t3 = rnorm(24),
    y_t3 = rnorm(24)
  )
  fit <- PanelNet(data, nodes = c("x", "y"), waves = 1:3, nfolds = 4)

  expect_true("id" %in% names(fit$data))
  fold_ids <- fit$fit$glmnet$foldid
  design_ids <- fit$fit$design$meta$id
  expect_true(all(vapply(
    split(fold_ids, design_ids),
    function(values) length(unique(values)) == 1,
    logical(1)
  )))

  stability <- LongitudinalStability(fit, nboot = 1, nfolds = 4)
  expect_true(all(stability$default$valid_bootstraps == 1))
})

test_that("NCT aligns variable names and supports disabling edge tests", {
  estimator <- function(x) {
    out <- stats::cor(x)
    diag(out) <- 0
    out
  }
  set.seed(30)
  data1 <- data.frame(a = rnorm(30), b = rnorm(30), c = rnorm(30))
  data2 <- data1[, c("c", "a", "b")]

  result <- NCT_gl(
    data1,
    data2,
    it = 2,
    estimator = estimator,
    test.edges = FALSE,
    test.centrality = FALSE,
    progressbar = FALSE
  )
  expect_lt(result$nwinv.real, 1e-12)
  expect_null(result$einv.pvals)

  wrapped <- NetCompare(
    data1,
    data2,
    it = 2,
    estimator = estimator,
    test.edges = FALSE,
    test.centrality = FALSE,
    progressbar = FALSE
  )
  expect_null(wrapped$edge_weight_p)
  expect_equal(wrapped$net1_mask, (wrapped$nw1 != 0) * 1)
})

test_that("NCT centrality subsets follow the requested node order", {
  skip_if_not_installed("qgraph")
  estimator <- function(x) {
    out <- stats::cor(x)
    diag(out) <- 0
    out
  }
  set.seed(31)
  data1 <- data.frame(a = rnorm(40), b = rnorm(40), c = rnorm(40))
  data2 <- data.frame(
    a = data1$a + 0.6 * data1$b,
    b = data1$b,
    c = data1$c - 0.4 * data1$b
  )
  result <- NCT_gl(
    data1,
    data2,
    it = 2,
    estimator = estimator,
    test.edges = FALSE,
    test.centrality = TRUE,
    centrality = "strength",
    nodes = c("c", "a"),
    progressbar = FALSE
  )
  centrality1 <- qgraph::centrality_auto(result$nw1)$node.centrality
  centrality2 <- qgraph::centrality_auto(result$nw2)$node.centrality
  expected <- centrality1[c("c", "a"), "Strength"] -
    centrality2[c("c", "a"), "Strength"]

  expect_equal(rownames(result$diffcen.real), c("c", "a"))
  expect_equal(as.numeric(result$diffcen.real[, "strength"]), as.numeric(expected))
})

test_that("binary NCT fails quickly when valid splits are impossible", {
  sparse1 <- data.frame(a = c(0, 1, 0), b = c(0, 1, 1))
  sparse2 <- data.frame(a = c(0, 1, 1), b = c(1, 0, 1))
  estimator <- function(x) {
    out <- stats::cor(x)
    diag(out) <- 0
    out
  }

  expect_error(
    NCT_gl(
      sparse1,
      sparse2,
      it = 1,
      binary.data = TRUE,
      estimator = estimator,
      test.centrality = FALSE,
      progressbar = FALSE
    ),
    "at least four observations"
  )
})

test_that("ordinal perturbations invert their correlation matrix", {
  correlation <- matrix(
    c(0, 0.6, 0.2, 0.6, 0, 0.4, 0.2, 0.4, 0),
    3,
    dimnames = list(letters[1:3], letters[1:3])
  )
  ordinal_fit <- quicknet_fit(
    model = "ordinal",
    networks = list(default = correlation),
    meta = list(directed = FALSE)
  )
  correlation_fit <- ordinal_fit
  correlation_fit$model <- "correlation"

  expect_equal(
    quicknet_perturb_precision(ordinal_fit),
    quicknet_perturb_precision(correlation_fit)
  )
})

test_that("panel SEM contemporaneous residual paths become a network layer", {
  residual_paths <- data.frame(
    lhs = c("x_t2", "x_t3"),
    rhs = c("y_t2", "y_t3"),
    est.std = c(0.2, 0.4)
  )
  mat <- quicknet_panel_sem_contemporaneous(
    residual_paths,
    nodes = c("x", "y"),
    waves = 1:3,
    prefix = "_t"
  )

  expect_equal(mat["x", "y"], 0.3)
  expect_equal(mat["y", "x"], 0.3)
})

test_that("graphics devices close even when plotting fails", {
  before <- grDevices::dev.cur()
  expect_error(
    quicknet_plot_to_device(
      filename = tempfile(fileext = ".pdf"),
      device = "pdf",
      width = 4,
      height = 4,
      plot_function = function() stop("plot failed")
    ),
    "plot failed"
  )
  expect_equal(grDevices::dev.cur(), before)
})
