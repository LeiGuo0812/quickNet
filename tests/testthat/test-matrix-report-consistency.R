test_that("centrality and bridge statistics use fitted weights despite display thresholds", {
  nm <- c("Z", "A", "C")
  w <- matrix(c(0, .7, .2, .7, 0, -.3, .2, -.3, 0), 3,
              dimnames = list(nm, nm))
  cached <- qgraph::qgraph(w, threshold = .5, DoNotPlot = TRUE)
  fit <- quicknet_fit("correlation", networks = list(default = w), plots = list(network = cached))
  centrality <- Centrality(fit, print = FALSE)
  expect_equal(unname(centrality$centrality_data$OutDegree), c(.9, 1, .5))
  expect_equal(unname(centrality$centrality_data$OutExpectedInfluence), c(.9, .4, -.1))
  expect_equal(centrality$node_table$strength, c(.9, 1, .5))
  expect_equal(quicknet_report(fit)$nodes, centrality$node_table)
  bridge <- Bridge(fit, communities = c(1, 1, 2), include = "all")
  expect_equal(bridge$bridge_data, networktools::bridge(w, communities = c(1, 1, 2)))
  expect_equal(unname(bridge$bridge_data$`Bridge Strength`), c(.2, .3, .5))
  expect_equal(globalCoeff(fit)$globalStrength, 1.2)
  # Display threshold remains a plotting choice, while the table retains all edges.
  expect_equal(sum(abs(qgraph::getWmat(plot(fit, DoNotPlot = TRUE))) > 0), 2)
  expect_equal(sum(abs(fit$graph) > 0), 6)
})

test_that("directed signs, self loops and orientation agree with qgraph and hand sums", {
  w <- matrix(c(.1, .2, -.3, .4, .5, 0, 0, -.6, .7), 3,
              dimnames = list(c("Z", "A", "C"), c("Z", "A", "C")))
  fit <- quicknet_fit("clpn", networks = list(default = w))
  graph <- qgraph::qgraph(t(w), directed = TRUE, DoNotPlot = TRUE)
  actual <- Centrality(fit, print = FALSE)
  native <- qgraph::centrality(graph)
  expect_equal(actual$centrality_data[names(native)], native)
  expect_equal(unname(native$InDegree), c(.4, .8, .3))
  expect_equal(unname(native$OutDegree), c(.5, .4, .6))
  expect_equal(actual$node_table$autoregressive, c(.1, .5, .7))
  expect_equal(actual$node_table$in_strength, c(.4, .8, .3))
  expect_equal(get_edges_df(fit), get_edges_df(graph))
  expect_equal(get_edges_df(w), get_edges_df(fit))
  expect_error(get_edges_df(fit, w * 0), "same directed/undirected")
  expect_error(get_edges(fit, w * 0), "same directed/undirected")
  edges <- quicknet_edge_table(w, directed = TRUE, include_diag = TRUE)
  expect_equal(edges$weight, w[cbind(match(edges$to, rownames(w)), match(edges$from, colnames(w)))])
  expect_equal(globalCoeff(fit)$globalStrength, 1.5)
  expect_equal(globalCoeff(fit)$AGS, .25)
  expect_equal(quicknet_report(fit)$networks$nonzero_edges, 4)
})

test_that("matrix extraction aligns independent axes and exported tables retain raw values", {
  nm <- c("Z", "A", "C")
  w <- matrix(c(0, .2, .3, .2, 0, .4, .3, .4, 0), 3, dimnames = list(nm, nm))
  expect_equal(quicknet_network_matrix(w[3:1, ]), w)
  expect_equal(get_edges_df(w, w[c(3, 1, 2), c(2, 3, 1)], "union"), get_edges_df(w))
  fit <- quicknet_fit("correlation", networks = list(default = w))
  directory <- tempfile(); dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  get_network_plot(fit, path = directory)
  exported <- as.matrix(read.csv(file.path(directory, "network_matrix.csv"), row.names = 1, check.names = FALSE))
  expect_equal(exported, w)
  for (matrix in list(w, w * 0)) {
    centrality <- suppressWarnings(Centrality(matrix, print = FALSE))
    expect_no_error(get_centrality_plot(centrality, path = directory))
    table <- read.csv(file.path(directory, "centrality_table.csv"))
    expect_equal(table$Nodes, nm)
    expect_equal(table$OutDegree, unname(centrality$centrality_data$OutDegree))
  }
})

test_that("MetaNet aligns study sample sizes and both axes before native estimation", {
  skip_if_not_installed("psychonetrics")
  set.seed(437)
  vars <- c("A", "B", "C")
  ns <- c(first = 80, second = 120, third = 160)
  cors <- lapply(ns, function(n) {
    z <- rnorm(n)
    cor(cbind(A = z + rnorm(n), B = .5 * z + rnorm(n), C = -.4 * z + rnorm(n)))
  })
  shuffled <- cors
  shuffled[[2]] <- cors[[2]][c(3, 1, 2), c(2, 3, 1)]
  shuffled[[3]] <- cors[[3]][3:1, 3:1]
  aligned <- quicknet_meta_align_inputs(shuffled, NULL, ns[3:1], vars, "meta_cor")
  expect_equal(aligned$cors, cors)
  expect_equal(aligned$nobs, ns)
  for (model in c("meta_cor", "meta_ggm")) {
    actual <- suppressWarnings(MetaNet(cors = shuffled, nobs = ns[3:1], vars = vars, model = model))
    native <- psychonetrics::meta_varcov(cors = cors, nobs = ns, vars = vars,
      type = if (model == "meta_cor") "cor" else "ggm", randomEffects = "chol", estimator = "FIML", verbose = FALSE)
    native <- suppressWarnings(quicknet_psychonetrics_run(native))
    expected <- psychonetrics::getmatrix(native, if (model == "meta_cor") "rho_y" else "omega_y")
    diag(expected) <- 0
    expect_equal(unname(actual$graph), unname(expected), tolerance = 1e-7)
    expect_equal(actual$meta$nobs, ns)
  }
  expect_error(quicknet_meta_align_inputs(cors, NULL, setNames(ns, c("x", "y", "z")), vars, "meta_cor"), "study matrix names")
})

test_that("Meta-GVAR labels preserve past and present blocks", {
  vars <- c("A", "B")
  labels <- c("A_lag1", "B_lag1", "A", "B")
  w <- matrix(c(1, .2, .3, .1, .2, 1, -.1, .4, .3, -.1, 1, .2, .1, .4, .2, 1), 4,
              dimnames = list(labels, labels))
  covs <- list(w, w[c(2, 1, 4, 3), c(1, 2, 4, 3)])
  expect_equal(quicknet_meta_infer_vars(NULL, NULL, covs, NULL, NULL, NULL, NULL, NULL, "meta_gvar"), vars)
  expect_equal(quicknet_meta_align_inputs(NULL, covs, c(100, 100), vars, "meta_gvar")$covs, list(w, w))
  wrong_time_order <- w[c(3, 4, 1, 2), c(3, 4, 1, 2)]
  expect_error(quicknet_meta_align_inputs(NULL, list(wrong_time_order, wrong_time_order), c(100, 100), vars, "meta_gvar"), "past.*followed by current")
  literal_vars <- c("anxiety_lag1", "mood")
  literal_labels <- c(paste0(literal_vars, "_lag1"), literal_vars)
  dimnames(w) <- list(literal_labels, literal_labels)
  expect_equal(quicknet_meta_align_inputs(NULL, list(w, w), c(100, 100), literal_vars, "meta_gvar")$covs, list(w, w))
})
