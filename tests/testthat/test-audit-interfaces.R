test_that("edge labels remain correct for every row", {
  mat <- matrix(c(0, .2, .3, .2, 0, .4, .3, .4, 0), 3)
  indices <- get_edges_df(mat)
  labels <- c("Alpha", "Beta", "Gamma")
  result <- get_edges_df(mat, labels = labels)
  expect_equal(result$from, labels[indices$from])
  expect_equal(result$to, labels[indices$to])
  expect_equal(result$weight, indices$weight)
})

test_that("native qgraph extraction preserves weights and directed orientation", {
  nm <- c("a", "b", "c")
  mat <- matrix(c(0, .2, .3, .2, 0, .4, .3, .4, 0), 3, dimnames = list(nm, nm))
  graph <- qgraph::qgraph(mat, labels = nm, DoNotPlot = TRUE)
  expect_equal(quicknet_network_matrix(graph), mat)
  expect_equal(get_edges_df(graph), get_edges_df(mat))
  expect_equal(globalCoeff(graph), globalCoeff(mat))
  directed <- mat
  directed[upper.tri(directed)] <- 0
  graph <- qgraph::qgraph(t(directed), directed = TRUE, labels = nm, DoNotPlot = TRUE)
  fit <- quicknet_fit("clpn", networks = list(default = directed))
  expect_equal(quicknet_network_matrix(graph), directed)
  expect_equal(get_edges_df(graph), get_edges_df(fit))
  expect_equal(length(get_edges(graph)), 3L)
  expect_equal(globalCoeff(graph)$globalStrength, sum(abs(directed)))
})

test_that("constant centrality gives finite named node sizes", {
  mat <- matrix(0, 3, 3, dimnames = list(letters[1:3], letters[1:3]))
  centrality <- suppressWarnings(Centrality(mat, print = FALSE))
  expect_equal(unname(centrality$centrality_data$InDegreeScale), rep(0, 3))
  sizes <- get_strength_node_size(centrality)
  expect_true(all(is.finite(sizes) & sizes > 0))
  expect_named(sizes, letters[1:3])
})

test_that("centrality plots retain negative influence and bridge plots label nodes", {
  mat <- matrix(c(0, -.4, -.3, -.4, 0, -.2, -.3, -.2, 0), 3,
    dimnames = list(letters[1:3], letters[1:3]))
  result <- Centrality(mat, include = "ExpectedInfluence", print = FALSE)
  plotted <- ggplot2::ggplot_build(result$centralityPlot)$data
  expect_true(any(vapply(plotted, function(layer) any(layer$x < 0, na.rm = TRUE), logical(1))))
  bridge <- Bridge(abs(mat), communities = c("A", "B", "B"), normalize = FALSE)
  built <- ggplot2::ggplot_build(bridge$bridgePlot)
  expect_equal(built$layout$panel_params[[1]]$y$get_labels(), letters[1:3])
  expect_false(inherits(bridge$bridgePlot$coordinates, "CoordFlip"))
})

test_that("bridge groups preserve singleton and numbered community names", {
  bridge <- list(bridge_data = list("Bridge Strength" = c(a = 1, b = 2, c = 3)))
  groups <- list(Group1 = 1L, Group2 = 2:3)
  expect_equal(as.character(bridgeGroup(bridge, groups, n = 0)), c("Group1", "Group2", "Group2"))
  expect_equal(as.character(bridgeGroup(bridge, groups, n = 1)), c("Bridge", "Group2", "Bridge"))
  expect_error(bridgeGroup(bridge, list(A = 1:2, B = 2:3)), "exactly once")
  expect_error(bridgeGroup(bridge, c("A", "B")), "one non-missing")
})

test_that("fit construction rejects invalid matrices and aligns node names", {
  expect_error(quicknet_fit("correlation", networks = list()), "named list")
  expect_error(quicknet_fit("correlation", networks = list(default = matrix(1, 2, 3))), "square")
  expect_error(quicknet_fit("correlation", networks = list(default = matrix(NA_real_, 2, 2))), "finite")
  mat <- matrix(c(0, .3, .3, 0), 2, dimnames = list(c("a", "b"), c("a", "b")))
  fit <- quicknet_fit("correlation", networks = list(custom = mat[2:1, ]))
  expect_equal(fit$graph, mat)
  expect_equal(quicknet_network_matrix(fit), mat)
  expect_s3_class(plot(fit, DoNotPlot = TRUE), "qgraph")
  multilayer <- quicknet_fit("correlation", networks = list(other = mat * 0, default = mat))
  expect_equal(multilayer$graph, quicknet_network_matrix(multilayer))
  expect_equal(multilayer$graph, mat)
})

test_that("replotting a fit preserves visual settings and permits overrides", {
  fit <- quickNet(mtcars[, 1:4], model = "correlation", pie = FALSE,
    labels = LETTERS[1:4], color = "tomato", layout = "circle", DoNotPlot = TRUE)
  graph <- plot(fit, DoNotPlot = TRUE)
  expect_equal(graph$graphAttributes$Nodes$labels, LETTERS[1:4])
  expect_equal(graph$graphAttributes$Nodes$color, fit$plots$network$graphAttributes$Nodes$color)
  expect_equal(graph$layout, fit$plots$network$layout)
  changed <- plot(fit, color = "navy", DoNotPlot = TRUE)
  expect_false(identical(changed$graphAttributes$Nodes$color, graph$graphAttributes$Nodes$color))
})

test_that("network comparisons align node names before computing or plotting", {
  nm <- c("a", "b", "c")
  mat <- matrix(c(0, .2, 0, .2, 0, .4, 0, .4, 0), 3, dimnames = list(nm, nm))
  reordered <- mat[3:1, 3:1]
  expect_equal(get_edges(mat, reordered, "intersect"), get_edges(mat))
  expect_equal(get_edges_df(mat, reordered, "union"), get_edges_df(mat))
  expect_equal(netCor(mat, reordered, nperm = 10, graph = FALSE)$z.stat,
    sum(mat[lower.tri(mat)]^2))
  other <- reordered
  dimnames(other) <- list(LETTERS[1:3], LETTERS[1:3])
  expect_error(get_edges(mat, other), "same unique node names")
  fit <- quicknet_fit("correlation", networks = list(default = mat),
    plots = list(network = qgraph::qgraph(mat, labels = nm, DoNotPlot = TRUE)))
  comparison <- list(diff_sig = reordered, "diff_sig_nw1>nw2" = reordered,
    "diff_sig_nw1<nw2" = reordered * 0)
  grDevices::pdf(tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)
  plots <- get_compare_plot(comparison, fit, output = FALSE)
  expect_equal(unname(qgraph::getWmat(plots$diff_plot)), unname(mat))
})

test_that("report backend names do not partially match backend arguments", {
  fit <- quicknet_fit("mlVAR", networks = list(default = diag(2)),
    meta = list(backend_args = list()))
  report <- quicknet_report(fit)
  expect_equal(report$estimation$value[report$estimation$parameter == "backend"], "mlVAR::mlVAR")
  fit$meta$backend_args <- list(verbose = FALSE, nLambda = 10)
  expect_equal(quicknet_report(fit)$estimation, report$estimation)
})
