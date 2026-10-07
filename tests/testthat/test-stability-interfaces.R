test_that("Stability rejects unsupported fit families before resampling", {
  graph <- matrix(c(0, .3, .3, 0), 2,
    dimnames = list(c("x", "y"), c("x", "y")))
  local_mocked_bindings(quicknet_bootstrap_edge_stability = function(...) {
    stop("Unsupported models must not start resampling")
  })
  for (model in c("confirmatory_ggm", "confirmatory_covariance", "latent_network",
                  "lvm", "panel_sem", "clpn", "mixedVAR", "meta_cor")) {
    fit <- quicknet_fit(model, networks = list(default = graph))
    expect_error(Stability(fit, nboot = 1), "supports only exploratory cross-sectional")
  }
  expect_error(Stability(mtcars[, 1:3], model = "confirmatory_ggm", nboot = 1),
               "supports only exploratory cross-sectional")
  expect_error(Stability(mtcars[, 1:3], model = "corr", nboot = 1),
               "Unsupported models must not start resampling")
})

test_that("custom stability results export both existing tables with raw values", {
  fit <- quickNet(mtcars[, 1:4], model = "correlation", pie = FALSE,
                  DoNotPlot = TRUE)
  set.seed(90723)
  stability <- Stability(fit, nboot = 3, case.drop = .1)
  output <- tempfile("quicknet-stability-")
  dir.create(output)
  on.exit(unlink(output, recursive = TRUE), add = TRUE)
  expect_null(get_stability_plot(stability, path = output, prefix = "cor"))
  expect_setequal(list.files(output), c("cor_edge_bootstrap_stability_table.csv",
                                       "cor_case_drop_centrality_stability_table.csv"))
  edges <- read.csv(file.path(output, "cor_edge_bootstrap_stability_table.csv"))
  case_drop <- read.csv(file.path(output, "cor_case_drop_centrality_stability_table.csv"))
  expect_identical(names(edges), names(stability$edge_bootstrap_stability))
  expect_equal(edges$original_weight, stability$edge_bootstrap_stability$original_weight,
               tolerance = 1e-14)
  expect_equal(edges$ci_lower, stability$edge_bootstrap_stability$ci_lower,
               tolerance = 1e-14)
  expect_equal(case_drop$median_correlation,
               stability$case_drop_centrality_stability$median_correlation, tolerance = 1e-14)
  expect_equal(case_drop$failed_reps, stability$case_drop_centrality_stability$failed_reps)
  expect_false(any(grepl("CS_coefficient", list.files(output))))
  expect_error(get_stability_plot(stability, path = output, get.table = FALSE),
               "get.table = TRUE")
})

test_that("native stability plots and CS exports keep their existing filenames", {
  plot <- ggplot2::ggplot(data.frame(x = 1:2, y = 1:2), ggplot2::aes(x, y)) +
    ggplot2::geom_point()
  stability <- list(edge_weight_CI_plot = plot, centrality_stability_plot = plot,
    CS_coefficient = c(strength = .5),
    edge_bootstrap_stability = data.frame(node_i = "x", node_j = "y",
                                        original_weight = .123456789))
  output <- tempfile("quicknet-native-stability-")
  dir.create(output)
  on.exit(unlink(output, recursive = TRUE), add = TRUE)
  get_stability_plot(stability, path = output, prefix = "native_", device = "pdf")
  expect_setequal(list.files(output), c("native_edge_weight_CI_plot.pdf",
    "native_centrality_stability_plot.pdf", "native_CS_coefficient_table.csv",
    "native_edge_bootstrap_stability_table.csv"))
  cs <- read.csv(file.path(output, "native_CS_coefficient_table.csv"),
                 check.names = FALSE)
  expect_equal(cs$Measure, "strength")
  expect_equal(cs$`CS-coefficient`, .5)
  expect_true(all(file.info(list.files(output, full.names = TRUE))$size > 0))
  expect_error(get_stability_plot(list(), path = output), "No stability plots")
  expect_error(get_stability_plot(stability, path = output, get.table = NA), "TRUE or FALSE")
})
