test_that("all continuous methods integrate with summary, plotting and reports", {
  dat <- read.csv(test_path("fixtures", "symperturb-data.csv"))
  graph <- stats::cor(dat); diag(graph) <- 0
  fit <- quicknet_fit("correlation", data = dat, networks = list(default = graph))
  modules <- c(A = "m1", B = "m1", C = "m2", D = "m3", E = "m3")
  for (method in c("dosage", "knockout", "knockdown", "edge_block", "node_block", "combination", "sequence", "symperturb")) {
    expect_true(check_input(model = "perturbation", fit = fit, method = method, quiet = TRUE)$ok)
    x <- Perturbation(fit, method, modules = modules,
                      config = list(run_robustness_scenarios = FALSE, sequence_length = 2))
    expect_equal(summary(x), x$metrics)
    expect_output(print(x), "SymPerturb")
    report <- quicknet_report(x)
    expect_equal(report$metrics, x$metrics)
    expect_equal(report$rankings, x$rankings)
    expect_match(report$text, "Reference: Zhu")
    expect_match(report$text, "Best target/configuration")
    expect_s3_class(ggplot2::ggplot_build(plot(x)), "ggplot_built")
    expect_s3_class(ggplot2::ggplot_build(get_perturbation_plot(x, "rank")), "ggplot_built")
    if (length(x$moments)) expect_s3_class(ggplot2::ggplot_build(get_perturbation_plot(x, "node_change")), "ggplot_built")
    if (method == "symperturb") {
      expect_equal(report$target_scores, x$target_scores)
      expect_equal(report$pair_scores, x$pair_scores)
      expect_equal(report$sequence, x$sequence)
      expect_s3_class(ggplot2::ggplot_build(get_perturbation_plot(x, "dose_response")), "ggplot_built")
      expect_s3_class(ggplot2::ggplot_build(get_perturbation_plot(x, "sequence")), "ggplot_built")
    }
  }
  empty <- Perturbation(fit, "edge_block", config = list(edge_threshold = 2))
  expect_equal(nrow(empty$metrics), 0L)
  expect_s3_class(ggplot2::ggplot_build(plot(empty)), "ggplot_built")
  expect_s3_class(quicknet_report(empty), "quicknet_report")
  expect_error(get_perturbation_plot(empty, "node_change"), "No node-level")
})

test_that("migration errors and configuration validation are explicit", {
  dat <- read.csv(test_path("fixtures", "symperturb-data.csv"))
  graph <- stats::cor(dat); diag(graph) <- 0
  fit <- quicknet_fit("partial", data = dat, networks = list(default = graph))
  expect_false(check_input(model = "perturbation", fit = fit, method = "typo", quiet = TRUE)$ok)
  expect_error(Perturbation(fit, "combination", dose = .5), "unit dose")
  expect_error(Perturbation(fit, "combination", combination_size = 3), "combination_size = 2")
  expect_error(Perturbation(fit, "edge_block", pulse_values = c(A = .6)), "retired")
  expect_error(Perturbation(fit, "edge_block", edges = data.frame(from = "A", to = "A")), "unique")
  expect_error(Perturbation(fit, "knockdown", dose = .5, remaining_strength = .5), "not both")
  expect_error(Perturbation(fit, dose = 2), "fractions")
  expect_error(Perturbation(fit, "symperturb"), "modules")
  expect_error(Perturbation(fit, config = list(typo = 1)), "config")
  expect_error(Perturbation(fit, config = list(bounds = c(4, 0))), "bounds")
  expect_error(Perturbation(fit, config = list(responsiveness_epsilon = 0)), "positive")
  expect_error(Perturbation(fit, symptom_weights = c(A = -1)), "non-negative")
  expect_error(Perturbation(fit, anchors = c(unknown = 1)), "anchors")
  expect_error(Perturbation(fit, "sequence", steps = 0), "sequence_length")
  bad <- fit; bad$data <- NULL
  expect_false(check_input(model = "perturbation", fit = bad, method = "dosage", quiet = TRUE)$ok)
  expect_error(Perturbation(bad), "fit\\$data")
  bad <- fit; bad$data[1, 1] <- NA_real_
  expect_error(Perturbation(bad), "finite data")
  x <- Perturbation(fit, dose = c(0, .3, 1), threshold = .1,
                    config = list(dose_grid = c(.5, 1), edge_threshold = .2))
  expect_equal(unique(x$metrics$dose), c(0, .3, 1))
  expect_equal(x$network$edge_threshold, .1)
  expect_equal(Perturbation(fit, "knockdown", targets = "A", dose = .5)$metrics,
               Perturbation(fit, "knockdown", targets = "A", remaining_strength = .5)$metrics)
  node_plot <- get_perturbation_plot(x, "node_change", target = "A")
  expect_equal(nrow(node_plot$data), ncol(fit$data))
  expect_equal(length(unique(node_plot$data$perturbation_id)), 1L)
  expect_match(node_plot$labels$subtitle, "SymPerturb Gaussian model")
})
