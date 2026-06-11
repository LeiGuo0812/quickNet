test_that("Perturbation supports continuous quicknet_fit objects", {
  fit <- quickNet(mtcars[, 1:5], model = "partial", pie = FALSE, legend = FALSE, DoNotPlot = TRUE)

  dosage <- Perturbation(fit, method = "dosage", targets = c("mpg", "cyl"), dose = c(0.25, 0.50))
  expect_s3_class(dosage, "quicknet_perturbation")
  expect_true(all(c("metrics", "perturbations", "rankings", "report") %in% names(dosage)))
  expect_true(all(c("burden_reduction", "target_reduction", "spillover_reduction") %in% names(dosage$metrics)))
  expect_equal(nrow(dosage$metrics), 4)
  dosage_report <- quicknet_report(dosage)
  expect_s3_class(dosage_report, "quicknet_report")
  expect_true(all(c("settings", "metrics", "rankings", "text") %in% names(dosage_report)))
  expect_s3_class(plot(dosage), "ggplot")
  expect_s3_class(get_perturbation_plot(dosage, type = "rank"), "ggplot")
  expect_s3_class(get_perturbation_plot(dosage, type = "dose_response"), "ggplot")
  expect_s3_class(get_perturbation_plot(dosage, type = "node_change", perturbation_id = 1), "ggplot")

  knockout <- Perturbation(fit, method = "knockout", targets = "mpg")
  expect_s3_class(knockout, "quicknet_perturbation")
  expect_true(any(knockout$metrics$perturbation_type == "precision_edge_vKO"))

  knockdown <- Perturbation(fit, method = "knockdown", targets = "mpg", remaining_strength = 0.50)
  expect_s3_class(knockdown, "quicknet_perturbation")
  expect_true(any(grepl("vKD", knockdown$metrics$perturbation_type)))
  expect_equal(
    sum(knockdown$perturbations$final_state),
    knockdown$metrics$final_burden,
    tolerance = 1e-8
  )

  blocked <- Perturbation(fit, method = "edge_block", targets = "mpg")
  expect_s3_class(blocked, "quicknet_perturbation")
  expect_true(all(c("blocked_edge", "spillover_blocked") %in% names(blocked$metrics)))
  expect_s3_class(get_perturbation_plot(blocked), "ggplot")
  expect_s3_class(get_perturbation_plot(blocked, type = "edge_block"), "ggplot")
  expect_error(get_perturbation_plot(blocked, type = "dose_response"), "requires a dosage perturbation")

  combo <- Perturbation(fit, method = "combination", targets = c("mpg", "cyl", "disp"), combination_size = 2, dose = 0.5)
  expect_s3_class(combo, "quicknet_perturbation")
  expect_true("synergy" %in% names(combo$metrics))
  expect_s3_class(get_perturbation_plot(combo, type = "rank"), "ggplot")

  sequence <- Perturbation(fit, method = "sequence", targets = c("mpg", "cyl", "disp"), steps = 2, dose = 0.5)
  expect_s3_class(sequence, "quicknet_perturbation")
  expect_true(all(c("step", "chosen_node", "incremental_burden_reduction") %in% names(sequence$metrics)))
  expect_equal(nrow(sequence$metrics), 2)
  expect_s3_class(get_perturbation_plot(sequence), "ggplot")
  expect_s3_class(get_perturbation_plot(sequence, type = "sequence"), "ggplot")
})

test_that("Perturbation supports Ising threshold perturbation", {
  set.seed(42)
  binary_data <- data.frame(
    b1 = rbinom(80, 1, 0.5),
    b2 = rbinom(80, 1, 0.45),
    b3 = rbinom(80, 1, 0.55),
    b4 = rbinom(80, 1, 0.5)
  )

  fit <- quickNet(binary_data, model = "ising", pie = FALSE, gamma = 0.25, DoNotPlot = TRUE)
  default_perturbation <- Perturbation(
    fit,
    targets = "b1",
    threshold_shift = -0.5,
    n_samples = 80,
    burnin = 40,
    thinning = 1,
    seed = 9
  )
  expect_equal(default_perturbation$method, "ising_threshold")

  perturbation <- Perturbation(
    fit,
    method = "ising_threshold",
    targets = c("b1", "b2"),
    threshold_shift = -0.5,
    n_samples = 100,
    burnin = 50,
    thinning = 1,
    seed = 10
  )

  expect_s3_class(perturbation, "quicknet_perturbation")
  expect_true(all(c("baseline_activity", "perturbed_activity", "activity_reduction") %in% names(perturbation$metrics)))
  expect_equal(nrow(perturbation$metrics), 2)
  expect_true(nrow(perturbation$perturbations) > 0)
  expect_match(quicknet_report(perturbation)$text, "activity reduction")
  expect_s3_class(plot(perturbation), "ggplot")
  expect_s3_class(get_perturbation_plot(perturbation, type = "node_change", target = "b1"), "ggplot")
})

test_that("Perturbation treats confirmatory Ising fits as Ising models", {
  skip_if_not_installed("psychonetrics")
  set.seed(43)
  binary_data <- data.frame(
    b1 = rbinom(120, 1, 0.5),
    b2 = rbinom(120, 1, 0.45),
    b3 = rbinom(120, 1, 0.55),
    b4 = rbinom(120, 1, 0.5)
  )

  fit <- suppressWarnings(ConfirmatoryNet(binary_data, model = "ising"))
  expect_equal(fit$model, "confirmatory_ising")
  expect_true("threshold" %in% names(fit$nodes))

  perturbation <- Perturbation(
    fit,
    targets = "b1",
    threshold_shift = -0.5,
    n_samples = 80,
    burnin = 40,
    thinning = 1,
    seed = 11
  )

  expect_s3_class(perturbation, "quicknet_perturbation")
  expect_equal(perturbation$method, "ising_threshold")
  expect_equal(perturbation$model, "confirmatory_ising")
  expect_true(all(c("baseline_activity", "perturbed_activity", "activity_reduction") %in% names(perturbation$metrics)))
  expect_false(check_input(model = "perturbation", fit = fit, method = "dosage", quiet = TRUE)$ok)
  expect_error(Perturbation(fit, method = "dosage"), "requires an EBICglasso")
})

test_that("Perturbation rejects unsupported model-method combinations", {
  fit <- quickNet(mtcars[, 1:5], model = "correlation", pie = FALSE, legend = FALSE, DoNotPlot = TRUE)

  expect_error(Perturbation(fit, method = "ising_threshold"), "requires an Ising fit")
})
