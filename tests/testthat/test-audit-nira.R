test_that("NIRA validates sampler support before coercing to integer", {
  local_mocked_bindings(
    quicknet_nira_native_samples = function(...) matrix(0.5, 2, 2),
    .package = "quickNet"
  )
  expect_error(quickNet:::quicknet_nira_simulate_condition(
    matrix(0, 2, 2), c(A = -1, B = 1), 2L, "native",
    quickNet:::quicknet_nira_make_streams(101, 1)[[1]]
  ), "0/1 support")
})

test_that("NIRA tied stability ranks do not favour the first node", {
  parameters <- list(
    weight_matrix = matrix(0, 2, 2),
    thresholds = c(A = 1000, B = 1001), beta = 1
  )
  args <- list(
    parameters = parameters, perturbation_type = "alleviating",
    threshold_delta = 1, n_samples = 2L, stability_reps = 100L,
    top_n = 1L, engine = "native",
    stage_stream = quickNet:::quicknet_nira_make_streams(102, 1)[[1]],
    use_parallel = FALSE, ncores = 1L, n_iter = 1L
  )
  result <- do.call(quickNet:::quicknet_nira_run_stability, args)
  expect_true(all(result$condition_means == 2))
  expect_true(all(result$node_summary$top1_proportion > 0.2))
  expect_true(all(result$node_summary$top1_proportion < 0.8))
  expect_identical(result, do.call(quickNet:::quicknet_nira_run_stability, args))
})

test_that("NIRA plot rejects top_n outside the integer range", {
  expect_error(
    quickNet:::quicknet_nira_plot_validate_top_n(.Machine$integer.max + 1),
    "positive integer"
  )
})

test_that("NIRA native sampling matches exact independent probabilities", {
  samples <- quickNet:::quicknet_nira_simulate_condition(
    matrix(0, 3, 3), c(A = -1.2, B = 0.1, C = 0.8),
    20000L, "native", quickNet:::quicknet_nira_make_streams(103, 1)[[1]],
    beta = 1.5, n_iter = 1L
  )
  expected <- stats::plogis(1.5 * c(-1.2, 0.1, 0.8))
  expect_equal(unname(colMeans(samples)), expected, tolerance = 0.015)
  expect_true(all(samples %in% 0:1))
})

test_that("NIRA moderation plot distinguishes unsigned magnitudes", {
  result <- structure(list(
    status = "completed_moderation_not_tested",
    moderation = list(table = data.frame(
      moderator = c("A", "B"),
      moderated_node_1 = c("B", "A"), moderated_node_2 = "C",
      mean_moderation_estimate = c(-0.4, 0.5),
      ci_lower = c(-0.6, 0.2), ci_upper = c(-0.2, 0.8),
      estimate_scale = c("signed", "magnitude"),
      stable_moderation = TRUE
    ))
  ), class = "quicknet_nira")
  plot <- get_nira_plot(result, "moderation")
  expect_match(plot$labels$subtitle, "do not indicate direction")
  expect_match(as.character(plot$data$label[plot$data$estimate == 0.5]),
               "[magnitude]", fixed = TRUE)
  expect_false(grepl("magnitude", as.character(
    plot$data$label[plot$data$estimate == -0.4]
  )))
  expect_s3_class(ggplot2::ggplotGrob(plot), "gtable")
})
