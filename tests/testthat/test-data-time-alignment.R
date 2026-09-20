test_that("longitudinal occasion indices distinguish inferred order from duplicate times", {
  set.seed(4601)
  d <- data.frame(id = rep(c("person-b", "person-a"), each = 12),
    day = rep(rep(1:2, each = 6), 2), beep = rep(1:6, 4), a = rnorm(24), b = rnorm(24))
  expect_true(check_input(d, "graphicalVAR", vars = c("a", "b"), day = "day", beep = NULL, quiet = TRUE)$ok)
  expect_true(check_input(d, "graphicalVAR", vars = c("a", "b"), quiet = TRUE)$ok)
  bad <- rbind(d, d[1, ])
  expect_false(check_input(bad, "graphicalVAR", vars = c("a", "b"), day = "day", beep = "beep", quiet = TRUE)$ok)
  for (value in list(NA_real_, Inf, 1.5)) {
    bad <- d; bad$beep[1] <- value
    expect_false(check_input(bad, "graphicalVAR", vars = c("a", "b"), day = "day", beep = "beep", quiet = TRUE)$ok)
  }
  expect_false(check_input(d, "graphicalVAR", vars = c("a", "a"), day = "day", beep = "beep", quiet = TRUE)$ok)
  expect_false(check_input(d, "graphicalVAR", vars = c("a", "b"), id = c("id", "day"), quiet = TRUE)$ok)
  expect_false(check_input(d, "graphicalVAR", vars = c("a", "b"), beep = c("beep", "day"), quiet = TRUE)$ok)
})

test_that("lag indices never connect subjects or days and retain missed-occasion gaps", {
  d <- data.frame(id = c("乙", "乙", "乙", "乙", "甲", "甲"),
    day = c(1, 1, 1, 2, 1, 1), beep = c(1, 3, 4, 1, 1, 2),
    a = 1:6, b = 11:16)
  index <- quicknet_longitudinal_lag_index(d, c("a", "b"), "id", "day", "beep", c(1, 2))
  expect_equal(index$predecessor[index$lag == 1], c(NA, NA, 2, NA, NA, 5))
  expect_equal(index$predecessor[index$lag == 2], c(NA, 1, NA, NA, NA, NA))
  d$a[2] <- NA
  index <- quicknet_longitudinal_lag_index(d, c("a", "b"), "id", "day", "beep", 1)
  expect_false(index$complete[3])
  inferred <- quicknet_longitudinal_lag_index(d, c("a", "b"), "id", "day", NULL, 1)
  expect_equal(inferred$predecessor, c(NA, 1, 2, NA, NA, 5))
})

test_that("panel identifiers and complete-case design rows are traceable", {
  d <- data.frame(id = c("b", "a", "c", "d"), a_t1 = 1:4, b_t1 = 2:5,
    a_t2 = c(2, NA, 4, 5), b_t2 = 3:6)
  design <- quicknet_clpn_design(d, c("a", "b"), 1:2, standardize = FALSE)
  expect_equal(design$retained_rows, c(1L, 3L, 4L))
  expect_equal(design$dropped_rows, 2L)
  expect_equal(design$meta$input_row, c(1L, 3L, 4L))
  expect_equal(design$meta$id, c("b", "c", "d"))
  bad <- d; bad$id[1] <- NA
  expect_false(check_input(bad, "clpn", nodes = c("a", "b"), waves = 1:2, quiet = TRUE)$ok)
  bad <- d; bad$id[1] <- "c"
  expect_false(check_input(bad, "clpn", nodes = c("a", "b"), waves = 1:2, quiet = TRUE)$ok)
})

test_that("native MGM lag inclusion and weighted sample counts are retained", {
  set.seed(4602)
  d <- as.data.frame(matrix(rnorm(360), 120, 3))
  day <- rep(1:4, each = 30)
  beep <- rep(1:30, 4)
  keep <- !seq_len(nrow(d)) %in% c(8, 46)
  d <- d[keep, ]; day <- day[keep]; beep <- beep[keep]
  f <- MixedVARNet(d, types = rep("g", 3), levels = rep(1, 3), lags = c(1, 2),
    beepvar = beep, dayvar = day, regularize = FALSE, threshold = "none", signInfo = FALSE)
  included <- f$fit$call$data_lagged$included
  expect_equal(f$meta$analysis_sample$temporal_rows, sum(included))
  expect_equal(f$meta$analysis_sample$positive_weight_rows, sum(f$fit$call$weights_design > 0))
  expect_equal(f$meta$lag_index$included[f$meta$lag_index$lag == 2], included)
  expect_identical(names(f$networks), c("default", "temporal", "lag_1", "lag_2"))
})


test_that("unavailable native model counts remain unknown without breaking diagnostics", {
  d <- data.frame(id = 1, day = 1, beep = 1:6, a = 1:6, b = c(2, 1, 4, 3, 6, 5))
  index <- quicknet_longitudinal_lag_index(d, c("a", "b"), "id", "day", "beep", 1)
  # importMplus returns MplusAutomation tables, which have no nobs method.
  unavailable <- list(output = list(summaries = data.frame(NObservations = 6),
    parameters = list(unstandardized = data.frame(est = c(0.2, 0.3)))))
  sample <- quicknet_longitudinal_sample(d, c("a", "b"), "id", index, unavailable, "mlVAR")
  expect_true(is.na(sample$temporal_rows))
  expect_true(all(is.na(sample$temporal_rows_by_node$observations)))
  expect_true(all(is.na(sample$temporal_subjects_by_node$subjects)))
  expect_equal(sample$complete_lag_rows, 5)
})
