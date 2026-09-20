test_that("psychonetrics centering follows the available backend controls", {
  old_backend <- function(data, standardize = "none") NULL
  current_backend <- function(data, centerWithin = FALSE) NULL
  changed_default <- function(data, centerWithin = TRUE) NULL
  expect_identical(quicknet_psychonetrics_center_args(NULL, old_backend), list())
  expect_identical(quicknet_psychonetrics_center_args(FALSE, old_backend), list())
  expect_error(quicknet_psychonetrics_center_args(TRUE, old_backend), "does not support")
  expect_identical(quicknet_psychonetrics_center_args(NULL, current_backend), list(centerWithin = FALSE))
  expect_identical(quicknet_psychonetrics_center_args(TRUE, current_backend), list(centerWithin = TRUE))
  expect_identical(quicknet_psychonetrics_center_args(NULL, changed_default), list(centerWithin = TRUE))
  expect_error(quicknet_psychonetrics_center_args(NA, current_backend), "single logical")
  expect_error(quicknet_psychonetrics_center_args(c(TRUE, FALSE), current_backend), "single logical")
})
