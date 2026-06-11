test_that("MetaNet supports meta-analytic correlation and GGM networks", {
  skip_if_not_installed("psychonetrics")
  set.seed(201)
  vars <- c("x1", "x2", "x3")
  make_cor <- function(n) {
    stats::cor(matrix(rnorm(n * length(vars)), ncol = length(vars), dimnames = list(NULL, vars)))
  }
  cors <- list(make_cor(80), make_cor(100), make_cor(120))
  nobs <- c(80, 100, 120)

  meta_cor <- suppressWarnings(MetaNet(cors = cors, nobs = nobs, vars = vars, model = "meta_cor"))
  expect_s3_class(meta_cor, "quicknet_fit")
  expect_equal(meta_cor$model, "meta_cor")
  expect_equal(meta_cor$meta$n_studies, 3)
  expect_true("studies" %in% names(quicknet_report(meta_cor)$sample))

  meta_ggm <- suppressWarnings(MetaNet(cors = cors, nobs = nobs, vars = vars, model = "meta_ggm"))
  expect_s3_class(meta_ggm, "quicknet_fit")
  expect_equal(meta_ggm$model, "meta_ggm")
  expect_true("randomEffects" %in% quicknet_report(meta_ggm)$estimation$parameter)
})

test_that("MetaNet supports meta-analytic GVAR from multi-study raw data", {
  skip_if_not_installed("psychonetrics")
  set.seed(202)
  make_study <- function(study, persons = 8, times = 5) {
    ids <- rep(seq_len(persons), each = times)
    beep <- rep(seq_len(times), times = persons)
    x1 <- rnorm(length(ids))
    x2 <- rnorm(length(ids))
    for (person in seq_len(persons)) {
      idx <- which(ids == person)
      for (t in idx[-1]) {
        previous <- t - 1
        x1[t] <- 0.25 * x1[previous] + rnorm(1)
        x2[t] <- 0.20 * x2[previous] + 0.10 * x1[previous] + rnorm(1)
      }
    }
    data.frame(study = study, id = paste(study, ids, sep = "_"), day = 1, beep = beep, x1 = x1, x2 = x2)
  }
  dat <- rbind(make_study("s1"), make_study("s2"), make_study("s3"))

  fit <- suppressWarnings(MetaNet(data = dat, studyvar = "study", vars = c("x1", "x2"), model = "meta_gvar"))

  expect_s3_class(fit, "quicknet_fit")
  expect_equal(fit$model, "meta_gvar")
  expect_true(all(c("temporal", "contemporaneous") %in% names(fit$networks)))
  expect_true(fit$network_summary$directed[fit$network_summary$network == "temporal"])
  expect_equal(quicknet_report(fit)$sample$studies, 3)
})
