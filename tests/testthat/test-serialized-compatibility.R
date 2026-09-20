legacy_fixture <- function(commit) {
  readRDS(test_path("fixtures", paste0("legacy-", commit, ".rds")))
}

legacy_report_value <- function(fit, key) {
  report <- quicknet_report(fit)$estimation
  report$value[match(key, report$parameter)]
}

test_that("real historical RDS objects retain their graphs and qgraph presentation", {
  for (commit in c("29a414b", "72666a3")) {
    fixture <- legacy_fixture(commit)
    expect_match(fixture$manifest$source_commit, paste0("^", commit))
    for (name in names(fixture$fits)) {
      fit <- fixture$fits[[name]]
      expect_s3_class(fit, "quicknet_fit")
      expect_s3_class(summary(fit), "data.frame")
      expect_output(print(fit), "quicknet_fit")
      graph <- fit$graph
      rendered <- suppressWarnings(plot(fit, DoNotPlot = TRUE))
      expect_s3_class(rendered, "qgraph")
      if (name != "clpn") {
        expect_equal(rendered$layout, fit$plots$network$layout)
        expect_equal(rendered$graphAttributes$Nodes$labels, fit$plots$network$graphAttributes$Nodes$labels)
      }
      expect_s3_class(quicknet_report(fit), "quicknet_report")
      expect_identical(fit$graph, graph)
    }
  }
})

test_that("historical metadata is reconciled with actual backend evidence in reports", {
  for (commit in c("29a414b", "72666a3")) {
    fixture <- legacy_fixture(commit)
    f <- fixture$fits$EBICglasso
    expect_identical(f$meta$cor_method, "spearman")
    expect_identical(legacy_report_value(f, "cor_method"), "pearson")
    expect_identical(f$meta$cor_method, "spearman") # Reporting does not mutate the saved object.
    f <- fixture$fits$EBIC_missing_none
    expect_identical(f$meta$missing, "none")
    expect_true(anyNA(f$data))
    expect_identical(legacy_report_value(f, "missing"), "pairwise")
    f <- fixture$fits$mgm
    expect_identical(legacy_report_value(f, "lambdaSel"), "EBIC")
    expect_identical(legacy_report_value(f, "ruleReg"), "OR")
    expect_false("cor_method" %in% quicknet_report(f)$estimation$parameter)
    f <- fixture$fits$ising
    expect_identical(legacy_report_value(f, "AND"), "FALSE")
    expect_false("cor_method" %in% quicknet_report(f)$estimation$parameter)
    f <- fixture$fits$clpn
    expect_identical(legacy_report_value(f, "standardize_data"), "TRUE")
    expect_identical(legacy_report_value(f, "standardize"), "FALSE")
  }
})

test_that("historical estimator defaults and explicit controls survive refitting", {
  skip_if_not_installed("glmnet")
  for (commit in c("29a414b", "72666a3")) {
    fixture <- legacy_fixture(commit)
    backend_matches <- vapply(setdiff(names(fixture$manifest$versions), "R"), function(package) {
      identical(as.character(utils::packageVersion(package)), fixture$manifest$versions[[package]])
    }, logical(1))
    for (name in names(fixture$fits)) {
      fit <- fixture$fits[[name]]
      oracle <- fixture$oracles[[name]]
      if (name == "clpn") {
        m <- fit$meta
        refit <- quicknet_refit_with_backend_args(PanelNet, fit,
          data = oracle$data, nodes = m$nodes, waves = m$waves, id = m$id, prefix = m$prefix,
          standardize = m$standardize, standardize_data = FALSE, alpha = m$alpha,
          lambda_rule = m$lambda_rule, nfolds = m$nfolds, seed = m$seed)
        expect_true(refit$meta$standardize_data)
        expect_false(refit$meta$standardize)
        expect_identical(refit$meta$lambda_rule, "lambda.min")
        expect_equal(refit$meta$alpha, .6)
      } else {
        settings <- quicknet_cross_refit_args(fit)
        expect_identical(quicknet_nct_fit_settings(fit), settings)
        refit <- suppressWarnings(quicknet_refit_like(oracle$data, fit))
        if (fit$model == "EBICglasso") {
          expect_identical(settings$cor_method, "pearson")
          expect_equal(settings$backend_args$nlambda, 100)
          expect_identical(settings$backend_args$sampleSize, "pairwise_average")
          expect_false(settings$backend_args$threshold)
          expect_equal(settings$gamma, .5)
          if (name == "EBIC_missing_none") {
            expect_identical(settings$missing, "pairwise")
            expect_true(anyNA(refit$data))
          }
        } else if (fit$model == "mgm") {
          expect_identical(refit$fit$call$lambdaSel, "EBIC")
          expect_identical(refit$fit$call$ruleReg, "OR")
          expect_equal(refit$fit$call$lambdaGam, if (commit == "29a414b") .5 else .25)
        } else if (fit$model == "ising") {
          expect_false(refit$fit$AND)
          expect_equal(refit$fit$gamma, if (commit == "29a414b") .5 else .25)
        } else {
          expect_true(refit$meta$repair_pd)
          expect_identical(refit$meta$cor_method, "spearman")
        }
      }
      # Numerical equality across quickNet revisions is assessed only with the
      # same backend versions; dependency upgrades can change fitted numbers.
      if (all(backend_matches)) expect_equal(refit$graph, oracle$graph, tolerance = 1e-8)
      expect_true(all(is.finite(refit$graph)))
    }
  }
})

test_that("missing historical correlation evidence is unknown rather than a current default", {
  f <- legacy_fixture("29a414b")$fits$EBICglasso
  f$fit$estimator <- NULL
  expect_identical(legacy_report_value(f, "cor_method"), "unknown (not recorded)")
  expect_error(quicknet_refit_like(f$data, f), "original EBIC correlation estimator is unknown")
})

test_that("saved EBIC defaults and explicit backend evidence have stable precedence", {
  f <- legacy_fixture("29a414b")$fits$EBICglasso
  saved <- quicknet_saved_ebic_args(f)
  # These settings are in the historical bootnet function saved inside the RDS.
  expect_equal(saved[c("corMethod", "missing", "sampleSize", "corArgs", "refit",
                      "principalDirection", "lambda.min.ratio", "nlambda", "threshold",
                      "nonPositiveDefinite", "transform")],
    list(corMethod = "cor", missing = "pairwise", sampleSize = "pairwise_average",
         corArgs = list(), refit = FALSE, principalDirection = FALSE,
         lambda.min.ratio = .01, nlambda = 100, threshold = FALSE,
         nonPositiveDefinite = "stop", transform = "none"))
  expect_false(saved$verbose) # An actual supplied argument overrides saved TRUE.
  expect_false("unlock" %in% names(saved))

  # Exercise provenance disagreement without treating deliberately changed
  # objects as additional real historical fixtures.
  f$meta$backend_args <- list(nlambda = 71, threshold = TRUE, corArgs = list(method = "spearman"))
  f$fit$arguments$nlambda <- 43
  f$fit$arguments["corArgs"] <- list(NULL)
  saved <- quicknet_saved_ebic_args(f)
  expect_equal(saved$nlambda, 43)
  expect_true(saved$threshold)
  expect_true("corArgs" %in% names(saved))
  expect_null(saved$corArgs)
  expect_identical(legacy_report_value(f, "cor_method"), "pearson")
})

test_that("unrecoverable legacy fields are distinguished from resolved settings", {
  f <- legacy_fixture("29a414b")$fits$EBIC_missing_none
  f$fit$estimator <- function(data, corMethod = "cor", nlambda = missing_default) NULL
  expect_null(quicknet_saved_ebic_args(f)$nlambda)
  expect_identical(legacy_report_value(f, "missing"), "unknown (not recorded)")
  expect_error(quicknet_cross_refit_args(f), "original EBIC missing-data rule is unknown")

  f <- legacy_fixture("29a414b")$fits$mgm
  f$fit$call$lambdaSel <- NULL
  f$meta$lambdaSel <- NULL
  expect_identical(legacy_report_value(f, "lambdaSel"), "unknown (not recorded)")
  expect_error(quicknet_cross_refit_args(f), "MGM selection method is unknown")

  f <- legacy_fixture("29a414b")$fits$clpn
  f$fit$glmnet$fits[[1L]]$call$standardize <- quote(standardize_flag)
  expect_identical(legacy_report_value(f, "standardize"), "unknown (not recorded)")
  expect_identical(legacy_report_value(f, "standardize_data"), "TRUE")
  expect_error(quicknet_refit_with_backend_args(PanelNet, f), "original CLPN glmnet standardization setting is unknown")
  f <- legacy_fixture("29a414b")$fits$clpn
  for (i in seq_along(f$fit$glmnet$fits)) f$fit$glmnet$fits[[i]]$call$standardize <- NULL
  expect_error(quicknet_refit_with_backend_args(PanelNet, f), "original CLPN glmnet standardization setting is unknown")
  f <- legacy_fixture("29a414b")$fits$clpn
  f$meta$standardize <- NULL
  expect_identical(legacy_report_value(f, "standardize_data"), "unknown (not recorded)")
  expect_error(quicknet_refit_with_backend_args(PanelNet, f), "original CLPN data standardization setting is unknown")

  f <- legacy_fixture("29a414b")$fits$ising
  f$fit$AND <- NULL
  expect_false(quicknet_cross_refit_args(f)$AND)
  f$meta$AND <- NULL
  expect_identical(legacy_report_value(f, "AND"), "unknown (not recorded)")
  expect_error(quicknet_cross_refit_args(f), "original Ising AND rule is unknown")
})

test_that("new fitted objects retain recorded controls through serialization", {
  data <- legacy_fixture("29a414b")$fits$EBICglasso$data
  fit <- suppressWarnings(quickNet(data, pie = FALSE, DoNotPlot = TRUE, cor_method = "spearman",
    gamma = .17, nlambda = 40, missing = "pairwise"))
  file <- tempfile(fileext = ".rds")
  on.exit(unlink(file), add = TRUE)
  saveRDS(fit, file, version = 3)
  loaded <- readRDS(file)
  expect_identical(loaded$meta$backend_args, fit$meta$backend_args)
  expect_identical(loaded$meta$backend_version, fit$meta$backend_version)
  expect_identical(loaded$graph, fit$graph)
  expect_identical(legacy_report_value(loaded, "cor_method"), "spearman")
  expect_identical(legacy_report_value(loaded, "nlambda"), "40")
  expect_equal(suppressWarnings(quicknet_refit_like(data, loaded))$graph, fit$graph, tolerance = 1e-10)
})
