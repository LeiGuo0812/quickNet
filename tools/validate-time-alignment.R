#!/usr/bin/env Rscript
# Run from package root: Rscript --vanilla tools/validate-time-alignment.R [output]
args <- commandArgs(trailingOnly = TRUE)
output <- if (length(args)) args[[1L]] else "../output/audit/time-alignment"
dir.create(output, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1")
pkgload::load_all(".", quiet = TRUE)
set.seed(46510)
vars <- c("症状甲", "症状乙", "症状丙")
blocks <- lapply(1:8, function(id) do.call(rbind, lapply(1:3, function(day) {
  z <- matrix(0, 85, 3)
  for (i in 2:85) z[i, ] <- c(.2 * z[i - 1, 1],
    .7 * z[i - 1, 1] - .15 * z[i - 1, 2], -.25 * z[i - 1, 3]) + rnorm(3)
  d <- data.frame(id = id, day = day, beep = 1:45, z[41:85, ], check.names = FALSE)
  names(d)[4:6] <- vars
  d
})))
complete <- do.call(rbind, blocks)
d <- complete[!(complete$beep %in% c(8, 29) & complete$id %% 2 == 0), ]
d[d$beep == 17 & d$id %% 3 == 0, vars[[1]]] <- NA_real_
rownames(d) <- NULL
set.seed(46511)
shuffled <- d[sample.int(nrow(d)), c(vars[[3]], "beep", vars[[1]], "id", vars[[2]], "day")]
native_args <- list(data = d, vars = vars, idvar = "id", dayvar = "day", beepvar = "beep")
wrapped_args <- list(data = d, vars = vars, id = "id", day = "day", beep = "beep")
results <- list(); fitted <- list()
equal <- function(a, b, label, tolerance = 1e-8) {
  if (!isTRUE(all.equal(unname(a), unname(b), tolerance = tolerance, check.attributes = FALSE)))
    stop(label, call. = FALSE)
  max(abs(a - b), na.rm = TRUE)
}
assert <- function(value, message) if (!isTRUE(value)) stop(message, call. = FALSE)

# An independent endpoint lookup uses explicit id/day/occasion comparisons.
index <- function(lags) {
  predecessor <- sapply(lags, function(lag) vapply(seq_len(nrow(d)), function(i) {
    found <- which(d$id == d$id[i] & d$day == d$day[i] & d$beep == d$beep[i] - lag)
    if (length(found)) found[[1L]] else NA_integer_
  }, integer(1)))
  predecessor <- matrix(predecessor, ncol = length(lags))
  ok <- complete.cases(d[, vars]) & apply(!is.na(predecessor), 1, all)
  for (j in seq_along(lags)) {
    found <- which(!is.na(predecessor[, j]))
    ok[found] <- ok[found] & complete.cases(d[predecessor[found, j], vars])
  }
  list(predecessor = predecessor, included = ok)
}

# Native graphicalVAR data construction and direction (PDC is source by target).
g_args <- c(native_args, list(scale = FALSE, centerWithin = FALSE, subjectNetworks = FALSE,
                              nLambda = 5, verbose = FALSE))
native <- do.call(graphicalVAR::mlGraphicalVAR, g_args)
fit <- do.call(LongitudinalNet, c(wrapped_args, list(scale = FALSE, centerWithin = FALSE,
                       subjectNetworks = FALSE, nLambda = 5)))
shuffled_args <- wrapped_args; shuffled_args$data <- shuffled
other <- do.call(LongitudinalNet, c(shuffled_args, list(scale = FALSE, centerWithin = FALSE,
                       subjectNetworks = FALSE, nLambda = 5)))
expected <- index(1)
prep <- do.call(get("tsData", asNamespace("graphicalVAR")), c(native_args,
               list(scale = FALSE, centerWithin = FALSE)))
center <- colMeans(d[, vars], na.rm = TRUE)
current <- sweep(as.matrix(d[expected$included, vars]), 2, center, "-")
lagged <- sweep(as.matrix(d[expected$predecessor[expected$included, 1], vars]), 2, center, "-")
equal(as.matrix(prep$data_c), current, "graphicalVAR current-row pairing differs")
equal(as.matrix(prep$data_l[, -1]), lagged, "graphicalVAR predecessor pairing differs")
assert(fit$meta$analysis_sample$temporal_rows == sum(expected$included), "graphicalVAR sample count differs")
results$graphicalVAR <- list(max_difference = equal(fit$networks$temporal, t(native$fixedPDC), "graphicalVAR direction differs"),
  shuffled_difference = equal(fit$networks$temporal, other$networks$temporal, "graphicalVAR row/column shuffle differs"),
  input_rows = nrow(d), temporal_rows = sum(expected$included))
assert(fit$networks$temporal[vars[[2]], vars[[1]]] > .2, "Known A to B signal missing")
fitted$graphicalVAR <- fit
message("graphicalVAR pairing, direction, reordering: PASS")

# Native mlVAR augments missed occasions and requires every requested lag.
native <- do.call(mlVAR::mlVAR, c(native_args, list(lags = c(1, 2), scale = FALSE,
                  temporal = "fixed", contemporaneous = "fixed", verbose = FALSE)))
fit <- do.call(LongitudinalNet, c(wrapped_args, list(model = "mlVAR", lags = c(1, 2),
                  scale = FALSE, temporal = "fixed", contemporaneous = "fixed")))
other <- do.call(LongitudinalNet, c(shuffled_args, list(model = "mlVAR", lags = c(1, 2),
                  scale = FALSE, temporal = "fixed", contemporaneous = "fixed")))
expected <- index(c(1, 2))
assert(nrow(native$data) == sum(expected$included), "mlVAR native usable-row count differs")
equal(as.matrix(native$data[, c("id", "day", "beep")]),
      as.matrix(d[expected$included, c("id", "day", "beep")]), "mlVAR retained occasions differ")
differences <- vapply(1:2, function(lag) {
  expected_network <- t(mlVAR::getNet(native, type = "temporal", lag = lag, nonsig = "show"))
  equal(fit$networks[[paste0("temporal_lag_", lag)]], expected_network, "mlVAR lag direction differs")
}, numeric(1))
results$mlVAR <- list(max_difference = max(differences),
  shuffled_difference = equal(fit$graph, other$graph, "mlVAR reordering differs"),
  input_rows = nrow(d), temporal_rows = sum(expected$included),
  node_counts = fit$meta$analysis_sample$temporal_rows_by_node)
assert(all(fit$meta$analysis_sample$temporal_rows_by_node$observations == sum(expected$included)), "mlVAR model nobs differs")
assert(fit$networks$temporal_lag_1[vars[[2]], vars[[1]]] > .2, "Known mlVAR A to B signal missing")
fitted$mlVAR <- fit
message("mlVAR pairing, multiple lags, direction, reordering: PASS")

# psychonetrics keeps partial lag vectors for its native FIML rule.
native <- psychonetrics::runmodel(do.call(psychonetrics::gvar, c(native_args,
                  list(standardize = "none", centerWithin = FALSE, verbose = FALSE))))
fit <- do.call(LongitudinalNet, c(wrapped_args, list(model = "psychonetrics_gvar")))
other <- do.call(LongitudinalNet, c(shuffled_args, list(model = "psychonetrics_gvar")))
prep <- do.call(get("tsData", asNamespace("psychonetrics")), c(native_args, list(includeID = TRUE)))
expected <- index(1)
lagged <- as.matrix(d[expected$predecessor[, 1], vars])
assert(identical(unname(is.na(as.matrix(prep[, paste0(vars, "_lag1")]))), unname(is.na(lagged))), "psychonetrics missing lag pattern differs")
equal(as.matrix(prep[, paste0(vars, "_lag1")]), lagged, "psychonetrics lag pairing differs")
results$psychonetrics_gvar <- list(max_difference = equal(fit$graph, psychonetrics::getmatrix(native, "beta"), "psychonetrics direction differs", 1e-6),
  shuffled_difference = equal(fit$graph, other$graph, "psychonetrics reordering differs", 1e-6),
  backend_nobs = native@sample@groups$nobs, complete_lag_rows = sum(expected$included),
  recorded = fit$meta$analysis_sample)
assert(fit$meta$analysis_sample$temporal_rows == sum(native@sample@groups$nobs), "psychonetrics sample count differs")
fitted$psychonetrics_gvar <- fit
message("psychonetrics pairing, FIML sample count, direction, reordering: PASS")

# MGM has a row-offset/consecutiveness contract and does not insert missing rows.
dyn <- complete[complete$id == 1 & !complete$beep %in% c(8, 29), ]
mgm_args <- list(data = as.matrix(dyn[, vars]), type = rep("g", 3), level = rep(1, 3),
  lags = c(1, 2), beepvar = dyn$beep, dayvar = dyn$day, regularize = FALSE,
  threshold = "none", scale = FALSE, signInfo = FALSE, pbar = FALSE)
native <- do.call(mgm::mvar, mgm_args)
fit <- MixedVARNet(dyn, vars = vars, types = rep("g", 3), levels = rep(1, 3),
  lags = c(1, 2), beepvar = dyn$beep, dayvar = dyn$day, regularize = FALSE,
  threshold = "none", scale = FALSE, signInfo = FALSE)
consecutive <- c(FALSE, diff(dyn$beep) == 1 & diff(dyn$day) == 0)
eligible <- consecutive & c(FALSE, head(consecutive, -1))
assert(identical(native$call$data_lagged$included, eligible), "MGM row-offset lag inclusion differs")
differences <- vapply(1:2, function(lag) equal(fit$networks[[paste0("lag_", lag)]],
  native$wadj[, , lag] * native$signs[, , lag], "MGM lag direction differs"), numeric(1))
results$mixedVAR <- list(max_difference = max(differences), temporal_rows = sum(eligible),
                        recorded = fit$meta$analysis_sample)
assert(fit$meta$analysis_sample$temporal_rows == sum(eligible), "MGM sample count differs")
fitted$mixedVAR <- fit
message("MGM consecutive row windows, signs, multiple lags: PASS")

timepoints <- seq(0, 1, length.out = nrow(dyn))
native <- mgm::tvmvar(data = as.matrix(dyn[, vars]), type = rep("g", 3), level = rep(1, 3),
  lags = c(1, 2), timepoints = timepoints, estpoints = c(.3, .7), bandwidth = .5,
  beepvar = dyn$beep, dayvar = dyn$day, regularize = FALSE, threshold = "none", scale = FALSE, pbar = FALSE)
fit <- TimeVaryingNet(dyn, vars = vars, types = rep("g", 3), levels = rep(1, 3),
  lags = c(1, 2), timepoints = timepoints, estpoints = c(.3, .7), bandwidth = .5,
  beepvar = dyn$beep, dayvar = dyn$day, regularize = FALSE, threshold = "none", scale = FALSE)
differences <- unlist(lapply(1:2, function(point) vapply(1:2, function(lag)
  equal(fit$networks[[paste0("estpoint_", point, "_lag_", lag)]],
    native$wadj[, , lag, point] * native$signs[, , lag, point], "time-varying MGM extraction differs"), numeric(1))))
results$time_varying_mvar <- list(max_difference = max(differences), recorded = fit$meta$analysis_sample)
fitted$time_varying_mvar <- fit
message("Time-varying MGM local weights and lag layers: PASS")

# Day-only data use row order within day; missing beep labels cannot be inferred.
inferred_args <- native_args; inferred_args$beepvar <- NULL
native <- do.call(graphicalVAR::mlGraphicalVAR, c(inferred_args,
  list(scale = FALSE, centerWithin = FALSE, subjectNetworks = FALSE, nLambda = 5, verbose = FALSE)))
fit <- LongitudinalNet(d, vars = vars, id = "id", day = "day", scale = FALSE,
  centerWithin = FALSE, subjectNetworks = FALSE, nLambda = 5)
results$day_only <- list(max_difference = equal(fit$graph, t(native$fixedPDC), "day-only native result differs"),
  temporal_rows = fit$meta$analysis_sample$temporal_rows)
fitted$day_only <- fit
message("Day-only native inferred occasion order: PASS")

# Panel design independently pools adjacent waves and deletes incomplete subjects.
set.seed(46512)
panel <- data.frame(id = paste0("person-", 1:120))
z <- matrix(rnorm(360), 120, 3)
for (wave in 1:3) {
  if (wave > 1) z <- cbind(.2 * z[, 1], .7 * z[, 1] - .15 * z[, 2], -.25 * z[, 3]) + matrix(rnorm(360), 120, 3)
  for (node in seq_along(vars)) panel[[paste0(vars[node], "_t", wave)]] <- z[, node]
}
panel[7, paste0(vars[2], "_t2")] <- NA_real_
fit <- PanelNet(panel, nodes = vars, waves = 1:3, nfolds = 5, seed = 46513,
  standardize = FALSE, lambda_rule = "lambda.min", nlambda = 20)
columns <- unlist(lapply(1:3, function(wave) paste0(vars, "_t", wave)))
keep <- complete.cases(panel[, columns])
x <- do.call(rbind, lapply(1:2, function(wave) unname(as.matrix(panel[keep, paste0(vars, "_t", wave)]))))
y <- do.call(rbind, lapply(2:3, function(wave) unname(as.matrix(panel[keep, paste0(vars, "_t", wave)]))))
colnames(x) <- colnames(y) <- vars
equal(fit$fit$design$predictors, x, "CLPN predictor pairing differs")
equal(fit$fit$design$outcomes, y, "CLPN outcome pairing differs")
assert(identical(fit$fit$design$meta$input_row, rep(which(keep), 2)), "CLPN original row mapping differs")
assert(all(vapply(split(fit$fit$glmnet$foldid, fit$fit$design$meta$id), function(folds) length(unique(folds)) == 1, logical(1))), "CLPN subject leaked across folds")
coefficients <- t(vapply(seq_along(vars), function(target) {
  native <- glmnet::cv.glmnet(x, y[, target], alpha = 1, family = "gaussian", nfolds = 5,
    foldid = fit$fit$glmnet$foldid, standardize = FALSE, nlambda = 20)
  as.numeric(as.matrix(stats::coef(native, s = native$lambda.min))[-1, 1])
}, numeric(3)))
other <- PanelNet(panel[, rev(names(panel))], nodes = vars, waves = 1:3,
  nfolds = 5, seed = 46513, standardize = FALSE, lambda_rule = "lambda.min", nlambda = 20)
results$clpn <- list(max_difference = equal(fit$graph, coefficients, "CLPN native coefficient direction differs"),
  column_reordering_difference = equal(fit$graph, other$graph, "CLPN column reordering differs"),
  input_subjects = nrow(panel), analyzed_subjects = sum(keep), temporal_rows = nrow(x))
assert(fit$meta$analysis_sample$temporal_rows == nrow(x), "CLPN sample count differs")
assert(fit$graph[vars[2], vars[1]] > .2, "Known panel A to B signal missing")
fitted$clpn <- fit
message("CLPN adjacent waves, grouped folds, native direction, sample counts: PASS")

saveRDS(list(results = results, fitted = fitted, data = d, dynamic_data = dyn,
  sessionInfo = sessionInfo(), design_seed = 46510, shuffle_seed = 46511),
  file.path(output, "validation.rds"), compress = "xz")
jsonlite::write_json(results, file.path(output, "validation.json"), pretty = TRUE, auto_unbox = TRUE)
writeLines(capture.output(sessionInfo()), file.path(output, "session-info.txt"))
