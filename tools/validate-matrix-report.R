#!/usr/bin/env Rscript
# Run from package root, with BLAS threads limited before R starts.
args <- commandArgs(trailingOnly = TRUE)
out <- if (length(args)) args[[1]] else "../output/audit/matrix-report"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
pkgload::load_all(quiet = TRUE)
rows <- list(); fits <- list(); warnings <- list()
run <- function(x) {
  value <- NULL
  invisible(capture.output(value <- withCallingHandlers(force(x), warning = function(w) {
    warnings[[length(warnings) + 1L]] <<- conditionMessage(w)
    invokeRestart("muffleWarning")
  })))
  value
}
compare <- function(case, layer, actual, expected, tolerance = 1e-7) {
  stopifnot(identical(dim(actual), dim(expected)))
  error <- max(abs(actual - expected))
  rows[[length(rows) + 1L]] <<- data.frame(case, layer, max_abs_error = error, tolerance,
                                        passed = is.finite(error) && error <= tolerance)
  if (!is.finite(error) || error > tolerance) stop(case, "/", layer, " mismatch: ", error)
}
offdiag <- function(x) { diag(x) <- 0; x }
set.seed(46280)
latent <- matrix(rnorm(1000), 500, 2)
latent[, 2] <- .4 * latent[, 1] + sqrt(1 - .4^2) * latent[, 2]
dat <- as.data.frame(cbind(sapply(1:3, function(i) latent[, 1] + rnorm(500, sd = .8)),
                          sapply(1:3, function(i) latent[, 2] + rnorm(500, sd = .8))))
names(dat) <- paste0("x", 1:6)
for (model in c("ggm", "cor", "covariance", "precision", "ising")) {
  data <- if (model == "ising") as.data.frame(1L * (dat[, 1:4] > 0)) else dat[, c(3, 1, 4, 2)]
  actual <- run(ConfirmatoryNet(data, model = model))
  fun <- if (model == "ising") psychonetrics::Ising else psychonetrics::varcov
  source_args <- list(data = data, vars = colnames(data), verbose = FALSE)
  if (model != "ising") source_args$type <- switch(model, covariance = "cov", precision = "prec", model)
  native <- run(psychonetrics::runmodel(do.call(fun, source_args)))
  matrix_name <- switch(model, ggm = "omega", cor = "rho", covariance = "sigma", precision = "kappa", ising = "omega")
  compare(paste0("confirmatory_", model), matrix_name, actual$graph,
          offdiag(psychonetrics::getmatrix(native, matrix_name)))
  compare(paste0("confirmatory_", model), "nobs", matrix(quicknet_report(actual)$sample$observations),
          matrix(sum(native@sample@groups$nobs)), 0)
  fits[[paste0("confirmatory_", model)]] <- actual
}

syntax <- "F1 =~ x1+x2+x3\nF2 =~ x4+x5+x6"
actual <- run(LatentNet(dat, syntax))
native <- run(lavaan::cfa(syntax, data = dat, std.lv = FALSE, missing = "listwise"))
compare("CFA", "latent_correlation", actual$networks$latent, offdiag(lavaan::lavInspect(native, "cor.lv")))
scores <- lavaan::lavPredict(native)
residuals <- sapply(dat, function(y) resid(lm(y ~ scores)))
residual_cor <- cor(residuals)
# Regressing all items on factor scores can make the correlation singular;
# the documented residual workflow uses the Matrix nearPD correction.
if (min(eigen(residual_cor, symmetric = TRUE, only.values = TRUE)$values) <= 1e-8)
  residual_cor <- as.matrix(Matrix::nearPD(residual_cor, corr = TRUE)$mat)
compare("CFA", "factor_score_regression_residual_correlation", actual$networks$residual, offdiag(residual_cor))
fits$CFA <- actual

lambda <- matrix(0, 6, 2, dimnames = list(names(dat), c("F1", "F2")))
lambda[1:3, 1] <- 1; lambda[4:6, 2] <- 1
residual_template <- matrix(0, 6, 6); residual_template[1, 4] <- residual_template[4, 1] <- 1
for (model in c("lnm", "lrnm")) {
  extra <- if (model == "lrnm") list(omega_epsilon = residual_template) else list()
  actual <- run(do.call(LatentNet, c(list(data = dat, model = model, lambda = lambda), extra)))
  native <- run(psychonetrics::runmodel(do.call(get(model, asNamespace("psychonetrics")),
    c(list(data = dat, lambda = lambda, vars = names(dat), latents = c("F1", "F2"),
           identification = "loadings", verbose = FALSE), extra))))
  compare(model, "latent", actual$networks$latent, psychonetrics::getmatrix(native, "omega_zeta"))
  if (model == "lrnm") compare(model, "residual", actual$networks$residual, psychonetrics::getmatrix(native, "omega_epsilon"))
  fits[[model]] <- actual
}

# Independent participant trajectories with a negative one-way cross-lag.
n <- 350; nodes <- c("B", "A"); waves <- 1:4
panel <- data.frame(id = seq_len(n)); ri <- matrix(rnorm(n * 2, sd = .7), n, 2)
previous <- matrix(rnorm(n * 2), n, 2)
transition <- matrix(c(.4, -.25, 0, .25), 2)
for (wave in waves) {
  current <- previous %*% t(transition) + matrix(rnorm(n * 2), n, 2)
  panel[paste0(nodes, "_t", wave)] <- current + ri
  previous <- current
}
vars <- matrix(paste0(rep(nodes, times = 4), "_t", rep(waves, each = 2)), 2, 4)
for (model in c("ri_clpm", "panel_var", "panel_gvar")) {
  actual <- run(PanelNet(panel, nodes, waves, model = model, standardize = FALSE))
  fun <- get(switch(model, ri_clpm = "ri_clpm", panel_var = "panelvar", panel_gvar = "panelgvar"), asNamespace("psychonetrics"))
  native <- run(psychonetrics::runmodel(do.call(fun, c(list(data = panel, vars = vars, standardize = "none", verbose = FALSE),
    if (model == "ri_clpm") list(type = "cov") else list()))))
  if (model == "ri_clpm") {
    beta <- psychonetrics::getmatrix(native, "beta")
    innovation <- psychonetrics::getmatrix(native, "sigma_zeta")
    temporal <- Reduce(`+`, lapply(1:3, function(t) beta[t * 2 + 1:2, (t - 1) * 2 + 1:2])) / 3
    within <- Reduce(`+`, lapply(0:3, function(t) innovation[t * 2 + 1:2, t * 2 + 1:2])) / 4
    compare(model, "temporal", actual$networks$temporal, temporal)
    compare(model, "contemporaneous", actual$networks$contemporaneous, offdiag(within))
    compare(model, "random_intercept", actual$networks$random_intercept, offdiag(innovation[9:10, 9:10]))
  } else {
    compare(model, "temporal", actual$networks$temporal, psychonetrics::getmatrix(native, "beta"))
    prefix <- if (model == "panel_var") "sigma" else "omega"
    compare(model, "within", actual$networks$within, offdiag(psychonetrics::getmatrix(native, paste0(prefix, "_zeta_within"))))
    compare(model, "between", actual$networks$between, offdiag(psychonetrics::getmatrix(native, paste0(prefix, "_zeta_between"))))
  }
  fits[[model]] <- actual
}

# Direct lavaan source model assembled independently of quickNet's syntax builder.
actual <- run(PanelSEMNet(panel, nodes, waves))
regression <- unlist(lapply(2:4, function(t) paste0(nodes, "_t", t, " ~ B_t", t - 1, " + A_t", t - 1)))
covariance <- paste0("B_t", 2:4, " ~~ A_t", 2:4)
native <- run(lavaan::sem(paste(c(regression, covariance), collapse = "\n"), data = panel,
                         missing = "listwise", auto.cov.y = FALSE))
standardized <- lavaan::standardizedSolution(native)
expected <- matrix(0, 2, 2)
for (i in 1:2) for (j in 1:2) expected[i, j] <- mean(vapply(2:4, function(t) {
  standardized$est.std[standardized$op == "~" & standardized$lhs == paste0(nodes[i], "_t", t) &
                       standardized$rhs == paste0(nodes[j], "_t", t - 1)]
}, numeric(1)))
compare("panel_sem", "mean_standardized_adjacent_paths", actual$graph, expected)
fits$panel_sem <- actual

for (model in names(fits)) {
  fit <- fits[[model]]
  report <- quicknet_report(fit)
  stopifnot(identical(report$nodes, fit$nodes), identical(report$diagnostics, fit$diagnostics))
  for (layer in unique(fit$edges$network)) {
    edges <- fit$edges[fit$edges$network == layer, ]
    mat <- fit$networks[[layer]]
    compare(model, paste0(layer, "_edge_values"), matrix(edges$weight),
            matrix(mat[cbind(match(edges$to, rownames(mat)), match(edges$from, colnames(mat)))]), 0)
    counted <- sum(abs(edges$weight[edges$from != edges$to]) > 1e-10)
    stopifnot(report$edges$nonzero_edges[report$edges$network == layer] == counted)
  }
}
write.csv(do.call(rbind, rows), file.path(out, "native-comparisons.csv"), row.names = FALSE)
write.csv(do.call(rbind, lapply(names(fits), function(name) cbind(model = name, fits[[name]]$diagnostics))),
          file.path(out, "convergence.csv"), row.names = FALSE)
writeLines(unlist(warnings), file.path(out, "backend-warnings.txt"))
writeLines(capture.output(sessionInfo()), file.path(out, "session-info.txt"))
saveRDS(fits, file.path(out, "source-checked-fits.rds"))
cat(length(rows), "source comparisons passed.\n")
