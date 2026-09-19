# Gaussian state and topology operators for the SymPerturb method contract.
# Implemented in R from the mathematical specification; topology thresholding
# never changes the covariance used for state interventions.

quicknet_sym_defaults <- function() {
  list(ridge = 0.02, edge_threshold = 0.03, bounds = c(0, 4),
       state_map = "linked", mu_power = 1, sigma_power = 1,
       dose_grid = c(0, .10, .25, .50, .75, 1),
       dose_efficiency_grid = c(.25, .50, .75),
       responsiveness_epsilon = .10, breadth_threshold = .10,
       module_threshold = .20, block_fraction = .80,
       propagation_steps = 6L, propagation_gamma = .45,
       propagation_absolute = TRUE, adjacency_normalization = "raw",
       combination_partner_k = 5L, combination_mode = "signed",
       vpps_weights = numeric(), bootstrap_replicates = 0L,
       bootstrap_top_k = 5L, random_seed = 20260727L,
       run_robustness_scenarios = TRUE, sequence_length = 0L,
       sequence_pool = 8L, sequence_beam_width = 20L,
       sequence_eta = .90, sequence_cost_lambda = 0)
}

quicknet_sym_config <- function(config = list()) {
  cfg <- quicknet_sym_defaults()
  if (!is.list(config) || (length(config) &&
      (is.null(names(config)) || anyNA(names(config)) || anyDuplicated(names(config)) ||
       any(!names(config) %in% names(cfg))))) {
    stop("config must be a uniquely named list of SymPerturb settings; see ?Perturbation.", call. = FALSE)
  }
  for (nm in names(config)) cfg[nm] <- config[nm] # preserve explicit bounds=NULL
  scalar <- function(nm, lower = -Inf, upper = Inf, integer = FALSE) {
    v <- cfg[[nm]]
    if (!is.numeric(v) || length(v) != 1L || !is.finite(v) ||
        v < lower || v > upper || (integer && v != floor(v))) {
      stop("config$", nm, " must be a finite ", if (integer) "integer " else "number ",
           "in [", lower, ", ", upper, "].", call. = FALSE)
    }
  }
  for (nm in c("ridge", "edge_threshold", "propagation_gamma", "sequence_cost_lambda")) scalar(nm, 0)
  for (nm in c("mu_power", "sigma_power")) {
    scalar(nm, 0)
    if (cfg[[nm]] == 0) stop("config$", nm, " must be positive.", call. = FALSE)
  }
  for (nm in c("breadth_threshold", "module_threshold")) scalar(nm)
  for (nm in c("block_fraction", "sequence_eta", "responsiveness_epsilon")) scalar(nm, 0, 1)
  if (cfg$responsiveness_epsilon == 0) stop("responsiveness_epsilon must be positive.", call. = FALSE)
  for (nm in c("propagation_steps", "combination_partner_k", "bootstrap_top_k", "sequence_pool", "sequence_beam_width")) scalar(nm, 1, integer = TRUE)
  for (nm in c("bootstrap_replicates", "sequence_length", "random_seed")) scalar(nm, 0, .Machine$integer.max, TRUE)
  for (nm in c("propagation_absolute", "run_robustness_scenarios")) {
    if (!is.logical(cfg[[nm]]) || length(cfg[[nm]]) != 1L || is.na(cfg[[nm]])) stop(nm, " must be TRUE or FALSE.", call. = FALSE)
  }
  choices <- list(state_map = c("linked", "location_only", "scale_only", "independent"),
                  adjacency_normalization = c("raw", "row", "spectral"),
                  combination_mode = c("signed", "positive"))
  for (nm in names(choices)) {
    if (!is.character(cfg[[nm]]) || length(cfg[[nm]]) != 1L || !cfg[[nm]] %in% choices[[nm]]) stop("Invalid config$", nm, ".", call. = FALSE)
  }
  for (nm in c("dose_grid", "dose_efficiency_grid")) {
    quicknet_sym_validate_dose(cfg[[nm]])
  }
  if (!any(cfg$dose_efficiency_grid > 0)) stop("dose_efficiency_grid requires a positive dose.", call. = FALSE)
  b <- cfg$bounds
  if (!is.null(b) && (!is.numeric(b) || length(b) != 2L || any(!is.finite(b)) || b[1] >= b[2])) stop("bounds must be NULL or two finite increasing limits.", call. = FALSE)
  cfg
}

quicknet_sym_validate_dose <- function(alpha) {
  if (!is.numeric(alpha) || !length(alpha) || any(!is.finite(alpha)) || any(alpha < 0 | alpha > 1)) stop("dose must contain finite fractions in [0, 1].", call. = FALSE)
  invisible(alpha)
}

quicknet_sym_inverse <- function(x) {
  inv <- tryCatch(solve(x), error = function(e) NULL)
  if (!is.null(inv)) return(list(value = inv, used_pseudoinverse = FALSE))
  # numpy.linalg.pinv's default cutoff is 1e-15 times the largest singular value.
  d <- svd(x)
  keep <- d$d > 1e-15 * max(d$d)
  val <- if (any(keep)) {
    sweep(d$v[, keep, drop = FALSE], 2, d$d[keep], "/") %*% t(d$u[, keep, drop = FALSE])
  } else matrix(0, nrow(x), ncol(x))
  dimnames(val) <- dimnames(x)
  list(value = val, used_pseudoinverse = TRUE)
}

quicknet_sym_network <- function(data, config) {
  s <- stats::cov(data)
  covariance <- s + diag(config$ridge * diag(s), nrow(s))
  inv <- quicknet_sym_inverse(covariance)
  den <- sqrt(pmax(diag(inv$value), 1e-15))
  pc <- -inv$value / outer(den, den)
  diag(pc) <- 0
  pc <- (pc + t(pc)) / 2
  adjacency <- pc
  adjacency[abs(adjacency) < config$edge_threshold] <- 0
  list(mu = colMeans(data), covariance = covariance, precision = inv$value,
       partial_correlations = pc, adjacency = adjacency, ridge = config$ridge,
       edge_threshold = config$edge_threshold, used_pseudoinverse = inv$used_pseudoinverse)
}

quicknet_sym_moments <- function(mu, covariance, targets, alpha, anchors = rep(0, length(mu)),
                                state_map = "linked", mu_power = 1, sigma_power = 1) {
  p <- length(mu)
  s <- sort(unique(targets))
  if (!length(s) || anyNA(s) || any(s < 1 | s > p | s != floor(s))) stop("Invalid target indices.", call. = FALSE)
  if (!identical(dim(covariance), c(p, p)) || length(anchors) != p) stop("State moment dimensions do not match.", call. = FALSE)
  quicknet_sym_validate_dose(alpha)
  if (length(alpha) != 1L && length(alpha) != length(s)) stop("Supply one dose or one dose per sorted target.", call. = FALSE)
  a <- rep(alpha, length.out = length(s))
  dmu <- dsigma <- 1 - a
  if (state_map == "location_only") dsigma[] <- 1
  else if (state_map == "scale_only") dmu[] <- 1
  else if (state_map == "independent") {
    if (mu_power <= 0 || sigma_power <= 0) stop("State powers must be positive.", call. = FALSE)
    dmu <- (1 - a)^mu_power
    dsigma <- (1 - a)^sigma_power
  } else if (state_map != "linked") stop("Unknown state_map.", call. = FALSE)
  k <- setdiff(seq_len(p), s)
  target_cov <- covariance[s, s, drop = FALSE] * outer(dsigma, dsigma)
  post_mu <- mu
  post_mu[s] <- anchors[s] + dmu * (mu[s] - anchors[s])
  post_cov <- matrix(0, p, p, dimnames = dimnames(covariance))
  post_cov[s, s] <- target_cov
  if (length(k)) {
    B <- covariance[k, s, drop = FALSE] %*% quicknet_sym_inverse(covariance[s, s, drop = FALSE])$value
    residual <- covariance[k, k, drop = FALSE] - B %*% covariance[s, k, drop = FALSE]
    post_mu[k] <- mu[k] + as.vector(B %*% (post_mu[s] - mu[s]))
    post_cov[k, k] <- residual + B %*% target_cov %*% t(B)
    post_cov[k, s] <- B %*% target_cov
    post_cov[s, k] <- t(B %*% target_cov)
  }
  list(mean = post_mu, covariance = (post_cov + t(post_cov)) / 2,
       target_indices = s, location_multipliers = dmu, scale_multipliers = dsigma)
}

quicknet_sym_observed <- function(mu, covariance, bounds) {
  if (is.null(bounds)) return(mu)
  sd <- sqrt(pmax(diag(covariance), 0))
  lo <- bounds[1]; hi <- bounds[2]
  out <- pmin(hi, pmax(lo, mu))
  idx <- which(sd > 1e-14)
  a <- (lo - mu[idx]) / sd[idx]
  b <- (hi - mu[idx]) / sd[idx]
  out[idx] <- lo * stats::pnorm(a) + mu[idx] * (stats::pnorm(b) - stats::pnorm(a)) +
    sd[idx] * (stats::dnorm(a) - stats::dnorm(b)) + hi * (1 - stats::pnorm(b))
  names(out) <- names(mu)
  out
}

quicknet_sym_radius <- function(W) max(Mod(eigen(W, only.values = TRUE)$values))

quicknet_sym_normalize_adjacency <- function(W, mode) {
  if (mode == "row") return(W / pmax(rowSums(abs(W)), .Machine$double.xmin))
  if (mode == "spectral") {
    r <- quicknet_sym_radius(abs(W))
    if (r > 1e-15) return(W / r)
  }
  W
}

quicknet_sym_potential <- function(W, config) {
  A <- quicknet_sym_normalize_adjacency(W, config$adjacency_normalization)
  if (config$propagation_absolute) A <- abs(A)
  A <- config$propagation_gamma * A
  power <- diag(nrow(A))
  total <- 0
  for (i in seq_len(config$propagation_steps)) {
    power <- power %*% A
    total <- total + sum(power)
  }
  total
}

quicknet_sym_block <- function(W, targets, fraction, edge = FALSE) {
  if (edge) {
    W[targets[1], targets[2]] <- W[targets[1], targets[2]] * (1 - fraction)
    W[targets[2], targets[1]] <- W[targets[2], targets[1]] * (1 - fraction)
  } else {
    W[targets, ] <- W[targets, ] * (1 - fraction)
    W[, targets] <- W[, targets] * (1 - fraction)
    diag(W) <- 0
  }
  W
}

quicknet_sym_minmax <- function(x) {
  out <- rep(NA_real_, length(x))
  ok <- is.finite(x)
  if (!any(ok)) return(out)
  lo <- min(x[ok]); hi <- max(x[ok])
  out[ok] <- if (abs(lo - hi) <= 1e-8 + 1e-5 * abs(hi)) 50 else 100 * (x[ok] - lo) / (hi - lo)
  out
}

quicknet_sym_utilities <- function() c("efficacy", "dose_efficiency", "breadth", "cross_module",
                                      "communication_block", "combination_value", "responsiveness")

quicknet_sym_named <- function(x, nodes, default, label, nonnegative = FALSE) {
  out <- stats::setNames(rep(default, length(nodes)), nodes)
  if (is.null(x) || !length(x)) return(out)
  if (!is.numeric(x) || is.null(names(x)) || anyNA(names(x)) || anyDuplicated(names(x)) ||
      any(!names(x) %in% nodes) || any(!is.finite(x)) || (nonnegative && any(x < 0))) {
    stop(label, " must be a finite named numeric vector", if (nonnegative) " with non-negative values", ".", call. = FALSE)
  }
  out[names(x)] <- x
  out
}

quicknet_sym_context <- function(data, config, anchors = NULL, symptom_weights = NULL,
                                modules = NULL, costs = NULL) {
  if (!is.data.frame(data) && !is.matrix(data)) stop("SymPerturb requires original participant data in fit$data.", call. = FALSE)
  if (nrow(data) < 3L || ncol(data) < 3L || !all(vapply(as.data.frame(data), is.numeric, logical(1)))) stop("SymPerturb requires at least three rows and three numeric symptom columns.", call. = FALSE)
  x <- as.matrix(data)
  nodes <- colnames(x)
  if (is.null(nodes) || anyNA(nodes) || any(!nzchar(nodes)) || anyDuplicated(nodes)) stop("Symptom names must be unique and nonempty.", call. = FALSE)
  if (any(!is.finite(x))) stop("SymPerturb requires finite data; handle missingness explicitly before analysis.", call. = FALSE)
  if (!is.null(modules)) {
    if (is.null(names(modules)) || anyNA(names(modules)) || anyDuplicated(names(modules)) || any(!nodes %in% names(modules)) || anyNA(modules)) stop("modules must map every symptom name to a module.", call. = FALSE)
    modules <- stats::setNames(as.character(modules[nodes]), nodes)
    if (length(unique(modules)) < 2L) stop("Cross-module utility requires at least two modules.", call. = FALSE)
  }
  net <- quicknet_sym_network(x, config)
  weights <- quicknet_sym_named(symptom_weights, nodes, 1, "symptom_weights", TRUE)
  if (sum(weights) <= 1e-8) stop("symptom_weights must have positive total.", call. = FALSE)
  list(data = x, nodes = nodes, config = config, network = net,
       anchors = quicknet_sym_named(anchors, nodes, 0, "anchors"), weights = weights,
       modules = modules, costs = quicknet_sym_named(costs, nodes, 0, "costs", TRUE),
       observed = quicknet_sym_observed(net$mu, net$covariance, config$bounds),
       denominator = sqrt(pmax(diag(net$covariance), 1e-15)))
}

quicknet_sym_post <- function(ctx, targets, alpha) {
  cfg <- ctx$config
  quicknet_sym_moments(ctx$network$mu, ctx$network$covariance, match(targets, ctx$nodes),
                      alpha, ctx$anchors, cfg$state_map, cfg$mu_power, cfg$sigma_power)
}

quicknet_sym_delta <- function(ctx, targets, alpha = 1) {
  post <- quicknet_sym_post(ctx, targets, alpha)
  (ctx$observed - quicknet_sym_observed(post$mean, post$covariance, ctx$config$bounds)) / ctx$denominator
}

quicknet_sym_benefit <- function(ctx, delta, outcomes) {
  if (!length(outcomes)) return(0)
  w <- ctx$weights[outcomes]
  if (sum(w) <= 1e-8) mean(delta[outcomes]) else sum(w * delta[outcomes]) / sum(w)
}

quicknet_sym_pair <- function(ctx, a, b) {
  outcomes <- setdiff(ctx$nodes, c(a, b))
  joint <- quicknet_sym_benefit(ctx, quicknet_sym_delta(ctx, c(a, b)), outcomes)
  sa <- quicknet_sym_benefit(ctx, quicknet_sym_delta(ctx, a), outcomes)
  sb <- quicknet_sym_benefit(ctx, quicknet_sym_delta(ctx, b), outcomes)
  data.frame(target_a = a, target_b = b, joint_benefit_common_set = joint,
             single_a_common_set = sa, single_b_common_set = sb,
             incremental_pair_value = joint - max(sa, sb), row.names = NULL)
}
