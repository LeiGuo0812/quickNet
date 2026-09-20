# Independent finite-sample calibration of MTD inference.
# Run from the package root: Rscript tools/validate-mtd-inference.R [output_dir]
# Requires only base R and stats; does not install or load quickNet dependencies.
Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1")
args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args)) args[[1L]] else "../output/audit/inference"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
source("R/internal_utils.R")
source("R/MTD.No.Smooth.Test.R")

# Independent implementation of the previous row-shuffle test. The statistic
# is unchanged; computing just the cross product avoids constructing arrays.
row_shuffle <- function(data, nperm) {
  dx <- diff(data[, 1L]); y <- data[, 2L]
  dx <- dx / sd(dx)
  dy <- diff(y)
  observed <- mean(dx * dy / sd(dy))
  permuted <- vapply(seq_len(nperm), function(i) {
    dy <- diff(sample(y))
    mean(dx * dy / sd(dy))
  }, numeric(1))
  list(coupling_mean = observed,
       p.value = (1 + sum(abs(permuted) >= abs(observed))) / (nperm + 1L))
}

n <- 160L
outer <- 1000L
nperm <- 199L
alpha <- .05
radius <- 39L # Chosen before calibration: 159 derivatives, 81 retained, p_min=.025.
set.seed(98732)
example <- cbind(arima.sim(list(ar = -.7), n = 100),
                 arima.sim(list(ar = -.7), n = 100))
set.seed(9183)
independent <- row_shuffle(example, nperm)
set.seed(9183)
public_shuffle <- MTD.No.Smooth.Test(example, method = "shuffle", nperm = nperm)
stopifnot(isTRUE(all.equal(independent$coupling_mean, public_shuffle$coupling_mean,
                          tolerance = 1e-14)),
          identical(independent$p.value, public_shuffle$p.value))

# During the repair audit this file is an untouched copy of the original public
# implementation. A fresh checkout can still run the entire calibration above
# and below without the optional audit files.
frozen_path <- "../tmp/mtd-baseline-source.R"
frozen_verified <- file.exists(frozen_path)
if (frozen_verified) {
  old <- new.env(parent = globalenv())
  sys.source(frozen_path, envir = old)
  set.seed(9183)
  frozen <- old$MTD.No.Smooth.Test(example, nperm = nperm)
  stopifnot(isTRUE(all.equal(independent$coupling_mean, frozen$coupling_mean,
                            tolerance = 1e-14)),
            identical(independent$p.value, frozen$p.value))
}
cat("Independent/public shuffle equivalence passed; frozen baseline checked:",
    frozen_verified, "\n")

scenarios <- list(
  iid = list(ar = numeric()),
  ar1_positive_08 = list(ar = .8),
  ar1_negative_05 = list(ar = -.5),
  ar1_negative_08 = list(ar = -.8),
  ar2_period12_radius095 = list(ar = c(2 * .95 * cos(pi / 6), -.95^2)),
  ar2_period24_radius098 = list(ar = c(2 * .98 * cos(pi / 12), -.98^2))
)
baseline_path <- file.path(out_dir, "mtd-baseline-independent.rds")
baseline <- if (file.exists(baseline_path)) readRDS(baseline_path) else NULL
raw <- list(); summaries <- list(); baseline_verified <- !is.null(baseline)
summarize <- function(values, scenario, method, seed, null) {
  rejected <- sum(values <= alpha)
  rate <- rejected / length(values)
  ci <- binom.test(rejected, length(values))$conf.int
  data.frame(scenario = scenario, method = method, null = null, n = n,
    outer = length(values), alpha = alpha,
    nperm = if (method == "shuffle") nperm else NA_integer_,
    radius = if (method == "tts") radius else NA_integer_,
    rejected = rejected, rejection_rate = rate,
    mcse = sqrt(rate * (1 - rate) / length(values)),
    ci_low = ci[[1L]], ci_high = ci[[2L]], seed = seed)
}
for (s in seq_along(scenarios)) {
  name <- names(scenarios)[[s]]; ar <- scenarios[[s]]$ar
  seed <- 991500L + s
  set.seed(seed)
  simulate <- function() {
    if (length(ar)) as.numeric(arima.sim(list(ar = ar), n = n, n.start = 2000L)) else rnorm(n)
  }
  vals <- t(vapply(seq_len(outer), function(i) {
    x <- simulate(); y <- simulate(); d <- cbind(x, y)
    # Preserve the full old RNG path before computing deterministic TTS.
    previous <- row_shuffle(d, nperm)
    rng_before <- .Random.seed
    corrected <- MTD.No.Smooth.Test(d, radius = radius)
    stopifnot(identical(rng_before, .Random.seed))
    c(p = previous$p.value, coupling = previous$coupling_mean,
      dx_acf1 = acf(diff(x), lag.max = 1, plot = FALSE)$acf[[2L]],
      dy_acf1 = acf(diff(y), lag.max = 1, plot = FALSE)$acf[[2L]],
      p_tts = corrected$p.value, tested_coupling = corrected$test_coupling_mean)
  }, numeric(6)))
  if (!is.null(baseline)) {
    stopifnot(identical(vals[, 1:4, drop = FALSE], baseline$raw[[name]]))
  }
  raw[[name]] <- vals
  rows <- rbind(summarize(vals[, "p"], name, "shuffle", seed, TRUE),
                summarize(vals[, "p_tts"], name, "tts", seed, TRUE))
  summaries[[name]] <- rows
  print(rows, row.names = FALSE); flush.console()
}

# Stationary dependence sanity checks: same AR spectrum, correlated innovations.
# These settings were chosen in advance; this is not a general power study.
for (s in 1:2) {
  rho <- c(.6, -.6)[[s]]
  name <- c("coupled_ar1_positive", "coupled_ar1_negative")[[s]]
  seed <- 994000L + s
  set.seed(seed)
  vals <- t(vapply(seq_len(outer), function(i) {
    x <- as.numeric(arima.sim(list(ar = .5), n = n, n.start = 2000L))
    independent <- as.numeric(arima.sim(list(ar = .5), n = n, n.start = 2000L))
    y <- rho * x + sqrt(1 - rho^2) * independent
    corrected <- MTD.No.Smooth.Test(cbind(x, y), radius = radius)
    c(p_tts = corrected$p.value, coupling = corrected$coupling_mean,
      tested_coupling = corrected$test_coupling_mean)
  }, numeric(3)))
  raw[[name]] <- vals
  row <- summarize(vals[, "p_tts"], name, "tts", seed, FALSE)
  row$rejected <- as.integer(row$rejected)
  summaries[[name]] <- row
  print(row, row.names = FALSE); flush.console()
}
summary <- do.call(rbind, summaries)
rownames(summary) <- NULL
metadata <- list(n = n, outer = outer, nperm = nperm, alpha = alpha, radius = radius,
  derivative_length = n - 1L, retained_derivative_length = n - 1L - 2L * radius,
  shifts = 2L * radius + 1L, minimum_tts_p = 1 / (radius + 1),
  source_md5 = tools::md5sum(c("R/MTD.No.Smooth.Test.R", "tools/validate-mtd-inference.R")),
  frozen_baseline_verified = frozen_verified,
  all_old_results_identical = baseline_verified,
  scenarios = scenarios, alternative_ar = .5, alternative_rho = c(.6, -.6),
  sessionInfo = sessionInfo())
write.csv(summary, file.path(out_dir, "mtd-inference-validation.csv"), row.names = FALSE)
saveRDS(list(summary = summary, raw = raw, metadata = metadata),
        file.path(out_dir, "mtd-inference-validation.rds"))
writeLines(capture.output(str(metadata)), file.path(out_dir, "mtd-inference-validation-metadata.txt"))
cat("Frozen source verified:", frozen_verified,
    "; every previous calibration value reproduced exactly:", baseline_verified, "\n")
