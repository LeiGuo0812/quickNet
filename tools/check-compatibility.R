# Run against an installed package in a fresh R session:
# Rscript tools/check-compatibility.R complete|minimal current|baseline [output_dir]
# A minimal profile must expose only Imports and their hard dependencies, plus
# standard R packages and package-check tooling. It is not a minimum-version test.
args <- commandArgs(trailingOnly = TRUE)
profile <- if (length(args)) match.arg(args[[1L]], c("complete", "minimal")) else "complete"
track <- if (length(args) >= 2L) match.arg(args[[2L]], c("current", "baseline")) else "current"
if (profile == "minimal") .libPaths(setdiff(.libPaths(), .Library.site), include.site = FALSE)
out_dir <- if (length(args) >= 3L) args[[3L]] else "../output/audit/compatibility"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_dir <- normalizePath(out_dir, winslash = "/")
report_path <- file.path(out_dir, "compatibility-report.txt")
sink(report_path, split = TRUE)
cat("Profile:", profile, " Track:", track, "\n")
cat("Library paths:\n"); print(.libPaths())
print(sessionInfo())
cat("\n")

desc <- read.dcf("DESCRIPTION")
parse_deps <- function(value) trimws(gsub("\\s*\\(.*?\\)", "", strsplit(value, ",")[[1L]]))
imports <- parse_deps(desc[1L, "Imports"])
suggests <- parse_deps(desc[1L, "Suggests"])
ip <- installed.packages()
hard <- unique(c(imports, unlist(tools::package_dependencies(imports, db = ip,
  which = c("Depends", "Imports", "LinkingTo"), recursive = TRUE))))
hard <- setdiff(hard, "R")
stopifnot(all(hard %in% rownames(ip)))
if (profile == "complete") stopifnot(all(suggests %in% rownames(ip)))
optional <- setdiff(suggests, hard)
standard <- rownames(ip)[!is.na(ip[, "Priority"])]
unneeded_optional <- setdiff(optional, standard)
if (profile == "minimal" && any(unneeded_optional %in% rownames(ip))) {
  stop("The minimal library exposes optional packages outside the hard dependency closure: ",
       paste(intersect(unneeded_optional, rownames(ip)), collapse = ", "))
}
cat("Suggests required transitively by Imports:", paste(intersect(suggests, hard), collapse = ", "), "\n")
cat("Independent optional packages:", paste(optional, collapse = ", "), "\n")

baseline <- read.csv("tools/compatibility-baseline.csv", stringsAsFactors = FALSE)
selected <- if (profile == "complete") baseline$Package else intersect(baseline$Package, hard)
version_table <- baseline[baseline$Package %in% selected, , drop = FALSE]
version_table$Installed <- ip[match(version_table$Package, rownames(ip)), "Version"]
write.csv(version_table, file.path(out_dir, "dependency-versions.csv"), row.names = FALSE)
write.csv(ip[, c("Package", "Version", "LibPath")],
          file.path(out_dir, "installed-packages.csv"), row.names = FALSE)
if (track == "baseline") {
  stopifnot(as.character(getRversion()) == "4.5.3")
  mismatch <- version_table$Package[is.na(version_table$Installed) |
    version_table$Installed != version_table$Version]
  if (length(mismatch)) stop("The baseline environment differs from its recorded snapshot: ",
                            paste(mismatch, collapse = ", "))
}
writeLines(capture.output(sessionInfo()), file.path(out_dir, "session-info.txt"))

suppressPackageStartupMessages(library(quickNet))
ns <- asNamespace("quickNet")
get_private <- function(name) get(name, envir = ns, inherits = FALSE)
expect_error <- function(expr, pattern) {
  error <- tryCatch({ force(expr); NULL }, error = identity)
  if (!inherits(error, "error") || !grepl(pattern, conditionMessage(error))) {
    stop("Expected a dependency diagnostic matching: ", pattern)
  }
  cat("Dependency diagnostic:", conditionMessage(error), "\n")
}

# qgraph's argument routing is sensitive to its private whitelist structure.
# Check behavior as well as the adapter's list, so backend changes fail loudly.
allowed <- get_private("quicknet_qgraph_argument_names")()
stopifnot(all(c("DoNotPlot", "layout", "color", "label.cex") %in% allowed))
set.seed(90123)
data <- as.data.frame(matrix(rnorm(480), 120, 4))
names(data) <- paste0("x", 1:4)
fit <- quickNet(data, model = "correlation", pie = FALSE,
                DoNotPlot = TRUE, layout = "circle", color = "steelblue", label.cex = .8)
stopifnot(inherits(fit, "quicknet_fit"), all(is.finite(fit$graph)),
          identical(rownames(fit$graph), names(data)))
expected <- cor(data); diag(expected) <- 0
stopifnot(isTRUE(all.equal(fit$graph, expected)))
report <- quicknet_report(fit)
stopifnot(inherits(report, "quicknet_report"), nrow(report$networks) >= 1L)
grDevices::pdf(file.path(out_dir, "network-smoke.pdf"))
plot(fit)
grDevices::dev.off()
cat("qgraph routing, core fit, report and graphics: PASS\n")

# Follow current bootnet formals, including versions whose correlation default
# is cor_auto. This verifies inheritance without forcing a future default.
source_missing <- get_private("quicknet_backend_default")(bootnet::bootnet_EBICglasso, "missing", TRUE)
stopifnot(identical(get_private("quicknet_cross_missing")("EBICglasso"), source_missing))
ebic <- EBICglassoNet(data, nlambda = 10)
stopifnot(inherits(ebic, "quicknet_fit"), all(is.finite(ebic$graph)))
cat("bootnet defaults and estimation: PASS\n")

# lavaan is currently in the hard dependency closure even though quickNet also
# lists it in Suggests. Verify the actual object slot used by the adapter.
if (requireNamespace("lavaan", quietly = TRUE)) {
  z <- rnorm(160)
  latent_data <- as.data.frame(replicate(3L, z + rnorm(160, sd = .5)))
  names(latent_data) <- paste0("x", 1:3)
  latent <- LatentNet(latent_data, "f =~ x1 + x2 + x3", estimator = "MLR", residual = FALSE)
  stopifnot("Options" %in% methods::slotNames(latent$fit$model),
            identical(latent$meta$estimator, "MLR"))
  cat("lavaan estimator metadata and S4 extraction: PASS\n")
}

if (profile == "complete") {
  confirmatory <- ConfirmatoryNet(data)
  raw <- confirmatory$fit$model
  if (is.null(raw)) raw <- confirmatory$fit
  stopifnot(all(c("estimator", "optimizer", "identification") %in% methods::slotNames(raw)),
            isTRUE(all.equal(confirmatory$meta$estimator, raw@estimator)),
            all(is.finite(confirmatory$graph)))
  cat("psychonetrics S4 extraction and estimation: PASS\n")
} else {
  longitudinal <- data.frame(id = rep(1:4, each = 10), a = rnorm(40), b = rnorm(40))
  if ("graphicalVAR" %in% unneeded_optional)
    expect_error(LongitudinalNet(longitudinal, vars = c("a", "b")), "Package 'graphicalVAR' is required")
  if ("mlVAR" %in% unneeded_optional)
    expect_error(LongitudinalNet(longitudinal, vars = c("a", "b"), model = "mlVAR"), "Package 'mlVAR' is required")
  if ("psychonetrics" %in% unneeded_optional)
    expect_error(ConfirmatoryNet(data), "Package 'psychonetrics' is required")
  if ("powerly" %in% unneeded_optional)
    expect_error(NetworkPower(method = "powerly", nodes = 4, density = .3,
      range_lower = 100, range_upper = 200), "Package 'powerly' is required")
}
cat("\nCompatibility smoke checks: PASS\n")
sink()
