#!/usr/bin/env Rscript
# Revalidate saved intervention audit results without repeating long sampling.
# Run after validate-intervention-reliability.R has finished writing its files.
script <- normalizePath(sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))][1L]))
root <- normalizePath(file.path(dirname(script), ".."))
output <- file.path(root, "..", "output", "audit", "intervention-reliability")
audit_script <- file.path(root, "tools", "validate-intervention-reliability.R")
pkgload::load_all(root, quiet = TRUE)
expressions <- parse(audit_script)
environment <- new.env(parent = globalenv())
environment$root <- root; environment$output <- output
# Load definitions from the frozen script without executing its stage loop.
for (expression in expressions) {
  if (is.call(expression) && identical(expression[[1L]], quote(`<-`)) &&
      is.call(expression[[3L]]) && identical(expression[[3L]][[1L]], quote(`function`))) {
    eval(expression, environment)
  }
}
dispatch <- Filter(function(x) is.call(x) && identical(x[[1L]], quote(`<-`)) && identical(x[[2L]], quote(stages)), as.list(expressions))
stopifnot(length(dispatch) == 1L)
stages <- eval(dispatch[[1L]][[3L]], environment)
stopifnot(identical(names(stages), c("sampling", "stability", "permutation", "moderation", "failure_guard", "symperturb", "bootstrap", "reference")),
          all(vapply(stages, is.function, logical(1))))
regenerate_summary <- function(fun, raw, filename) {
  environment$raw <- raw
  statements <- as.list(body(fun))[-1L]
  expression <- Filter(function(x) is.call(x) && identical(x[[1L]], quote(`<-`)) && identical(x[[2L]], quote(summary)), statements)
  stopifnot(length(expression) == 1L)
  rebuilt <- eval(expression[[1L]][[3L]], environment)
  old <- read.csv(file.path(output, filename))
  rownames(rebuilt) <- rownames(old) <- NULL
  stopifnot(isTRUE(all.equal(rebuilt, old, tolerance = 1e-12, check.attributes = FALSE)))
  write.csv(rebuilt, file.path(output, filename), row.names = FALSE)
  rebuilt
}

sampling <- readRDS(file.path(output, "sampling.rds"))
counts <- with(sampling$raw, table(scenario, engine, budget, target))
stopifnot(identical(dim(counts), c(4L, 2L, 3L, 4L)), all(counts == 12L), nrow(sampling$raw) == 1152L,
          length(sampling$design) == 4L, length(sampling$exact) == 4L)
sampling_summary <- regenerate_summary(stages$sampling, sampling$raw, "sampling-summary.csv")
probability_error <- vapply(names(sampling$design), function(name) {
  design <- sampling$design[[name]]; exact <- sampling$exact[[name]]$original
  source <- apply(exact$states, 1L, function(state)
    IsingSampler::IsingStateProb(state, design$W, design$thresholds, 1))
  max(abs(source - exact$probability))
}, numeric(1))
stopifnot(all(probability_error < 1e-12))

moderation <- read.csv(file.path(output, "moderation-raw.csv"))
details <- readRDS(file.path(output, "moderation-details.rds"))
stopifnot(nrow(moderation) == 36L, length(details) == 36L,
          all(table(moderation$scenario) == 12L), all(moderation$completed),
          all(moderation$valid_reps + moderation$failed_reps == 19L))
moderation_summary <- regenerate_summary(stages$moderation, moderation, "moderation-summary.csv")
sparse <- readRDS(file.path(output, "moderation-sparse.rds"))
stopifnot(length(sparse$results) == 40L)

# A one-event category fails the full-sample minimum-count check. Two events
# permit that fit, but participant resampling can remove the rare category.
set.seed(9511)
data <- cbind(A = c(1, 1, rep(0, 38)), B = rbinom(40, 1, .5), C = rbinom(40, 1, .5))
guard <- tryCatch(quicknet_nira_run_moderation(data, colnames(data), "AND", .25, 19L,
  quicknet_nira_make_streams(9612L, 1L)[[1L]], FALSE, 1L), error = function(e) list(error = conditionMessage(e)))
stopifnot(grepl("More than 20%.*\\(9/19\\)", guard$error))
saveRDS(list(data = data, nboot = 19L, result = guard), file.path(output, "moderation-two-event-guard.rds"), version = 2)

logs <- file.path(root, "..", "tmp", paste0("intervention-", c("sampling", "moderation"), ".log"))
for (log in logs[file.exists(logs)]) file.copy(log, file.path(output, basename(log)), overwrite = TRUE)
result <- list(status = "passed", date = as.character(Sys.time()),
  note = paste("Long calculations and saved numerical results were complete before an operational trailing-code error caused by editing an active Rscript file.",
               "Both original logs are retained. This clean process parses the frozen script, checks stage dispatch, reloads every saved case and regenerates both summaries without rerunning unchanged sampling."),
  script_sha256 = digest::digest(file = audit_script, algo = "sha256"),
  revalidation_sha256 = digest::digest(file = script, algo = "sha256"),
  sampling_rows = nrow(sampling$raw), sampling_configurations = 24L, sampling_summary_rows = nrow(sampling_summary),
  exact_probability_max_error = max(probability_error),
  moderation_datasets = nrow(moderation), successful_moderation_bootstraps = sum(moderation$valid_reps),
  sparse_worker_requested = length(sparse$results), sparse_worker_failed = sum(!vapply(sparse$results, `[[`, logical(1), "ok")),
  two_event_guard_error = guard$error,
  R = R.version.string, mgm = as.character(packageVersion("mgm")), IsingSampler = as.character(packageVersion("IsingSampler")))
saveRDS(result, file.path(output, "clean-revalidation.rds"), version = 2)
jsonlite::write_json(result, file.path(output, "clean-revalidation.json"), pretty = TRUE, auto_unbox = TRUE)
cat("Clean revalidation passed: 24 sampling configurations, 1152 target records, 36 moderation datasets, and the actual 9/19 bootstrap-failure guard.\n")
