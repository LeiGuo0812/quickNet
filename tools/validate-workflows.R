#!/usr/bin/env Rscript
# No args: run each README in a separate clean R process. Or pass README/output.
extract_blocks <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  first_example <- grep("^### 1\\.", lines)[[1L]]
  blocks <- list(); heading <- ""; i <- first_example
  while (i <= length(lines)) {
    if (grepl("^#{2,3} ", lines[[i]])) heading <- lines[[i]]
    if (grepl("^```r[[:space:]]*$", lines[[i]])) {
      end <- i + which(lines[(i + 1L):length(lines)] == "```")[[1L]]
      blocks[[length(blocks) + 1L]] <- list(heading = heading, first_line = i + 1L,
        last_line = end - 1L, code = lines[seq.int(i + 1L, end - 1L)])
      i <- end
    }
    i <- i + 1L
  }
  blocks
}
args <- commandArgs(trailingOnly = TRUE)
if (identical(args, "--record-prose-revision")) {
  for (root in c("../output/audit/workflows", "../output/audit/workflows-windows")) {
    for (name in c("README", "README.zh-CN")) {
      output <- file.path(root, name)
      if (!file.exists(file.path(output, "summary.json"))) next
      path <- paste0(name, ".md")
      blocks <- extract_blocks(path)
      current <- unlist(lapply(seq_along(blocks), function(j)
        c(paste0("# Block ", j, ": ", blocks[[j]]$heading), blocks[[j]]$code, "")))
      extracted <- file.path(output, "extracted-examples.R")
      stopifnot(identical(current, readLines(extracted, encoding = "UTF-8", warn = FALSE)))
      temporary <- tempfile()
      writeChar(paste0(paste(current, collapse = "\n"), "\n"), temporary, eos = NULL, useBytes = TRUE)
      executed <- jsonlite::read_json(file.path(output, "summary.json"))
      record <- list(executed_readme_md5 = executed$readme_md5,
        current_readme_md5 = unname(tools::md5sum(path)),
        extracted_code_md5 = unname(tools::md5sum(extracted)),
        normalized_LF_code_md5 = unname(tools::md5sum(temporary)), code_unchanged = TRUE,
        reason = "Non-executable prose revision after execution; extracted code and headings are unchanged after newline normalization.")
      jsonlite::write_json(record, file.path(output, "text-only-revision.json"), pretty = TRUE, auto_unbox = TRUE)
      unlink(temporary)
    }
  }
  message("Recorded prose revisions; executed code is unchanged.")
  quit(status = 0)
}
if (!length(args)) {
  parsed <- lapply(c("README.md", "README.zh-CN.md"), function(path) {
    code <- unlist(lapply(extract_blocks(path), `[[`, "code"))
    parse(text = code, keep.source = FALSE)
  })
  stopifnot(identical(parsed[[1L]], parsed[[2L]]))
  for (readme in c("README.md", "README.zh-CN.md")) {
    output <- file.path("../output/audit/workflows", sub("\\.md$", "", readme))
    status <- system2(file.path(R.home("bin"), "Rscript"), c("--vanilla",
      shQuote("tools/validate-workflows.R"), shQuote(readme), shQuote(output)))
    if (status != 0L) quit(status = status)
  }
  quit(status = 0)
}
readme <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
output <- if (length(args) >= 2L) args[[2L]] else file.path("../output/audit/workflows", sub("\\.md$", "", basename(readme)))
dir.create(output, recursive = TRUE, showWarnings = FALSE)
output <- normalizePath(output, winslash = "/", mustWork = TRUE)
Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1")
source_md5_at_start <- tools::md5sum(list.files("R", full.names = TRUE))
readme_md5_at_start <- unname(tools::md5sum(readme))
pkgload::load_all(".", quiet = TRUE)
blocks <- extract_blocks(readme)
writeLines(unlist(lapply(seq_along(blocks), function(j)
  c(paste0("# Block ", j, ": ", blocks[[j]]$heading), blocks[[j]]$code, ""))),
  file.path(output, "extracted-examples.R"), useBytes = TRUE)
grDevices::pdf(file.path(output, "workflow-plots.pdf"), width = 10, height = 8)
example_env <- new.env(parent = globalenv())
records <- list()
peak_rss <- function() {
  if (!file.exists("/proc/self/status")) return(NA_real_)
  line <- grep("^VmHWM:", readLines("/proc/self/status"), value = TRUE)
  if (length(line)) as.numeric(gsub("[^0-9]", "", line)) else NA_real_
}
write_records <- function() {
  jsonlite::write_json(records, file.path(output, "blocks.json"), pretty = TRUE, auto_unbox = TRUE, null = "null")
  saveRDS(records, file.path(output, "blocks.rds"))
}
for (j in seq_along(blocks)) {
  block <- blocks[[j]]
  message(basename(readme), " block ", j, "/", length(blocks), " ", block$heading)
  log <- file(file.path(output, sprintf("block-%02d.log", j)), open = "wt", encoding = "UTF-8")
  memory_file <- file.path(output, sprintf("block-%02d-memory.txt", j))
  profiled <- isTRUE(capabilities("profmem"))
  gc(verbose = FALSE)
  if (profiled) utils::Rprofmem(memory_file)
  started <- proc.time()
  warnings <- character()
  sink(log); sink(log, type = "message")
  error <- tryCatch(withCallingHandlers({
    expressions <- parse(text = block$code, keep.source = TRUE)
    for (expr in expressions) {
      value <- withVisible(eval(expr, envir = example_env))
      if (is.call(expr) && identical(expr[[1L]], as.name("$")) && is.null(value$value)) {
        stop("Displayed result field is NULL: ", paste(deparse(expr), collapse = " "))
      }
      if (value$visible) print(value$value)
    }
    NULL
  }, warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    cat("Warning: ", conditionMessage(w), "\n", sep = "")
    invokeRestart("muffleWarning")
  }), error = function(e) conditionMessage(e))
  elapsed <- proc.time() - started
  if (profiled) utils::Rprofmem(NULL)
  sink(type = "message"); sink(); close(log)
  allocation <- if (profiled) {
    rows <- readLines(memory_file, warn = FALSE)
    sum(as.numeric(sub(" .*", "", rows[grepl("^[0-9]+ ", rows)])))
  } else NA_real_
  unlink(memory_file)
  records[[j]] <- list(block = j, heading = block$heading, first_line = block$first_line,
    last_line = block$last_line, passed = is.null(error), error = error,
    elapsed_seconds = unname(elapsed[["elapsed"]]), user_seconds = unname(elapsed[["user.self"]]),
    allocated_R_bytes = allocation, process_peak_RSS_kB = peak_rss(), warnings = unique(warnings))
  write_records()
  if (!is.null(error)) {
    grDevices::dev.off()
    stop("README block ", j, " failed: ", error, call. = FALSE)
  }
}
grDevices::dev.off()
# Verify reports and serialized/exported artifacts from the public examples.
fits <- Filter(function(name) inherits(get(name, example_env), "quicknet_fit"), ls(example_env))
checks <- lapply(fits, function(name) {
  fit <- get(name, example_env)
  report <- quicknet_report(fit)
  if (!is.null(fit$diagnostics)) stopifnot(!any(fit$diagnostics$status == "failed"))
  stopifnot(is.list(report), length(report$text) > 0,
    all(vapply(fit$networks, function(m) is.matrix(m) && all(is.finite(m)), logical(1))))
  list(object = name, model = fit$model, layers = names(fit$networks),
       estimation = report$estimation, diagnostics = fit$diagnostics)
})
saveRDS(mget(fits, example_env), file.path(output, "fitted-models.rds"), compress = "xz")
stopifnot(exists("export_dir", example_env, inherits = FALSE))
export_dir <- get("export_dir", example_env)
stopifnot(file.exists(file.path(export_dir, "edges.csv")), file.exists(file.path(export_dir, "report.txt")))
exported <- list.files(export_dir, full.names = TRUE)
stopifnot(any(grepl("\\.(pdf|png)$", exported)), all(file.info(exported)$size > 0))
dir.create(file.path(output, "exports"), showWarnings = FALSE)
file.copy(exported, file.path(output, "exports"), overwrite = TRUE)
summary <- list(readme = basename(readme), readme_md5 = readme_md5_at_start,
  extracted_code_md5 = unname(tools::md5sum(file.path(output, "extracted-examples.R"))),
  R = R.version.string, platform = R.version$platform, total_blocks = length(blocks),
  passed_blocks = sum(vapply(records, `[[`, logical(1), "passed")),
  elapsed_seconds = sum(vapply(records, `[[`, numeric(1), "elapsed_seconds")),
  nira_status = get("nira_result", example_env)$status,
  allocated_R_bytes = sum(vapply(records, `[[`, numeric(1), "allocated_R_bytes")),
  process_peak_RSS_kB = peak_rss(), fits = checks, exports = basename(exported),
  excluded = "Installation, API overview and input-validation templates before numbered Example 1 require user-specific setup/data and are not executable examples.",
  measurement = "Per-block Rprofmem allocation; cumulative process RSS high-water mark on Linux. Native/child-process allocation is not counted by Rprofmem; RSS is unavailable here on non-Linux platforms.")
jsonlite::write_json(summary, file.path(output, "summary.json"), pretty = TRUE, auto_unbox = TRUE)
saveRDS(list(summary = summary, records = records, session = sessionInfo(),
  source_md5 = source_md5_at_start), file.path(output, "provenance.rds"))
writeLines(capture.output(sessionInfo()), file.path(output, "session-info.txt"))
message("PASS: ", basename(readme), " (", length(blocks), " blocks)")
