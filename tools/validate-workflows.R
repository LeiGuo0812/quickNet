#!/usr/bin/env Rscript
# Every analysis fence runs in its own Rscript --vanilla process.
# No args: both READMEs. Or: README.md [output] [--blocks=1,2].
# --check-only validates extraction and bilingual parity without running examples.
# --record-prose-revision updates only the new independent-run records.
local({
  execution_mode <- "independent-blocks-v1"
  script_arg <- grep("^--file=", commandArgs(), value = TRUE)[[1L]]
  script_path <- normalizePath(sub("^--file=", "", script_arg), winslash = "/", mustWork = TRUE)
  source_root <- dirname(dirname(script_path))
  output_root <- file.path(dirname(source_root), "output", "audit",
    if (.Platform$OS.type == "windows") "workflows-independent-windows" else "workflows-independent")

  extract_blocks <- function(path) {
    lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
    blocks <- list(); heading <- ""; i <- 1L
    while (i <= length(lines)) {
      if (grepl("^#{2,3} ", lines[[i]])) heading <- lines[[i]]
      if (grepl("^```[rR][[:space:]]*$", lines[[i]])) {
        endings <- which(lines == "```" & seq_along(lines) > i)
        if (!length(endings)) stop("Unclosed R fence in ", path, " at line ", i)
        end <- endings[[1L]]
        code <- lines[seq.int(i + 1L, end - 1L)]
        installation <- grepl("(devtools|remotes)::install_(github|local)[[:space:]]*\\(",
                              paste(code, collapse = "\n"))
        blocks[[length(blocks) + 1L]] <- list(fence = length(blocks) + 1L,
          heading = heading, first_line = i + 1L, last_line = end - 1L,
          code = code, installation = installation)
        i <- end
      }
      i <- i + 1L
    }
    excluded <- which(vapply(blocks, `[[`, logical(1), "installation"))
    if (!identical(excluded, 1:2)) {
      stop("Expected exactly the first two R fences to be installation commands in ", path)
    }
    list(analysis = blocks[-excluded], excluded = blocks[excluded])
  }
  parse_block <- function(block) parse(text = block$code, keep.source = FALSE)
  loads_quicknet <- function(expressions) any(vapply(as.list(expressions), function(expr) {
    is.call(expr) && length(expr) >= 2L &&
      paste(deparse(expr[[1L]]), collapse = "") %in% c("library", "base::library") &&
      identical(as.character(expr[[2L]]), "quickNet")
  }, logical(1)))
  check_bilingual <- function() {
    paths <- file.path(source_root, c("README.md", "README.zh-CN.md"))
    plans <- lapply(paths, extract_blocks)
    if (length(plans[[1L]]$analysis) != length(plans[[2L]]$analysis)) {
      stop("The two READMEs have different numbers of analysis fences.")
    }
    for (i in seq_along(plans[[1L]]$analysis)) {
      a <- parse_block(plans[[1L]]$analysis[[i]])
      b <- parse_block(plans[[2L]]$analysis[[i]])
      if (!identical(a, b)) stop("Bilingual parsed expressions differ in analysis block ", i)
      if (!loads_quicknet(a)) stop("Analysis block ", i, " must load library(quickNet) explicitly.")
    }
    plans
  }
  extracted_lines <- function(blocks) unlist(lapply(seq_along(blocks), function(j)
    c(paste0("# Analysis block ", j, "; R fence ", blocks[[j]]$fence, ": ", blocks[[j]]$heading),
      blocks[[j]]$code, "")))
  write_json <- function(value, path) jsonlite::write_json(value, path,
    pretty = TRUE, auto_unbox = TRUE, null = "null", na = "null")
  md5 <- function(path) unname(tools::md5sum(path))
  source_hashes <- function() tools::md5sum(list.files(file.path(source_root, "R"), full.names = TRUE))
  peak_rss <- function() {
    if (!file.exists("/proc/self/status")) return(NA_real_)
    line <- grep("^VmHWM:", readLines("/proc/self/status"), value = TRUE)
    if (length(line)) as.numeric(gsub("[^0-9]", "", line)) else NA_real_
  }
  package_versions <- function() {
    namespaces <- sort(loadedNamespaces())
    data.frame(package = namespaces, version = vapply(namespaces,
      function(name) as.character(utils::packageVersion(name)), character(1)))
  }

  run_worker <- function(payload_path) {
    payload <- readRDS(payload_path)
    block <- payload$block
    output <- payload$output
    Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1")
    setwd(source_root)
    warnings <- character(); stage <- "source_loading"; profiled <- FALSE
    elapsed <- allocation <- NA_real_; load_seconds <- validation_seconds <- NA_real_
    task_start <- proc.time()[["elapsed"]]
    source_md5 <- source_hashes()
    record <- list(execution_mode = execution_mode, block = payload$index,
      fence = block$fence, heading = block$heading, first_line = block$first_line,
      last_line = block$last_line, pid = Sys.getpid(), R = R.version.string,
      platform = R.version$platform, readme_md5 = payload$readme_md5,
      code_md5 = md5(file.path(output, "code.R")), source_md5 = as.list(source_md5),
      runner_md5 = md5(script_path))
    error <- tryCatch(withCallingHandlers({
      if (!identical(source_md5, payload$source_md5)) stop("R source changed after this run was prepared.")
      started <- proc.time()[["elapsed"]]
      # Public exports only: examples must also work with library(quickNet).
      pkgload::load_all(source_root, quiet = TRUE, export_all = FALSE,
                        helpers = FALSE, attach_testthat = FALSE)
      namespace_path <- normalizePath(getNamespaceInfo("quickNet", "path"), winslash = "/", mustWork = TRUE)
      if (!identical(namespace_path, source_root)) stop("The loaded quickNet namespace is not the working source.")
      record$quicknet_namespace_path <- namespace_path
      record$initial_global_objects <- ls(.GlobalEnv, all.names = TRUE)
      load_seconds <- proc.time()[["elapsed"]] - started
      working_dir <- tempfile("session-", tmpdir = output)
      dir.create(working_dir)
      record$working_directory <- working_dir
      setwd(working_dir)
      grDevices::pdf(file.path(output, "plots.pdf"), width = 10, height = 8)
      stage <- "example_execution"
      expressions <- parse_block(block)
      if (!loads_quicknet(expressions)) stop("This standalone example does not explicitly load quickNet.")
      memory_file <- file.path(output, "memory.txt")
      profiled <- isTRUE(capabilities("profmem"))
      if (profiled) utils::Rprofmem(memory_file)
      started <- proc.time()[["elapsed"]]
      execution_error <- tryCatch({
        for (expr in expressions) {
          value <- withVisible(eval(expr, envir = .GlobalEnv))
          if (is.call(expr) && identical(expr[[1L]], as.name("$")) && is.null(value$value)) {
            stop("Displayed result field is NULL: ", paste(deparse(expr), collapse = " "))
          }
          if (value$visible) print(value$value)
        }
        NULL
      }, error = function(e) conditionMessage(e))
      elapsed <- proc.time()[["elapsed"]] - started
      if (profiled) {
        utils::Rprofmem(NULL); profiled <- FALSE
        rows <- readLines(memory_file, warn = FALSE)
        allocation <- sum(as.numeric(sub(" .*", "", rows[grepl("^[0-9]+ ", rows)])))
        unlink(memory_file)
      }
      if (!is.null(execution_error)) stop(execution_error, call. = FALSE)
      grDevices::graphics.off()
      stage <- "result_validation"
      started <- proc.time()[["elapsed"]]
      objects <- ls(.GlobalEnv, all.names = FALSE)
      fit_names <- Filter(function(name) inherits(get(name, .GlobalEnv), "quicknet_fit"), objects)
      record$fits <- lapply(fit_names, function(name) {
        fit <- get(name, .GlobalEnv)
        report <- quickNet::quicknet_report(fit)
        if (!is.null(fit$diagnostics)) stopifnot(!any(fit$diagnostics$status == "failed"))
        stopifnot(is.list(report), length(report$text) > 0L,
          all(vapply(fit$networks, function(m) is.matrix(m) && all(is.finite(m)), logical(1))))
        list(object = name, model = fit$model, layers = names(fit$networks),
          estimation = report$estimation, diagnostics = fit$diagnostics)
      })
      if (length(fit_names)) saveRDS(mget(fit_names, .GlobalEnv), file.path(output, "fitted-models.rds"), compress = "xz")
      result_names <- Filter(function(name) {
        value <- get(name, .GlobalEnv)
        inherits(value, c("quicknet_nira", "quicknet_power", "quicknet_perturbation"))
      }, objects)
      record$results <- lapply(result_names, function(name) {
        value <- get(name, .GlobalEnv)
        report <- quickNet::quicknet_report(value)
        stopifnot(is.list(report), length(report$text) > 0L)
        list(object = name, class = class(value), status = value$status %||% NULL)
      })
      if (length(result_names)) saveRDS(mget(result_names, .GlobalEnv), file.path(output, "analysis-results.rds"), compress = "xz")
      if (exists("export_dir", .GlobalEnv, inherits = FALSE)) {
        export_dir <- get("export_dir", .GlobalEnv)
        exported <- list.files(export_dir, full.names = TRUE)
        stopifnot(length(exported) > 0L, all(file.info(exported)$size > 0),
          any(grepl("\\.(pdf|svg|png)$", exported)), any(grepl("\\.csv$", exported)),
          any(grepl("\\.txt$", exported)), !anyDuplicated(basename(exported)))
        destination <- file.path(output, "exports")
        dir.create(destination, showWarnings = FALSE)
        stopifnot(all(file.copy(exported, destination, overwrite = TRUE)))
        record$exports <- data.frame(file = basename(exported), bytes = file.info(exported)$size,
          md5 = unname(tools::md5sum(exported)))
      }
      validation_seconds <- proc.time()[["elapsed"]] - started
      NULL
    }, warning = function(w) {
      warnings <<- c(warnings, paste0(stage, ": ", conditionMessage(w)))
      cat("Warning [", stage, "]: ", conditionMessage(w), "\n", sep = "")
      invokeRestart("muffleWarning")
    }), error = function(e) conditionMessage(e))
    if (profiled) utils::Rprofmem(NULL)
    grDevices::graphics.off()
    record$passed <- is.null(error); record$error <- error
    record$failed_stage <- if (!is.null(error)) stage else NULL
    record$elapsed_seconds <- elapsed; record$source_load_seconds <- load_seconds
    record$validation_seconds <- validation_seconds
    record$worker_elapsed_seconds <- proc.time()[["elapsed"]] - task_start
    record$allocated_R_bytes <- allocation; record$process_peak_RSS_kB <- peak_rss()
    record$warnings <- unique(warnings); record$versions <- package_versions()
    saveRDS(record, file.path(output, "result.rds"))
    write_json(record, file.path(output, "result.json"))
    saveRDS(sessionInfo(), file.path(output, "session-info.rds"))
    writeLines(capture.output(sessionInfo()), file.path(output, "session-info.txt"))
    is.null(error)
  }
  `%||%` <- function(x, y) if (is.null(x)) y else x

  run_readme <- function(path, output, selected = NULL) {
    path <- normalizePath(path, winslash = "/", mustWork = TRUE)
    plan <- extract_blocks(path); blocks <- plan$analysis
    dir.create(output, recursive = TRUE, showWarnings = FALSE)
    output <- normalizePath(output, winslash = "/", mustWork = TRUE)
    existing <- file.path(output, "summary.json")
    if (file.exists(existing) && !identical(jsonlite::read_json(existing)$execution_mode, execution_mode)) {
      stop("Refusing to overwrite a historical sequential-workflow result: ", output)
    }
    indices <- if (is.null(selected)) seq_along(blocks) else selected
    if (!length(indices) || anyNA(indices) || any(!indices %in% seq_along(blocks)) || anyDuplicated(indices)) {
      stop("--blocks must select unique analysis block numbers.")
    }
    source_md5 <- source_hashes(); readme_md5 <- md5(path)
    writeLines(extracted_lines(blocks), file.path(output, "extracted-examples.R"), useBytes = TRUE)
    saveRDS(plan, file.path(output, "extraction.rds"))
    records <- list()
    for (j in indices) {
      block <- blocks[[j]]
      message(basename(path), " independent block ", j, "/", length(blocks), " ", block$heading)
      block_dir <- file.path(output, sprintf("block-%02d", j))
      dir.create(block_dir, showWarnings = FALSE)
      writeLines(block$code, file.path(block_dir, "code.R"), useBytes = TRUE)
      payload <- list(index = j, block = block, output = block_dir,
        source_md5 = source_md5, readme_md5 = readme_md5)
      payload_path <- file.path(block_dir, "payload.rds")
      saveRDS(payload, payload_path)
      result_path <- file.path(block_dir, "result.rds")
      # An interrupted or failed process cannot reuse an earlier success record.
      unlink(file.path(block_dir, c("result.rds", "result.json", "plots.pdf",
        "fitted-models.rds", "analysis-results.rds", "session-info.rds", "session-info.txt")))
      unlink(file.path(block_dir, "exports"), recursive = TRUE)
      log <- file.path(block_dir, "execution.log")
      code <- system2(file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"),
        c("--vanilla", shQuote(script_path), "--block-worker", shQuote(payload_path)), stdout = log, stderr = log)
      record <- if (file.exists(result_path)) readRDS(result_path) else list(
        execution_mode = execution_mode, block = j, fence = block$fence, heading = block$heading,
        passed = FALSE, error = "Worker exited without a result record; inspect execution.log.")
      record$exit_status <- code
      if (code != 0L) record$passed <- FALSE
      records[[length(records) + 1L]] <- record
      saveRDS(records, file.path(output, "blocks.rds"))
      write_json(records, file.path(output, "blocks.json"))
    }
    passed <- vapply(records, function(x) isTRUE(x$passed), logical(1))
    summary <- list(execution_mode = execution_mode, readme = basename(path), readme_md5 = readme_md5,
      extracted_code_md5 = md5(file.path(output, "extracted-examples.R")), runner_md5 = md5(script_path),
      total_blocks = length(blocks), selected_blocks = indices, executed_blocks = length(records),
      passed_blocks = sum(passed), all_blocks_executed = length(indices) == length(blocks),
      excluded = lapply(plan$excluded, function(x) list(fence = x$fence, heading = x$heading,
        first_line = x$first_line, last_line = x$last_line, reason = "Package installation command")),
      source_md5 = as.list(source_md5), R = R.version.string, platform = R.version$platform,
      process_ids = vapply(records, function(x) x$pid %||% NA_integer_, integer(1)),
      measurement = "Each analysis block has a fresh R process. Per-block elapsed time excludes source loading and artifact checks. Rprofmem excludes native/child allocation. Linux RSS is the worker high-water mark; Windows RSS is unavailable.")
    write_json(summary, file.path(output, "summary.json"))
    saveRDS(list(summary = summary, records = records, extraction = plan), file.path(output, "provenance.rds"))
    message(if (all(passed)) "PASS: " else "FAIL: ", basename(path), " (", sum(passed), "/", length(records), " independent blocks)")
    all(passed)
  }
  record_prose_revision <- function() {
    for (root in file.path(dirname(source_root), "output", "audit",
                          c("workflows-independent", "workflows-independent-windows"))) {
      for (name in c("README", "README.zh-CN")) {
        output <- file.path(root, name)
        summary_path <- file.path(output, "summary.json")
        if (!file.exists(summary_path)) next
        executed <- jsonlite::read_json(summary_path)
        if (!identical(executed$execution_mode, execution_mode)) stop("Result is not an independent-block run: ", output)
        path <- file.path(source_root, paste0(name, ".md"))
        current <- extracted_lines(extract_blocks(path)$analysis)
        extracted <- file.path(output, "extracted-examples.R")
        stopifnot(identical(current, readLines(extracted, encoding = "UTF-8", warn = FALSE)))
        temporary <- tempfile()
        writeChar(paste0(paste(current, collapse = "\n"), "\n"), temporary, eos = NULL, useBytes = TRUE)
        write_json(list(execution_mode = execution_mode, executed_readme_md5 = executed$readme_md5,
          current_readme_md5 = md5(path), extracted_code_md5 = md5(extracted),
          normalized_LF_code_md5 = md5(temporary), code_unchanged = TRUE,
          reason = "Only non-executable prose changed; independent blocks are unchanged after newline normalization."),
          file.path(output, "text-only-revision.json"))
        unlink(temporary)
      }
    }
    message("Recorded prose revisions for independent runs only; historical sequential runs were not modified.")
  }

  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) && args[[1L]] == "--block-worker") {
    if (length(args) != 2L) stop("--block-worker requires one payload RDS path.")
    quit(status = if (run_worker(args[[2L]])) 0L else 1L)
  }
  if (identical(args, "--record-prose-revision")) {
    record_prose_revision(); quit(status = 0L)
  }
  plans <- check_bilingual()
  if (identical(args, "--check-only")) {
    message("PASS: bilingual parsed expressions match; ", length(plans[[1L]]$analysis),
      " independent analysis blocks each; only two installation fences excluded.")
    quit(status = 0L)
  }
  block_flag <- grep("^--blocks=", args, value = TRUE)
  if (length(block_flag) > 1L) stop("Supply at most one --blocks option.")
  selected <- if (length(block_flag)) as.integer(strsplit(sub("^--blocks=", "", block_flag), ",", fixed = TRUE)[[1L]]) else NULL
  args <- args[!grepl("^--blocks=", args)]
  if (length(args) > 2L) stop("Usage: validate-workflows.R [README.md [output]] [--blocks=1,2]")
  if (!length(args)) {
    success <- logical()
    for (name in c("README", "README.zh-CN")) {
      success <- c(success, run_readme(file.path(source_root, paste0(name, ".md")), file.path(output_root, name), selected))
    }
    quit(status = if (all(success)) 0L else 1L)
  }
  path <- args[[1L]]
  output <- if (length(args) == 2L) args[[2L]] else file.path(output_root, sub("\\.md$", "", basename(path)))
  quit(status = if (run_readme(path, output, selected)) 0L else 1L)
})
