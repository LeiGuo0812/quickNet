#!/usr/bin/env Rscript
# Cairo PDF and SVG exercise multilingual labels without depending on base PDF fonts.
args <- commandArgs(trailingOnly = TRUE)
out <- if (length(args)) args[[1]] else "../output/audit/graphics"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
pkgload::load_all(quiet = TRUE)
stopifnot(capabilities("cairo"))
labels <- c("睡眠质量", "Anxiety / long descriptive label", "情绪低落", "Isolated")
w <- matrix(c(0, -.4, .2, 0, -.4, 0, .3, 0, .2, .3, 0, 0, 0, 0, 0, 0), 4,
            dimnames = list(labels, labels))
warnings <- character()
withCallingHandlers({
  for (case in c("signed", "empty", "ties", "directed")) {
    mat <- switch(case, signed = w, empty = w * 0, ties = abs(w) * 0 + .2, directed = {
      value <- w; value[upper.tri(value)] <- 0; diag(value) <- c(.2, .3, .4, .5); value
    })
    if (case == "ties") diag(mat) <- 0
    cached <- qgraph::qgraph(if (case == "directed") t(mat) else mat,
      directed = case == "directed", labels = 1:4, nodeNames = labels,
      legend = TRUE, layout = "circle", DoNotPlot = TRUE)
    fit <- quicknet_fit(if (case == "directed") "clpn" else "correlation", networks = list(default = mat),
                       plots = list(network = cached))
    centrality <- Centrality(fit, print = FALSE)
    bridge <- Bridge(fit, communities = c("A", "A", "B", "B"), include = "all")
    for (device in c("cairo_pdf", "svg")) {
      prefix <- paste(case, device, sep = "_")
      get_network_plot(fit, path = out, prefix = prefix, device = device, family = "sans")
      get_centrality_plot(centrality, path = out, prefix = prefix, device = device)
      get_bridge_plot(bridge, path = out, prefix = prefix, device = device)
      exported <- as.matrix(read.csv(file.path(out, paste0(prefix, "_network_matrix.csv")), row.names = 1, check.names = FALSE))
      stopifnot(isTRUE(all.equal(exported, mat)))
    }
  }
}, warning = function(w) {
  warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning")
})
writeLines(warnings, file.path(out, "warnings.txt"))
writeLines(capture.output(sessionInfo()), file.path(out, "session-info.txt"))
stopifnot(!any(grepl("conversion failure|font.*not found|invalid font|Removed [0-9]+ rows", warnings)))
cat("24 PDF/SVG plots and their exported matrices/tables checked.\n")
