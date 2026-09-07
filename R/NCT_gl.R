#' @importFrom graphics hist points
#' @importFrom stats cor p.adjust
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom Matrix nearPD
#' @importFrom reshape2 melt
#' @importFrom stats na.omit
#' @importFrom methods is
#' @import qgraph
#' @import IsingFit
#' @importFrom networktools bridge

NCT_gl = function (data1, data2, gamma, it = 100, binary.data = FALSE,
          paired = FALSE, weighted = TRUE, AND = TRUE, abs = TRUE,
          test.edges = FALSE, edges = "all", progressbar = TRUE, make.positive.definite = TRUE,
          p.adjust.methods = c("none", "holm", "hochberg", "hommel",
                               "bonferroni", "BH", "BY", "fdr"), test.centrality = FALSE,
          centrality = c("strength", "expectedInfluence"), nodes = "all",
          communities = NULL, useCommunities = "all", estimator, estimatorArgs = list(),
          verbose = TRUE)
{
  if (!quicknet_is_positive_integer(it)) {
    stop("it must be a positive integer.", call. = FALSE)
  }
  p.adjust.methods <- match.arg(p.adjust.methods)
  logical_arguments <- list(
    binary.data = binary.data,
    paired = paired,
    weighted = weighted,
    test.edges = test.edges,
    progressbar = progressbar,
    test.centrality = test.centrality
  )
  invalid_logical <- names(logical_arguments)[!vapply(
    logical_arguments,
    function(value) is.logical(value) && length(value) == 1 && !is.na(value),
    logical(1)
  )]
  if (length(invalid_logical) > 0) {
    stop(
      paste(invalid_logical, collapse = ", "),
      " must be single non-missing logical values.",
      call. = FALSE
    )
  }
  if (missing(edges))
    edges <- "all"
  if (is(data1, "bootnetResult") || is(data2, "bootnetResult")) {

    if (!missing(estimator)) {
      stop("Custom estimator function not supported for bootnet objects.")
    }
  }
  if (is(data1, "bootnetResult")) {
    if (verbose)
      message("Note: estimateNetwork object used - estimation method has possibly not been validated.")
    estimator <- data1$estimator
    estimatorArgs <- data1$arguments
    estimatorArgs$verbose <- FALSE
    data1 <- data1$data
  }
  if (is(data2, "bootnetResult")) {
    estimator2 <- data2$estimator
    estimatorArgs2 <- data2$arguments
    estimatorArgs2$verbose <- FALSE
    if (missing(estimator)) {
      estimator <- estimator2
      estimatorArgs <- estimatorArgs2
    }
    if (!identical(estimator, estimator2)) {
      stop("Estimation methods are not identical.")
    }
    if (!identical(estimatorArgs, estimatorArgs2)) {
      stop("Estimation arguments are not identical.")
    }
    data2 <- data2$data
  }
  if (missing(gamma)) {
    if (binary.data) {
      gamma <- 0.25
    }
    else {
      gamma <- 0.5
    }
  }
  if (missing(estimator)) {
    if (binary.data) {
      estimator <- NCT_estimator_Ising
      estimatorArgs$AND <- AND
    }
    else {
      estimator <- NCT_estimator_GGM
      estimatorArgs$make.positive.definite <- make.positive.definite
    }
    estimatorArgs$gamma <- gamma
  }
  else {
    mc <- match.call()
    if ("binary.data" %in% names(mc)) {
      if (verbose)
        message("Note: Both 'estimator' and 'binary.data' arguments used: only the 'estimator' will be used ('binary.data' will be ignored)")
    }
  }
  if (progressbar == TRUE) {
    pb <- txtProgressBar(max = it, style = 3)
    on.exit(try(close(pb), silent = TRUE), add = TRUE)
  }
  x1 <- data1
  x2 <- data2
  valid_data <- vapply(
    list(x1, x2),
    function(x) is.data.frame(x) || is.matrix(x),
    logical(1)
  )
  if (!all(valid_data)) {
    stop("data1 and data2 must be data frames or matrices.", call. = FALSE)
  }
  if (ncol(x1) != ncol(x2)) {
    stop("data1 and data2 must contain the same number of variables.", call. = FALSE)
  }
  if (ncol(x1) < 2) {
    stop("At least two variables are required for network comparison.", call. = FALSE)
  }
  if (nrow(x1) < 3 || nrow(x2) < 3) {
    stop("Each dataset must contain at least three observations.", call. = FALSE)
  }
  names1 <- colnames(x1)
  names2 <- colnames(x2)
  if (is.null(names1) && is.null(names2)) {
    names1 <- names2 <- paste0("var", seq_len(ncol(x1)))
  } else if (is.null(names1)) {
    names1 <- names2
  } else if (is.null(names2)) {
    names2 <- names1
  }
  if (anyDuplicated(names1) || anyDuplicated(names2)) {
    stop("Variable names must be unique in both datasets.", call. = FALSE)
  }
  if (!setequal(names1, names2)) {
    stop("data1 and data2 must contain the same named variables.", call. = FALSE)
  }
  colnames(x1) <- names1
  colnames(x2) <- names2
  x2 <- x2[, names1, drop = FALSE]
  if (isTRUE(paired) && nrow(x1) != nrow(x2)) {
    stop("Paired network comparison requires equal numbers of rows.", call. = FALSE)
  }
  if (isTRUE(binary.data)) {
    binary_columns <- c(as.list(as.data.frame(x1)), as.list(as.data.frame(x2)))
    binary_valid <- vapply(
      binary_columns,
      function(column) all(is.finite(column)) && all(unique(column) %in% c(0, 1)),
      logical(1)
    )
    if (!all(binary_valid)) {
      stop("binary.data = TRUE requires complete variables coded 0/1.", call. = FALSE)
    }
    combined <- as.data.frame(rbind(x1, x2))
    category_counts <- vapply(
      combined,
      function(column) min(sum(column == 0), sum(column == 1)),
      numeric(1)
    )
    if (nrow(x1) < 4 || nrow(x2) < 4 || any(category_counts < 4)) {
      stop(
        "Binary permutation requires at least four observations per group and at least four total observations in each category for every variable.",
        call. = FALSE
      )
    }
  }
  nobs1 <- nrow(x1)
  nobs2 <- nrow(x2)
  dataall <- rbind(x1, x2)
  b <- seq_len(nobs1 + nobs2)
  nvars <- ncol(x1)
  nedges <- nvars * (nvars - 1)/2
  all_nodes <- is.character(nodes) && length(nodes) == 1 && identical(tolower(nodes), "all")
  nnodes <- if (all_nodes) nvars else length(nodes)
  nodes <- if (is.numeric(nodes)) {
    if (any(!is.finite(nodes)) || any(nodes < 1 | nodes > nvars) || any(nodes != floor(nodes))) {
      stop("Numeric nodes must be valid variable indices.", call. = FALSE)
    }
    colnames(x1)[nodes]
  }
  else {
    nodes
  }
  if (!all_nodes && !all(nodes %in% colnames(x1))) {
    stop("nodes must name variables present in both datasets.", call. = FALSE)
  }
  if (!all_nodes && (length(nodes) == 0 || anyDuplicated(nodes))) {
    stop("nodes must contain one or more unique variables.", call. = FALSE)
  }
  if (is.list(edges)) {
    if (length(edges) == 0) {
      stop("edges must contain at least one variable pair.", call. = FALSE)
    }
    edges.tested <- edges
    if (is.character(edges[[1]])) {
      whichfun <- function(x) {
        match(x, colnames(x1))
      }
      edges <- lapply(edges, whichfun)
    }
    valid_edges <- vapply(
      edges,
      function(edge) is.numeric(edge) && length(edge) == 2 && all(is.finite(edge)) &&
        all(edge == floor(edge)) && all(edge >= 1 & edge <= nvars) && edge[[1]] != edge[[2]],
      logical(1)
    )
    if (!all(valid_edges)) {
      stop("Each requested edge must contain two distinct valid variables.", call. = FALSE)
    }
  } else if (!is.character(edges) || length(edges) != 1 || !identical(tolower(edges), "all")) {
    stop("edges must be 'all' or a list of variable pairs.", call. = FALSE)
  }
  glstrinv.perm <- glstrinv.real <- nwinv.real <- nwinv.perm <- c()
  diffedges.perm <- if (test.edges) matrix(0, it, nedges) else NULL
  einv.perm.all <- if (test.edges) array(NA, dim = c(nvars, nvars, it)) else NULL
  corrpvals.all <- matrix(NA, nvars, nvars)
  edges.pvalmattemp <- matrix(0, nvars, nvars)
  validCentrality <- c("closeness", "betweenness", "strength",
                       "expectedInfluence", "bridgeStrength", "bridgeCloseness",
                       "bridgeBetweenness", "bridgeExpectedInfluence")
  bridgecen <- c("bridgeStrength", "bridgeBetweenness", "bridgeCloseness",
                 "bridgeExpectedInfluence")
  centrality <- if (is.character(centrality) && length(centrality) == 1 &&
    identical(tolower(centrality), "all")) {
    validCentrality
  }
  else {
    centrality
  }
  if (test.centrality && any(bridgecen %in% centrality) && is.null(communities)) {
    stop("communities must be provided when testing bridge centrality.", call. = FALSE)
  }
  diffcen.perm <- matrix(NA, it, nnodes * length(centrality))
  nw1 <- do.call(estimator, c(list(x1), estimatorArgs))
  if (is.list(nw1))
    nw1 <- nw1$graph
  nw2 <- do.call(estimator, c(list(x2), estimatorArgs))
  if (is.list(nw2))
    nw2 <- nw2$graph
  nw1 <- as.matrix(nw1)
  nw2 <- as.matrix(nw2)
  if (!all(dim(nw1) == c(nvars, nvars)) || !all(dim(nw2) == c(nvars, nvars))) {
    stop("The estimator must return one square network matrix per dataset.", call. = FALSE)
  }
  if (any(!is.finite(nw1)) || any(!is.finite(nw2))) {
    stop("The estimator returned missing or non-finite network weights.", call. = FALSE)
  }
  colnames(nw1) <- rownames(nw1) <- colnames(x1)
  colnames(nw2) <- rownames(nw2) <- colnames(x1)
  if (weighted == FALSE) {
    nw1 = (nw1 != 0) * 1
    nw2 = (nw2 != 0) * 1
  }
  if (abs) {
    glstrinv.real <- abs(sum(abs(nw1[upper.tri(nw1)])) -
                           sum(abs(nw2[upper.tri(nw2)])))
    glstrinv.sep <- c(sum(abs(nw1[upper.tri(nw1)])), sum(abs(nw2[upper.tri(nw2)])))
  }
  else {
    glstrinv.real <- abs(sum(nw1[upper.tri(nw1)]) - sum(nw2[upper.tri(nw2)]))
    glstrinv.sep <- c(sum(nw1[upper.tri(nw1)]), sum(nw2[upper.tri(nw2)]))
  }
  diffedges.real <- abs(nw1 - nw2)[upper.tri(abs(nw1 - nw2))]
  diffedges.realmat <- if (test.edges) {
    matrix(diffedges.real, it, nedges, byrow = TRUE)
  } else {
    NULL
  }
  diffedges.realoutput <- abs(nw1 - nw2)
  nwinv.real <- max(diffedges.real)
  if (test.centrality == TRUE) {
    if (!all(centrality %in% validCentrality)) {
      stop(paste0("'centrality' must be one of: ", paste0("'",
                                                          validCentrality, "'", collapse = ", ")))
    }
    cen1 <- qgraph::centrality_auto(nw1)$node.centrality
    cen2 <- qgraph::centrality_auto(nw2)$node.centrality
    names(cen1) <- names(cen2) <- c("betweenness", "closeness",
                                    "strength", "expectedInfluence")
    if (TRUE %in% (bridgecen %in% centrality)) {
      b1 <- networktools::bridge(nw1, communities = communities,
                                 useCommunities = useCommunities)
      b2 <- networktools::bridge(nw2, communities = communities,
                                 useCommunities = useCommunities)
      names(b1) <- names(b2) <- c(bridgecen, "bridgeExpectedInfluence2step",
                                  "communities")
      b1$communities <- b2$communities <- NULL
      cen1 <- data.frame(c(cen1, b1))
      cen2 <- data.frame(c(cen2, b2))
    }
    diffcen.real <- as.matrix(cen1) - as.matrix(cen2)
  }
  if (paired == TRUE) {
    if (verbose)
      message("Note: NCT for dependent data has not been validated.")
  }
  for (i in seq_len(it)) {
    diffedges.permtemp <- if (test.edges) matrix(0, nvars, nvars) else NULL
    if (paired == FALSE) {
      okay <- FALSE
      counter <- 0
      if (binary.data) {
        while (!okay && counter < 10000) {
          s <- sample(seq_len(nobs1 + nobs2), nobs1, replace = FALSE)
          x1perm <- dataall[s, ]
          x2perm <- dataall[b[-s], ]
          okay <- NCT_binary_group_valid(x1perm) && NCT_binary_group_valid(x2perm)
          if (!okay) counter <- counter + 1
        }
        if (!okay) {
          stop(
            "Could not generate a valid binary permutation after 10,000 attempts; category counts are too sparse.",
            call. = FALSE
          )
        }
      }
      else {
        s <- sample(seq_len(nobs1 + nobs2), nobs1, replace = FALSE)
        x1perm <- dataall[s, ]
        x2perm <- dataall[b[-s], ]
      }
      r1perm <- do.call(estimator, c(list(x1perm), estimatorArgs))
      if (is.list(r1perm))
        r1perm <- r1perm$graph
      r2perm <- do.call(estimator, c(list(x2perm), estimatorArgs))
      if (is.list(r2perm))
        r2perm <- r2perm$graph
      r1perm <- as.matrix(r1perm)
      r2perm <- as.matrix(r2perm)
      if (!all(dim(r1perm) == c(nvars, nvars)) || !all(dim(r2perm) == c(nvars, nvars))) {
        stop("The estimator returned an invalid network during permutation.", call. = FALSE)
      }
      if (any(!is.finite(r1perm)) || any(!is.finite(r2perm))) {
        stop("The estimator returned non-finite network weights during permutation.", call. = FALSE)
      }
      if (weighted == FALSE) {
        r1perm = (r1perm != 0) * 1
        r2perm = (r2perm != 0) * 1
      }
    }
    if (paired == TRUE) {
      okay <- FALSE
      counter <- 0
      if (binary.data) {
        while (!okay && counter < 10000) {
          s <- sample(c(1, 2), nobs1, replace = TRUE)
          x1perm <- x1[s == 1, ]
          x1perm <- rbind(x1perm, x2[s == 2, ])
          x2perm <- x2[s == 1, ]
          x2perm <- rbind(x2perm, x1[s == 2, ])
          okay <- NCT_binary_group_valid(x1perm) && NCT_binary_group_valid(x2perm)
          if (!okay) counter <- counter + 1
        }
        if (!okay) {
          stop(
            "Could not generate a valid paired binary permutation after 10,000 attempts; category counts are too sparse.",
            call. = FALSE
          )
        }
      }
      else {
        s <- sample(c(1, 2), nobs1, replace = TRUE)
        x1perm <- x1[s == 1, ]
        x1perm <- rbind(x1perm, x2[s == 2, ])
        x2perm <- x2[s == 1, ]
        x2perm <- rbind(x2perm, x1[s == 2, ])
      }
      r1perm <- do.call(estimator, c(list(x1perm), estimatorArgs))
      if (is.list(r1perm))
        r1perm <- r1perm$graph
      r2perm <- do.call(estimator, c(list(x2perm), estimatorArgs))
      if (is.list(r2perm))
        r2perm <- r2perm$graph
      r1perm <- as.matrix(r1perm)
      r2perm <- as.matrix(r2perm)
      if (!all(dim(r1perm) == c(nvars, nvars)) || !all(dim(r2perm) == c(nvars, nvars))) {
        stop("The estimator returned an invalid network during permutation.", call. = FALSE)
      }
      if (any(!is.finite(r1perm)) || any(!is.finite(r2perm))) {
        stop("The estimator returned non-finite network weights during permutation.", call. = FALSE)
      }
      if (weighted == FALSE) {
        r1perm = (r1perm != 0) * 1
        r2perm = (r2perm != 0) * 1
      }
    }
    if (abs) {
      glstrinv.perm[i] <- abs(sum(abs(r1perm[upper.tri(r1perm)])) -
                                sum(abs(r2perm[upper.tri(r2perm)])))
    }
    else {
      glstrinv.perm[i] <- abs(sum(r1perm[upper.tri(r1perm)]) -
                                sum(r2perm[upper.tri(r2perm)]))
    }
    current_diffedges <- abs(r1perm - r2perm)[upper.tri(abs(r1perm - r2perm))]
    if (test.edges) {
      diffedges.perm[i, ] <- current_diffedges
      diffedges.permtemp[upper.tri(diffedges.permtemp, diag = FALSE)] <- current_diffedges
      diffedges.permtemp <- diffedges.permtemp + t(diffedges.permtemp)
      einv.perm.all[, , i] <- diffedges.permtemp
    }
    nwinv.perm[i] <- max(current_diffedges)
    if (test.centrality == TRUE) {
      cen1permtemp <- qgraph::centrality_auto(r1perm)$node.centrality
      cen2permtemp <- qgraph::centrality_auto(r2perm)$node.centrality
      names(cen1permtemp) <- names(cen2permtemp) <- c("betweenness",
                                                      "closeness", "strength", "expectedInfluence")
      if (TRUE %in% (bridgecen %in% centrality)) {
        b1permtemp <- networktools::bridge(r1perm, communities = communities,
                                           useCommunities = useCommunities)
        b2permtemp <- networktools::bridge(r2perm, communities = communities,
                                           useCommunities = useCommunities)
        names(b1permtemp) <- names(b2permtemp) <- c(bridgecen,
                                                    "bridgeExpectedInfluence2step", "communities")
        b1permtemp$communities <- b2permtemp$communities <- NULL
        cen1permtemp <- data.frame(c(cen1permtemp, b1permtemp))
        cen2permtemp <- data.frame(c(cen2permtemp, b2permtemp))
      }
      diffcen.permtemp <- as.matrix(cen1permtemp) - as.matrix(cen2permtemp)
      if (all_nodes) {
        diffcen.perm[i, ] <- reshape2::melt(diffcen.permtemp[,
                                                             centrality])$value
      }
      else {
        diffcen.perm[i, ] <- reshape2::melt(
          diffcen.permtemp[match(nodes, colnames(x1)), centrality, drop = FALSE]
        )$value
      }
    }
    if (progressbar == TRUE)
      setTxtProgressBar(pb, i)
  }
  if (test.edges == TRUE) {
    edges.pvaltemp <- (colSums(diffedges.perm >= diffedges.realmat) +
                         1)/(it + 1)
    if (is.character(edges)) {
      corrpvals.all.temp <- p.adjust(edges.pvaltemp, method = p.adjust.methods)
      corrpvals.all[upper.tri(corrpvals.all, diag = FALSE)] <- corrpvals.all.temp
      rownames(corrpvals.all) <- colnames(corrpvals.all) <- colnames(x1)
      einv.pvals <- melt(corrpvals.all, na.rm = TRUE,
                         value.name = "p-value")
      einv.perm <- einv.perm.all
      einv.real <- diffedges.realoutput
      einv.pvals <- cbind(einv.pvals, round(einv.real[upper.tri(einv.real)],
                                            8))
      colnames(einv.pvals) <- c("Var1", "Var2", "p-value",
                                "Test statistic E")
    }
    if (is.list(edges)) {
      einv.perm <- matrix(NA, it, length(edges))
      colnames(einv.perm) <- vapply(
        edges,
        function(edge) paste(colnames(x1)[edge], collapse = "--"),
        character(1)
      )
      uncorrpvals <- einv.real <- pairs <- c()
      edges.pvalmattemp[upper.tri(edges.pvalmattemp, diag = FALSE)] <- edges.pvaltemp
      edges.pvalmattemp <- edges.pvalmattemp + t(edges.pvalmattemp)
      for (j in seq_along(edges)) {
        pairs <- rbind(pairs, c(colnames(x1)[edges[[j]][1]],
                                colnames(x1)[edges[[j]][2]]))
        uncorrpvals[j] <- edges.pvalmattemp[edges[[j]][1],
                                            edges[[j]][2]]
        einv.real[j] <- diffedges.realoutput[edges[[j]][1],
                                             edges[[j]][2]]
        for (l in seq_len(it)) {
          einv.perm[l, j] <- einv.perm.all[, , l][edges[[j]][1],
                                                  edges[[j]][2]]
        }
      }
      corrpvals <- p.adjust(uncorrpvals, method = p.adjust.methods)
      einv.pvals <- data.frame(
        Var1 = pairs[, 1], Var2 = pairs[, 2],
        `p-value` = corrpvals, `Test statistic E` = einv.real,
        check.names = FALSE, stringsAsFactors = FALSE
      )
    }
    res <- list(glstrinv.real = glstrinv.real, glstrinv.sep = glstrinv.sep,
                glstrinv.pval = (sum(glstrinv.perm >= glstrinv.real) +
                                   1)/(it + 1), glstrinv.perm = glstrinv.perm,
                nwinv.real = nwinv.real, nwinv.pval = (sum(nwinv.perm >=
                                                             nwinv.real) + 1)/(it + 1), nwinv.perm = nwinv.perm,
                einv.real = einv.real, einv.pvals = einv.pvals,
                einv.perm = einv.perm, nw1 = nw1, nw2 = nw2)
    if (is.list(edges)) res$edges.tested <- edges.tested
  }
  if (progressbar == TRUE) {
    close(pb)
    pb <- NULL
  }
  if (test.edges == FALSE) {
    res <- list(glstrinv.real = glstrinv.real, glstrinv.sep = glstrinv.sep,
                glstrinv.pval = (sum(glstrinv.perm >= glstrinv.real) +
                                   1)/(it + 1), glstrinv.perm = glstrinv.perm,
                nwinv.real = nwinv.real, nwinv.pval = (sum(nwinv.perm >=
                                                             nwinv.real) + 1)/(it + 1), nwinv.perm = nwinv.perm,
                nw1 = nw1, nw2 = nw2)
  }
  if (test.centrality) {
    if (all_nodes) {
      diffcen.real.vec <- reshape2::melt(diffcen.real[,
                                                      centrality])$value
    }
    else {
      diffcen.real.vec <- reshape2::melt(
        diffcen.real[match(nodes, colnames(x1)), centrality, drop = FALSE]
      )$value
    }
    diffcen.realmat <- matrix(diffcen.real.vec, it, nnodes *
                                length(centrality), byrow = TRUE)
    diffcen.pvaltemp <- (colSums(abs(diffcen.perm) >= abs(diffcen.realmat)) +
                           1)/(it + 1)
    diffcen.HBall <- p.adjust(diffcen.pvaltemp, method = p.adjust.methods)
    diffcen.pval <- matrix(diffcen.HBall, nnodes, length(centrality))
    diffcen.real <- matrix(diffcen.real.vec, nrow = nnodes,
                           ncol = length(centrality))
    colnames(diffcen.pval) <- colnames(diffcen.real) <- centrality
    res[["diffcen.real"]] <- diffcen.real
    res[["diffcen.perm"]] <- diffcen.perm
    res[["diffcen.pval"]] <- diffcen.pval
    if (all_nodes) {
      rownames(res[["diffcen.real"]]) <- rownames(res[["diffcen.pval"]]) <- colnames(x1)
      colnames(res[["diffcen.perm"]]) <- apply(expand.grid(colnames(x1),
                                                           centrality), 1, paste, collapse = ".")
    }
    else {
      rownames(res[["diffcen.real"]]) <- rownames(res[["diffcen.pval"]]) <- nodes
    }
  }
  class(res) <- "NCT"
  return(res)
}

# These are the two estimator functions based on code exactly as in the original NCT:
NCT_estimator_Ising <- function(x, gamma = 0.25, AND = TRUE){
  IF <- IsingFit::IsingFit(x, AND = AND, gamma=gamma, plot=FALSE, progressbar=FALSE)
  IF$weiadj
}

NCT_binary_group_valid <- function(x) {
  x <- as.matrix(x)
  all(apply(x, 2, function(column) {
    min(sum(column == 0), sum(column == 1)) > 1
  }))
}


NCT_estimator_GGM <- function(x, make.positive.definite = TRUE, gamma = 0.5, corMethod = c("cor","cor_auto"), verbose=FALSE){

  corMethod <- match.arg(corMethod)

  if (corMethod == "cor"){
    cor_x <- cor(x)
  } else if (corMethod == "cor_auto") {
    cor_x <- cor_auto(x, verbose = FALSE)
  }


  if(make.positive.definite){
    cor_x <- matrix(nearPD(cor_x, corr=TRUE)$mat, ncol = ncol(cor_x))
    cor_x <- (cor_x + t(cor_x)) / 2 # make symmetric
  }


  if(verbose){
    nw <- EBICglasso(cor_x,nrow(x),gamma=gamma)
  } else {
    nw <- suppressWarnings(suppressMessages(EBICglasso(cor_x,nrow(x),gamma=gamma)))
  }

  return(nw)
}
