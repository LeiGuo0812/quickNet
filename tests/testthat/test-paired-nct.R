# The reference uses explicit per-participant assignment, independently of the
# implementation's stacked-row indexing and independently of qgraph centrality.
paired_nct_reference <- function(x, y, swap, estimator) {
  a <- x
  b <- y
  for (i in seq_len(nrow(x))) {
    if (swap[[i]]) {
      a[i, ] <- y[i, ]
      b[i, ] <- x[i, ]
    }
  }
  ga <- estimator(a)
  gb <- estimator(b)
  edge <- abs(ga - gb)[upper.tri(ga)]
  list(network = max(edge), global = abs(sum(abs(ga[upper.tri(ga)])) -
    sum(abs(gb[upper.tri(gb)]))), edges = edge,
    centrality = c(rowSums(abs(ga)) - rowSums(abs(gb)), rowSums(ga) - rowSums(gb)))
}

test_that("paired swaps preserve participant positions and complete observation vectors", {
  x <- matrix(seq_len(15), 5, dimnames = list(paste0("id", 1:5), letters[1:3]))
  y <- x + 100L
  patterns <- as.matrix(expand.grid(rep(list(c(FALSE, TRUE)), nrow(x))))
  for (j in seq_len(nrow(patterns))) {
    swap <- patterns[j, ]
    result <- NCT_paired_swap(x, y, swap)
    for (i in seq_len(nrow(x))) {
      expect_identical(result$x1[i, ], if (swap[[i]]) y[i, ] else x[i, ])
      expect_identical(result$x2[i, ], if (swap[[i]]) x[i, ] else y[i, ])
    }
    expect_identical(dimnames(result$x1), dimnames(x))
    expect_identical(dimnames(result$x2), dimnames(y))
  }
})

test_that("paired NCT matches the exhaustive swap oracle with participant-specific weights", {
  x <- matrix(c(1, 4, 2, 3, 7, -3, 4, 1, 5, 2, 2, -1, 4, -2, 6), 5,
              dimnames = list(NULL, letters[1:3]))
  y <- matrix(c(2, 1, 5, 4, 3, 4, -1, 2, 3, -2, 3, 2, -1, 5, 4), 5,
              dimnames = dimnames(x))
  weights <- c(1, 2, 3, 5, 8)
  estimator <- function(data, weights) {
    graph <- crossprod(as.matrix(data) * sqrt(weights)) / sum(weights)
    diag(graph) <- 0
    graph
  }
  patterns <- as.matrix(expand.grid(rep(list(c(FALSE, TRUE)), nrow(x))))
  reference <- lapply(seq_len(nrow(patterns)), function(j)
    paired_nct_reference(x, y, patterns[j, ], function(data) estimator(data, weights)))
  set.seed(8732)
  draws <- replicate(127, sample(c(1, 2), nrow(x), replace = TRUE) == 2)
  indices <- apply(draws, 2, function(swap) 1L + sum(swap * 2^(seq_along(swap) - 1)))
  expected <- reference[indices]
  set.seed(8732)
  result <- NCT_gl(x, y, paired = TRUE, estimator = estimator,
    estimatorArgs = list(weights = weights), it = 127, test.edges = TRUE,
    test.centrality = TRUE, centrality = c("strength", "expectedInfluence"),
    p.adjust.methods = "holm", progressbar = FALSE, verbose = FALSE)
  expect_equal(result$nwinv.perm, vapply(expected, `[[`, numeric(1), "network"))
  expect_equal(result$glstrinv.perm, vapply(expected, `[[`, numeric(1), "global"))
  edge_null <- t(vapply(expected, `[[`, numeric(3), "edges"))
  centrality_null <- t(vapply(expected, `[[`, numeric(6), "centrality"))
  expect_equal(unname(result$diffcen.perm), unname(centrality_null))
  expect_equal(result$nwinv.pval, (1 + sum(result$nwinv.perm >= reference[[1]]$network)) / 128)
  expect_equal(result$glstrinv.pval, (1 + sum(result$glstrinv.perm >= reference[[1]]$global)) / 128)
  expect_equal(result$einv.pvals$`p-value`, stats::p.adjust(
    (1 + colSums(sweep(edge_null, 2, reference[[1]]$edges, `>=`))) / 128, "holm"))
  expect_equal(as.numeric(result$diffcen.pval), unname(stats::p.adjust(
    (1 + colSums(sweep(abs(centrality_null), 2, abs(reference[[1]]$centrality), `>=`))) / 128, "holm")))
  expect_true(result$info$call$paired)
  expect_equal(result$info$permutation$pairs, 5)
  expect_equal(result$info$permutation$draws, 127)
})

test_that("paired binary NCT reports conditional draws and rejects an ineligible original split", {
  x <- cbind(a = c(0, 0, 1, 1, 0, 0), b = c(0, 1, 0, 1, 0, 0))
  y <- 1 - x
  estimator <- function(data) {
    graph <- crossprod(data) / nrow(data)
    diag(graph) <- 0
    graph
  }
  # Generate the conditional reference without the package's validity helper.
  valid <- function(data) all(colSums(data) >= 2 & colSums(data) <= nrow(data) - 2)
  set.seed(4281)
  expected <- numeric()
  rejected <- 0L
  while (length(expected) < 63L) {
    swap <- sample(c(1, 2), nrow(x), replace = TRUE) == 2
    a <- x
    b <- y
    for (i in which(swap)) {
      a[i, ] <- y[i, ]
      b[i, ] <- x[i, ]
    }
    if (valid(a) && valid(b)) {
      expected <- c(expected, abs(estimator(a)[1, 2] - estimator(b)[1, 2]))
    } else rejected <- rejected + 1L
  }
  set.seed(4281)
  result <- NCT_gl(x, y, paired = TRUE, binary.data = TRUE,
    estimator = estimator, it = 63, progressbar = FALSE, verbose = FALSE)
  expect_equal(result$nwinv.perm, expected)
  expect_equal(result$info$permutation$rejected_draws, rejected)
  expect_true(result$info$permutation$conditional_on_binary_counts)
  expect_gt(rejected, 0)
  x[, 1] <- c(1, 0, 0, 0, 0, 0)
  expect_error(NCT_gl(x, y, paired = TRUE, binary.data = TRUE,
    estimator = estimator, it = 1, progressbar = FALSE), "each original dataset")
})

test_that("paired comparison states the exchangeability assumption", {
  x <- mtcars[1:8, 1:3]
  estimator <- function(data) { graph <- cor(data); diag(graph) <- 0; graph }
  expect_message(NetCompare(x, x + 1, paired = TRUE, estimator = estimator,
    it = 1, progressbar = FALSE), "exchangeability assumptions")
})
