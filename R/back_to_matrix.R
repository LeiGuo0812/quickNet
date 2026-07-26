#' @title Convert a 3-column result of edge difference back to matrix
#'
#' @importFrom dplyr select mutate across everything
#' @importFrom tidyr spread
#' @importFrom magrittr %>%
#' @param x a data frame named einv.pals from NCT result object
#'
#' @return a p value matrix with the same dimension with correlation matrix
#' @export
#'
#' @examples
#' # uncomment to run
#' # EBICglassoNet(mtcars) -> net1
#' # EBICglassoNet(mtcars^3) -> net2
#' # suppressMessages(NetworkComparisonTest::NCT(
#' #   net1, net2, it = 500, binary.data = FALSE,
#' #   test.edges = TRUE, edges = 'all', progressbar = FALSE
#' # )) -> NCT
#' # back_to_matrix(NCT$einv.pvals)
#'
back_to_matrix <- function(x){
  required <- c("Var1", "Var2", "p-value")
  if (!is.data.frame(x) || !all(required %in% names(x))) {
    stop("x must be a data frame containing Var1, Var2, and p-value.", call. = FALSE)
  }

  variables <- unique(c(as.character(x$Var1), as.character(x$Var2)))
  variables <- variables[!is.na(variables) & nzchar(variables)]
  p_matrix <- matrix(
    NA_real_,
    length(variables),
    length(variables),
    dimnames = list(variables, variables)
  )
  if (length(variables) == 0) return(p_matrix)
  diag(p_matrix) <- 1

  for (i in seq_len(nrow(x))) {
    node_i <- as.character(x$Var1[[i]])
    node_j <- as.character(x$Var2[[i]])
    if (node_i %in% variables && node_j %in% variables) {
      value <- suppressWarnings(as.numeric(as.character(x$`p-value`[[i]])))
      p_matrix[node_i, node_j] <- p_matrix[node_j, node_i] <- value
    }
  }
  p_matrix
}
