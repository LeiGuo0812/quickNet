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
#' # suppressMessages(NetworkComparisonTest::NCT(net1,net2,it=500, binary.data=FALSE,test.edges=TRUE, edges='all',progressbar = FALSE)) -> NCT
#' # back_to_matrix(NCT$einv.pvals)
#'
back_to_matrix <- function(x){
  var <- unique(c(as.character(x$Var1),as.character(x$Var2)))
  tested <- x %>%
    dplyr::filter(!is.na(`p-value`))
  tested_var <- unique(c(as.character(tested$Var1),as.character(tested$Var2)))
  x %>%
    spread('Var2','p-value') -> temp
  temp[nrow(temp) + 1, 1]  <- var[-c(1:length(var)-1)]
  temp[,var[1]] <- NA
  temp %>%
    select(Var1,var[1],everything()) %>%
    select(-1) %>%
    as.data.frame() %>%
    `row.names<-`(temp$Var1) -> matrix
  matrix[lower.tri(matrix,diag = T)]  <- 0
  matrix <- matrix %>%
   mutate(across(everything(), ~ as.numeric(.x)))
  matrix + t(matrix) %>%
    `diag<-`(NA_integer_) -> p_matrix
  for (i in seq_along(tested_var)) {
    p_matrix[tested_var[i],tested_var[i]] <- 1
  }
  return(p_matrix)
}
