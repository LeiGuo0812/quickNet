#' Change network linetype based on group/community.
#'
#' @param data The data frame used to construct the data
#' @param group The group information defining the community of nodes (must be a list.)
#' @param inner_type The line type of inner community edges.
#' @param outer_type The line type of outer community edges.
#'
#' @return A matrix that define the linetype of each edge. This can be directly passed to the \code{lty} parameter in function \code{quickNet}
#'
#' @export
#'
#' @examples
#' data("mtcars")
#' groups = list(`1` = seq(1,11,by=3),`2` = seq(2,11,by=3),`3` = seq(3,11,by=3))
#' lty = get_lty_matrix(mtcars, groups)
#' quickNet(mtcars, groups = groups, lty = lty)
#'
#'
get_lty_matrix = function(data, group, inner_type = 1, outer_type = 2) {
  lty_matrix = matrix(nrow = ncol(data), ncol = ncol(data))
  for (i in 1:nrow(lty_matrix)) {
    for (j in 1:ncol(lty_matrix)) {
      if (i!=j) {
        same_group = sapply(group, function(x) {
          return(i %in% x & j %in% x)
        })

        same_logic = any(same_group)

        if (isTRUE(same_logic)) {
          lty_matrix[i,j] = inner_type
        } else {
          lty_matrix[i,j] = outer_type
        }
      }
    }
  }
  return(lty_matrix)
}
