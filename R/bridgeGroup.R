#' Get new groups that bridge nodes are in a single group.
#' @importFrom forcats fct_relevel
#' @param bridge The output of \code{quickNet::Bridge}.
#' @param groups The original groups or communities parameter used.
#' @param labels If there are labels for each node, please provide.
#' @param probs The Bridge Strength threshold, 0~1. The nodes whose Bridge Strength bigger than \code{probs} will be assigned to a new group. Default is 0.8.
#'
#' @return A factor, can be used as new \code{groups} parameter in \code{quickNet::quickNet}
#' @export
#'
#' @examples
#' data('mtcars')
#' groups = list(A = 1:5, B = 6:11)
#' net_G <- quickNet(mtcars, groups = groups)
#' Bridge <- Bridge(net_G,communities = groups)
#' new_groups <- bridgeGroup(Bridge, groups)
#' net_G <- quickNet(mtcars, groups = new_groups)
#'
bridgeGroup <- function(bridge, groups, labels = NULL, probs = 0.8) {

  bridge_strength <- bridge$bridge_data$`Bridge Strength`

  if (!is.null(labels)) {
    names(bridge_strength) <- labels
  }

  top_bridges <- names(bridge_strength[bridge_strength>quantile(bridge_strength, probs=probs, na.rm=TRUE)])

  bridge_num_w1 <- which(names(bridge_strength) %in% top_bridges)

  new_groups <- vector(mode = 'character')

  if(is.character(groups) | is.factor(groups)){
    for(i in seq_along(bridge_strength)) {
      if(i %in% bridge_num_w1) {
        new_groups[i] <- "Bridge"
      } else {new_groups[i] <- groups[i]}
    }
    new_groups <- fct_relevel(new_groups, c(levels(as.factor(groups)), 'Bridge'))
  } else if (is.list(groups)){
    group_tmp <- vector('character')
    for (i in seq_along(groups)) {
      group_tmp[groups[[i]]] <- names(groups)[i]
    }
    for(i in seq_along(bridge_strength)) {
      if(i %in% bridge_num_w1) {
        new_groups[i] <- "Bridge"
      } else {new_groups[i] <- group_tmp[i]}
    }
    new_groups <- fct_relevel(new_groups, c(names(groups), 'Bridge'))
  }
  return(new_groups)
}
