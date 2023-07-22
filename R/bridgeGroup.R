#' Get new groups that bridge nodes are in a single group.
#' @importFrom forcats fct_relevel
#' @importFrom magrittr %>%
#' @importFrom stringr str_match
#' @importFrom dplyr group_by ungroup pull mutate top_n
#' @param bridge The output of \code{quickNet::Bridge}.
#' @param groups The original groups or communities parameter used. if groups is a list, the name of each group should not be end with numbers.
#' @param labels If there are labels for each node, please provide.
#' @param n The number of nodes to recode by bridge strength, \code{n} nodes with highest bridge strength will be recoded as bridge node.
#' @param by_group Whether the node recoding procedure should be conducted by groups. If TRUE, the function will recode n nodes with highest bridge strength in each original group. If FALSE,the function will recode n nodes with highest bridge strength in all nodes.
#'
#' @return A factor, can be used as new \code{groups} parameter in \code{quickNet::quickNet}
#' @export
#'
#' @examples
#' data('mtcars')
#' groups = list(A = 1:5, B = 6:11)
#' net_G <- quickNet(mtcars, groups = groups)
#' Bridge <- Bridge(net_G,communities = groups)
#' new_groups <- bridgeGroup(Bridge, groups, n = 1, by_group = TRUE)
#' net_G <- quickNet(mtcars, groups = new_groups)
#'
bridgeGroup <- function(bridge, groups, labels = NULL, n = 1, by_group = TRUE) {

  bridge_strength <- bridge$bridge_data$`Bridge Strength`

  if (!is.null(labels)) {
    names(bridge_strength) <- labels
  }

  if (is.character(groups) | is.factor(groups)) {
    groups_vec = groups

  } else if (is.list(groups)) {

    groups_vec = unlist(groups) %>%
      sort() %>%
      names() %>%
      str_match('([^0-9]+)[0-9]+$') %>%
      .[,2]
  }

  group_df = data.frame(label = names(bridge_strength),
                        bridge_strength = bridge_strength,
                        groups = groups_vec)

  if (by_group) {
    top_label = group_df %>%
      group_by(groups) %>%
      top_n(n, bridge_strength) %>%
      ungroup() %>%
      pull(label)
  } else {
    top_label = group_df %>%
      top_n(n, bridge_strength) %>%
      ungroup() %>%
      pull(label)
  }

  new_groups = group_df %>%
    mutate(group_new = ifelse(label %in% top_label, 'Bridge', as.character(groups))) %>%
    pull(group_new)

  if (is.character(groups) | is.factor(groups)) {
    new_groups = fct_relevel(new_groups, c(levels(as.factor(groups)), 'Bridge'))

  } else if (is.list(groups)) {
    new_groups = fct_relevel(new_groups, c(names(groups), 'Bridge'))
  }
  return(new_groups)
}
