#' Get new groups that bridge nodes are in a single group
#' @importFrom forcats fct_relevel
#' @importFrom magrittr %>%
#' @importFrom stringr str_match
#' @importFrom dplyr group_by ungroup pull mutate top_n
#' @param bridge The output of \code{quickNet::Bridge}.
#' @param groups The original groups or communities, as a vector or a named list of node indices.
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
  if (!is.numeric(bridge_strength) || length(bridge_strength) == 0L ||
      any(!is.finite(bridge_strength))) {
    stop("bridge must contain finite Bridge Strength values.", call. = FALSE)
  }
  if (!is.numeric(n) || length(n) != 1L || !is.finite(n) || n < 0 || n != floor(n)) {
    stop("n must be a non-negative integer.", call. = FALSE)
  }

  if (!is.null(labels)) {
    if (length(labels) != length(bridge_strength) || anyNA(labels) || anyDuplicated(labels)) {
      stop("labels must contain one unique name per node.", call. = FALSE)
    }
    names(bridge_strength) <- labels
  }

  if (is.character(groups) | is.factor(groups)) {
    groups_vec = groups

  } else if (is.list(groups)) {

    indices <- unlist(groups, use.names = FALSE)
    if (is.null(names(groups)) || anyNA(names(groups)) || any(!nzchar(names(groups))) ||
        anyDuplicated(names(groups)) || !is.numeric(indices) ||
        anyNA(indices) || anyDuplicated(indices) ||
        !setequal(indices, seq_along(bridge_strength))) {
      stop("groups must be a named list covering each node index exactly once.", call. = FALSE)
    }
    groups_vec <- character(length(bridge_strength))
    for (group_name in names(groups)) groups_vec[groups[[group_name]]] <- group_name
  } else {
    stop("groups must be a character/factor vector or a named list.", call. = FALSE)
  }

  if (length(groups_vec) != length(bridge_strength) || anyNA(groups_vec)) {
    stop("groups must contain one non-missing assignment per node.", call. = FALSE)
  }
  if (is.null(names(bridge_strength))) names(bridge_strength) <- as.character(seq_along(bridge_strength))

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
    new_groups = factor(new_groups, levels = unique(c(levels(as.factor(groups)), 'Bridge')))

  } else if (is.list(groups)) {
    new_groups = factor(new_groups, levels = unique(c(names(groups), 'Bridge')))
  }
  return(new_groups)
}
