#' @title Quick plot network graph
#' @description This function provide a quick way to plot network graph
#' @importFrom bootnet estimateNetwork
#' @importFrom mgm mgm
#' @importFrom qgraph qgraph
#' @param data a data frame, each column presents a node, there should be no miss values in the data frame.
#' @param layout This argument controls the layout of the graph. "circle" places all nodes in a single circle, "groups" gives a circular layout in which each group is put in separate circles and "spring" gives a force embedded layout. It also can be a matrix with a row for each node and x and y coordinates in the first and second column respectively. Defaults to "circular" in weighted graphs without a groups list, "groups" in weighted graphs with a groups list, and "spring" in unweighted graphs. Can also be a function from the igraph package.
#' @param pie a logical value, specifying whether draw a pie around the node with mgm algorithm.
#' @param maximum regards the highest of the maximum or highest absolute edge weight as the highest weight to scale the edge widths too. To compare several graphs, set this argument to a higher value than any edge weight in the graphs (typically 1 for correlations).
#' @param groups An object that indicates which nodes belong together. Can be a list in which each element is a vector of integers identifying the numbers of the nodes that belong together, or a factor.
#' @param color A vector with a color for each element in the groups list, or a color for each node. Defaults to the background color ("bg" argument, which defaults to "white") without groups list and rainbow(length(groups)) with a groups list. If color is not provided, a set of default colors will be used.
#' @param legend Logical value indicating if a legend should be plotted. Defaults to TRUE if a groups object or nodeNames is supplied
#' @param nodeNames for each node, can be used to plot a legend next to the plot that links the node labels to node names.
#' @param border.width Node border width passed to \code{qgraph::qgraph}.
#' @param border.color Node border color passed to \code{qgraph::qgraph}.
#' @param model network model. One of "EBICglasso", "correlation", "partial", "ising", "ordinal", or "mgm".
#' @param cor_method correlation method used by correlation and partial models.
#' @param missing missing-data handling. The default "listwise" removes rows with missing values.
#' @param gamma EBIC tuning parameter for EBICglasso, Ising, and MGM.
#' @param ordinal_method association method used by ordinal models.
#' @param AND logical. Should the Ising model use the AND rule?
#' @param types MGM variable types, one per variable.
#' @param levels MGM variable levels, one per variable.
#' @param ... parameters from \code{\link[qgraph]{qgraph}}
#'
#' @return a \code{quicknet_fit} object.
#' @export
#'
#' @examples
#' data("mtcars")
#' quickNet(mtcars)
#'
quickNet <- function(data, layout = 'spring', pie = TRUE, maximum = 0.47, groups = NULL, color = c("#71d0f5", "#fed439", "#66bb6a", "#fd7446", "#d2af81", "#d5e4a2", "#f44336", "#197ec0", "#46732e", "#8073ac", "#709ae1"), legend = FALSE, nodeNames = NULL, border.width=2, border.color='#555555', model = "EBICglasso", cor_method = "pearson", missing = "listwise", gamma = 0.5, ordinal_method = "polychoric", AND = TRUE, types = NULL, levels = NULL, ...){

  model <- match.arg(model, c("EBICglasso", "correlation", "partial", "ising", "ordinal", "mgm"))
  quicknet_validate_input(
    data,
    model = model,
    types = types,
    levels = levels,
    missing = missing
  )
  fit <- quicknet_fit_cross_sectional(
    data = data,
    model = model,
    cor_method = cor_method,
    missing = missing,
    gamma = gamma,
    ordinal_method = ordinal_method,
    AND = AND,
    types = types,
    levels = levels
  )

  if (pie && model %in% c("EBICglasso", "correlation", "partial")) {
    fit_mgm <- mgm(
      as.matrix(fit$data),
      type = rep('g', ncol(fit$data)),
      level = rep(1, ncol(fit$data)),
      pbar = FALSE,
      signInfo = FALSE,
      warnings = FALSE
    )
    pred_mgm <- stats::predict(fit_mgm, fit$data)
  } else {
    pred_mgm <- NULL
  }

  if (!identical(color, c("#71d0f5", "#fed439", "#66bb6a", "#fd7446", "#d2af81", "#d5e4a2", "#f44336", "#197ec0", "#46732e", "#8073ac", "#709ae1"))) {
    color = color

  } else if (is.null(groups)) {
    color = "#71d0f5"

  } else if (!is.null(groups) & is.list(groups)){
    color = c("#71d0f5", "#fed439", "#66bb6a", "#fd7446", "#d2af81", "#d5e4a2", "#f44336", "#197ec0", "#46732e", "#8073ac", "#709ae1")[1:length(groups)]
  } else if (!is.null(groups) & (is.character(groups) | is.factor(groups))) {
    color = c("#71d0f5", "#fed439", "#66bb6a", "#fd7446", "#d2af81", "#d5e4a2", "#f44336", "#197ec0", "#46732e", "#8073ac", "#709ae1")[1:length(unique(groups))]
  }

  network_G <- qgraph::qgraph(
    fit$graph,
    maximum = maximum,
    layout = layout,
    pie = if (!is.null(pred_mgm)) pred_mgm$error$R2 else NULL,
    groups = groups,
    color = color,
    legend = legend,
    nodeNames = nodeNames,
    border.width = border.width,
    border.color = border.color,
    ...
  )

  if (!is.null(pred_mgm)) {
    fit$nodes$predictability_R2 <- pred_mgm$error$R2
    fit$fit$mgm_predictability <- pred_mgm
  }

  fit$plots$network <- network_G
  fit$meta$plot <- list(
    layout = layout,
    maximum = maximum,
    groups = groups,
    color = color,
    legend = legend,
    nodeNames = nodeNames,
    border.width = border.width,
    border.color = border.color
  )

  return(fit)
}
