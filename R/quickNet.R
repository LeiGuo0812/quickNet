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
#' @param labels If FALSE, no labels are plotted. If TRUE, order in weights matrix is used as labels. This can also be a vector with a label for each node. Defaults for graphs with less than 20 nodes to a 3 character abbreviation of the columnames and rownames if these are identical or else to TRUE. If a label contains an asterisk (e.g. "x1*") then the asterisk will be omitted and the label will be printed in symbol font (use this for Greek letters). Can also be a list with a label as each element, which can be expressions for more advanced mathematical annotation.
#' @param nodeNames for each node, can be used to plot a legend next to the plot that links the node labels to node names.
#' @param ... parameters from \code{\link[qgraph]{qgraph}}
#'
#' @return a qgraph object.
#' @export
#'
#' @examples
#' data("mtcars")
#' quickNet(mtcars)
#'
quickNet <- function(data, layout = 'spring', pie = TRUE, maximum = 0.47, groups = NULL, color = c("#71d0f5", "#fed439", "#66bb6a", "#fd7446", "#d2af81", "#d5e4a2", "#f44336", "#197ec0", "#46732e", "#8073ac", "#709ae1"), legend = FALSE, nodeNames = NULL, border.width=2, border.color='#555555', ...){

  network <- estimateNetwork(data,default = "EBICglasso")

  if (pie) {
    fit_mgm <- mgm(as.matrix(na.omit(data)), type=rep('g', ncol(data)) , lev=rep(1,ncol(data)))
    pred_mgm <- predict(fit_mgm, na.omit(data))
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

  network_G <- plot(network,
                  maximum = maximum,
                  layout = layout,
                  pie = pred_mgm$error$R2,
                  groups = groups,
                  color = color,
                  legend = legend,
                  nodeNames = nodeNames,
                  border.width = border.width,
                  border.color = border.color,
                  ...)

  network_G$graphData <- network

  if (pie) {
    network_G$mgm_R2 <- pred_mgm$error$R2
  }

  return(network_G)
}
