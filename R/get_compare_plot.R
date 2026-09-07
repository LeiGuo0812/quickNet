#' @title Get network difference plots from NetCompare and quickNet results
#' @param NetCompare the result of \code{quickNet::NetCompare}.
#' @param network_G the result of \code{quickNet::quickNet}. The plot configurations of the results will be the same as network_G, but the legend and pie circle will be removed.
#' @param maximum  regards the highest of the maximum or highest absolute edge weight as the highest weight to scale the edge widths too. To compare several graphs, set this argument to a higher value than any edge weight in the graphs (typically 1 for correlations). If you want the same scale as network_G, keep it same as the number you set in quickNet.
#' @param use.mask only present significant different edges that are non-zero in the EBICglasso networks constructed from data1, data2, or both. \itemize{
#' \item \code{"none"}: present all significantly different edges.
#' \item \code{"net1"}: only present significantly different edges that are non-zero in the first estimated network.
#' \item \code{"net2"}: only present significantly different edges that are non-zero in the second estimated network.
#' \item \code{"both"}: only present significantly different edges that are non-zero in both estimated networks.
#' }
#' @param output whether output the plots as pdf files.
#' @param prefix the prefix of output plot files.
#' @param path the path of output files, can be either a relative or absolute path.
#' @param device 'pdf' or 'svg', deciding the output plot format.
#' @param width the width of plot, in inch.
#' @param height the height of plot, in inch. The parameters \code{prefix},
#' \code{path}, \code{width}, and \code{height} only apply when
#' \code{output = TRUE}.
#' @param ... ... other parameter from \code{pdf} or \code{svg}.
#'
#' @return a list contains the diff_plot, diff_pos_plot and diff_neg_plot,
#' representing the network difference of global, nw1>nw2 and nw1<nw2.
#' @export
#'
#' @examples
#' data('mtcars')
#' network_G <- quickNet(mtcars)
#' NetCompare <- NetCompare(mtcars,mtcars^3,it=100)
#' get_compare_plot(NetCompare, network_G, output = FALSE)
#'
get_compare_plot <- function(NetCompare, network_G, maximum = 0.47, use.mask = 'none', output = TRUE, prefix = '', path = '.', device = 'pdf', width = 10, height = 7, ...){

  results <- list()
  required_differences <- c("diff_sig", "diff_sig_nw1>nw2", "diff_sig_nw1<nw2")
  if (!all(required_differences %in% names(NetCompare))) {
    stop(
      "NetCompare must have been run with test.edges = TRUE to draw difference plots.",
      call. = FALSE
    )
  }
  network_plot <- if (inherits(network_G, "quicknet_fit")) {
    if (is.null(network_G$plots$network)) {
      stop("network_G is a quicknet_fit object without a stored qgraph plot.", call. = FALSE)
    }
    network_G$plots$network
  } else {
    network_G
  }

  if (!(use.mask %in% c('none','net1','net2','both'))) {
    stop('use.mask should be one of "none", "net1", "net2" and "both".')
  }

  base_matrix <- quicknet_network_matrix(network_G)
  graph_labels <- colnames(base_matrix)
  for (field in c(required_differences, "net1_mask", "net2_mask")) {
    if (!is.null(NetCompare[[field]])) {
      NetCompare[[field]] <- quicknet_align_network(base_matrix, NetCompare[[field]])
    }
  }

  if (use.mask == 'none') {
    diff_net <- NetCompare$`diff_sig`
    diff_pos_net <- NetCompare$`diff_sig_nw1>nw2`
    diff_neg_net <- NetCompare$`diff_sig_nw1<nw2`
  } else if (use.mask == 'net1') {
    diff_net <- NetCompare$`diff_sig` * NetCompare$net1_mask
    diff_pos_net <- NetCompare$`diff_sig_nw1>nw2` * NetCompare$net1_mask
    diff_neg_net <- NetCompare$`diff_sig_nw1<nw2` * NetCompare$net1_mask
  } else if (use.mask == 'net2') {
    diff_net <- NetCompare$`diff_sig` * NetCompare$net2_mask
    diff_pos_net <- NetCompare$`diff_sig_nw1>nw2` * NetCompare$net2_mask
    diff_neg_net <- NetCompare$`diff_sig_nw1<nw2` * NetCompare$net2_mask
  } else if (use.mask == 'both') {
    diff_net <- NetCompare$`diff_sig` * NetCompare$net1_mask * NetCompare$net2_mask
    diff_pos_net <- NetCompare$`diff_sig_nw1>nw2` * NetCompare$net1_mask * NetCompare$net2_mask
    diff_neg_net <- NetCompare$`diff_sig_nw1<nw2` * NetCompare$net1_mask * NetCompare$net2_mask
  }

  colnames(diff_net) <- rownames(diff_net) <- graph_labels
  colnames(diff_pos_net) <- rownames(diff_pos_net) <- graph_labels
  colnames(diff_neg_net) <- rownames(diff_neg_net) <- graph_labels

  results$diff_plot <- qgraph::qgraph(diff_net, maximum= maximum, layout = network_plot$layout, labels = graph_labels, DoNotPlot = TRUE)
  results$diff_pos_plot <- qgraph::qgraph(diff_pos_net, maximum= maximum, layout = network_plot$layout, labels = graph_labels, DoNotPlot = TRUE)
  results$diff_neg_plot <- qgraph::qgraph(diff_neg_net, maximum= maximum, layout = network_plot$layout, labels = graph_labels, DoNotPlot = TRUE)

  results$diff_plot$Arguments <- network_plot$Arguments
  results$diff_plot$plotOptions <- network_plot$plotOptions
  results$diff_plot$graphAttributes$Nodes <- network_plot$graphAttributes$Nodes
  results$diff_plot$graphAttributes$Knots<- network_plot$graphAttributes$Knots
  results$diff_plot$graphAttributes$Edges$curve = rep(network_plot$graphAttributes$Edges$curve[1], length(results$diff_plot$graphAttributes$Edges$labels))
  results$diff_plot$graphAttributes$Edges$lty = rep(network_plot$graphAttributes$Edges$lty[1], length(results$diff_plot$graphAttributes$Edges$labels))
  results$diff_plot$layout <- network_plot$layout
  results$diff_plot$layout.orig <- network_plot$layout.orig

  results$diff_plot$plotOptions$legend <- FALSE
  results$diff_plot$plotOptions$drawPies <- FALSE
  results$diff_plot$graphAttributes$Nodes$pie <- NULL

  results$diff_pos_plot$Arguments <- network_plot$Arguments
  results$diff_pos_plot$plotOptions <- network_plot$plotOptions
  results$diff_pos_plot$graphAttributes$Nodes <- network_plot$graphAttributes$Nodes
  results$diff_pos_plot$graphAttributes$Knots<- network_plot$graphAttributes$Knots
  results$diff_pos_plot$graphAttributes$Edges$curve = rep(network_plot$graphAttributes$Edges$curve[1], length(results$diff_pos_plot$graphAttributes$Edges$labels))
  results$diff_pos_plot$graphAttributes$Edges$lty = rep(network_plot$graphAttributes$Edges$lty[1], length(results$diff_pos_plot$graphAttributes$Edges$labels))
  results$diff_pos_plot$layout <- network_plot$layout
  results$diff_pos_plot$layout.orig <- network_plot$layout.orig

  results$diff_pos_plot$plotOptions$legend <- FALSE
  results$diff_pos_plot$plotOptions$drawPies <- FALSE
  results$diff_pos_plot$graphAttributes$Nodes$pie <- NULL

  results$diff_neg_plot$Arguments <- network_plot$Arguments
  results$diff_neg_plot$plotOptions <- network_plot$plotOptions
  results$diff_neg_plot$graphAttributes$Nodes <- network_plot$graphAttributes$Nodes
  results$diff_neg_plot$graphAttributes$Knots<- network_plot$graphAttributes$Knots
  results$diff_neg_plot$graphAttributes$Edges$curve = rep(network_plot$graphAttributes$Edges$curve[1], length(results$diff_neg_plot$graphAttributes$Edges$labels))
  results$diff_neg_plot$graphAttributes$Edges$lty = rep(network_plot$graphAttributes$Edges$lty[1], length(results$diff_neg_plot$graphAttributes$Edges$labels))
  results$diff_neg_plot$layout <- network_plot$layout
  results$diff_neg_plot$layout.orig <- network_plot$layout.orig

  results$diff_neg_plot$plotOptions$legend <- FALSE
  results$diff_neg_plot$plotOptions$drawPies <- FALSE
  results$diff_neg_plot$graphAttributes$Nodes$pie <- NULL

  plot(results$diff_plot)
  plot(results$diff_pos_plot)
  plot(results$diff_neg_plot)

  if (output) {

    if (str_sub(prefix,-1) %in% c('_','.','')) {
      prefix <- prefix
    } else {
      prefix <- paste0(prefix,'_')
    }

    device <- match.arg(device, c("pdf", "svg"))
    plot_specs <- list(
      diff_network_plot = results$diff_plot,
      diff_pos_network_plot = results$diff_pos_plot,
      diff_neg_network_plot = results$diff_neg_plot
    )
    for (plot_name in names(plot_specs)) {
      quicknet_plot_to_device(
        filename = path_join(c(path, paste0(prefix, plot_name, ".", device))),
        device = device,
        width = width,
        height = height,
        plot_function = local({
          current_plot <- plot_specs[[plot_name]]
          function() plot(current_plot)
        }),
        ...
      )
    }
  }
  return(results)
}
