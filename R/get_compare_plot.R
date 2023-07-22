#' @title get network difference plot from NetCompare and quickNet results.
#' @param NetCompare the result of \code{quickNet::NetCompare}.
#' @param network_G the result of \code{quickNet::quickNet}. The plot configurations of the results will be the same as network_G, but the legend and pie circle will be removed.
#' @param maximum  regards the highest of the maximum or highest absolute edge weight as the highest weight to scale the edge widths too. To compare several graphs, set this argument to a higher value than any edge weight in the graphs (typically 1 for correlations). If you want the same scale as network_G, keep it same as the number you set in quickNet.
#' @param use.mask only present significant different edges that are non-zero in the EBICglasso networks constructed from data1, data2, or both. \itemize{
#' \item{"none": present all significantly different edges.}
#' \item{"net1": only present significantly different edges that are non-zero in the EBICglasso network constructed from data1.}
#' \item{"net2": only present significantly different edges that are non-zero in the EBICglasso network constructed from data2.}
#' \item{"both": only present significantly different edges that are both non-zero in the EBICglasso networks constructed from data1 and data2.}
#' }
#' @param output whether output the plots as pdf files.
#' @param prefix the prefix of output plot files.
#' @param path the path of output files, can be either a relative or absolute path.
#' @param width the width of plot, in inch.
#' @param height the height of plot, in inch. \itemize{
#' \item the parameter prefix, path, width, and height only works when output is TRUE.
#' }
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

  if (!(use.mask %in% c('none','net1','net2','both'))) {
    stop('use.mask should be one of "none", "net1", "net2" and "both".')
  }

  diff_net <- diff_pos_net <- diff_neg_net <- network_G$graphData

  if (use.mask == 'none') {
    diff_net$graph <- NetCompare$`diff_sig`
    diff_pos_net$graph <- NetCompare$`diff_sig_nw1>nw2`
    diff_neg_net$graph <- NetCompare$`diff_sig_nw1<nw2`
  } else if (use.mask == 'net1') {
    diff_net$graph <- NetCompare$`diff_sig` * NetCompare$net1_mask
    diff_pos_net$graph <- NetCompare$`diff_sig_nw1>nw2` * NetCompare$net1_mask
    diff_neg_net$graph <- NetCompare$`diff_sig_nw1<nw2` * NetCompare$net1_mask
  } else if (use.mask == 'net2') {
    diff_net$graph <- NetCompare$`diff_sig` * NetCompare$net2_mask
    diff_pos_net$graph <- NetCompare$`diff_sig_nw1>nw2` * NetCompare$net2_mask
    diff_neg_net$graph <- NetCompare$`diff_sig_nw1<nw2` * NetCompare$net2_mask
  } else if (use.mask == 'both') {
    diff_net$graph <- NetCompare$`diff_sig` * NetCompare$net1_mask * NetCompare$net2_mask
    diff_pos_net$graph <- NetCompare$`diff_sig_nw1>nw2` * NetCompare$net1_mask * NetCompare$net2_mask
    diff_neg_net$graph <- NetCompare$`diff_sig_nw1<nw2` * NetCompare$net1_mask * NetCompare$net2_mask
  }

  results$diff_plot <- plot(diff_net, maximum= maximum)
  results$diff_pos_plot <- plot(diff_pos_net, maximum= maximum)
  results$diff_neg_plot <- plot(diff_neg_net, maximum= maximum)

  results$diff_plot$Arguments <- network_G$Arguments
  results$diff_plot$plotOptions <- network_G$plotOptions
  results$diff_plot$graphAttributes$Nodes <- network_G$graphAttributes$Nodes
  results$diff_plot$graphAttributes$Knots<- network_G$graphAttributes$Knots
  results$diff_plot$graphAttributes$Edges$curve = rep(network_G$graphAttributes$Edges$curve[1], length(results$diff_plot$graphAttributes$Edges$labels))
  results$diff_plot$graphAttributes$Edges$lty = rep(network_G$graphAttributes$Edges$lty[1], length(results$diff_plot$graphAttributes$Edges$labels))
  results$diff_plot$layout <- network_G$layout
  results$diff_plot$layout.orig <- network_G$layout.orig

  results$diff_plot$plotOptions$legend <- FALSE
  results$diff_plot$plotOptions$drawPies <- FALSE
  results$diff_plot$graphAttributes$Nodes$pie <- NULL

  results$diff_pos_plot$Arguments <- network_G$Arguments
  results$diff_pos_plot$plotOptions <- network_G$plotOptions
  results$diff_pos_plot$graphAttributes$Nodes <- network_G$graphAttributes$Nodes
  results$diff_pos_plot$graphAttributes$Knots<- network_G$graphAttributes$Knots
  results$diff_pos_plot$graphAttributes$Edges$curve = rep(network_G$graphAttributes$Edges$curve[1], length(results$diff_pos_plot$graphAttributes$Edges$labels))
  results$diff_pos_plot$graphAttributes$Edges$lty = rep(network_G$graphAttributes$Edges$lty[1], length(results$diff_pos_plot$graphAttributes$Edges$labels))
  results$diff_pos_plot$layout <- network_G$layout
  results$diff_pos_plot$layout.orig <- network_G$layout.orig

  results$diff_pos_plot$plotOptions$legend <- FALSE
  results$diff_pos_plot$plotOptions$drawPies <- FALSE
  results$diff_pos_plot$graphAttributes$Nodes$pie <- NULL

  results$diff_neg_plot$Arguments <- network_G$Arguments
  results$diff_neg_plot$plotOptions <- network_G$plotOptions
  results$diff_neg_plot$graphAttributes$Nodes <- network_G$graphAttributes$Nodes
  results$diff_neg_plot$graphAttributes$Knots<- network_G$graphAttributes$Knots
  results$diff_neg_plot$graphAttributes$Edges$curve = rep(network_G$graphAttributes$Edges$curve[1], length(results$diff_neg_plot$graphAttributes$Edges$labels))
  results$diff_neg_plot$graphAttributes$Edges$lty = rep(network_G$graphAttributes$Edges$lty[1], length(results$diff_neg_plot$graphAttributes$Edges$labels))
  results$diff_neg_plot$layout <- network_G$layout
  results$diff_neg_plot$layout.orig <- network_G$layout.orig

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

    if (device == 'pdf') {
      pdf(file = path_join(c(path,
                             paste0(prefix,
                                    'diff_network_plot.pdf'))),
          width = width,
          height = height,
          ...)

      plot(results$diff_plot)

      dev.off()

      pdf(file = path_join(c(path,
                             paste0(prefix,
                                    'diff_pos_network_plot.pdf'))),
          width = width,
          height = height,
          ...)

      plot(results$diff_pos_plot)

      dev.off()

      pdf(file = path_join(c(path,
                             paste0(prefix,
                                    'diff_neg_network_plot.pdf'))),
          width = width,
          height = height,
          ...)

      plot(results$diff_neg_plot)

      dev.off()

    } else if (device == 'svg') {
      svg(file = path_join(c(path,
                             paste0(prefix,
                                    'diff_network_plot.svg'))),
          width = width,
          height = height)

      plot(results$diff_plot)

      dev.off()

      svg(file = path_join(c(path,
                             paste0(prefix,
                                    'diff_pos_network_plot.svg'))),
          width = width,
          height = height)

      plot(results$diff_pos_plot)

      dev.off()

      svg(file = path_join(c(path,
                             paste0(prefix,
                                    'diff_neg_network_plot.svg'))),
          width = width,
          height = height,
          ...)

      plot(results$diff_neg_plot)

      dev.off()
    }
  }
  return(results)
}
