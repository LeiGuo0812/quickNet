#' @title Get stability plot from stability results
#' @importFrom stringr str_sub
#' @importFrom fs path_join
#' @importFrom dplyr select mutate everything
#' @param stability output from \code{quickNet::Stability}.
#' @param prefix the prefix of output plot files.
#' @param path the path of output files, can be either a relative or absolute path.
#' @param device 'pdf' or 'svg', deciding the output plot format.
#' @param width the width of plot, in inch.
#' @param height the height of plot, in inch.
#' @param get.table logical. whether get the csv file of CS-coefficient. Default is TRUE.
#' @param ... other parameter from \code{pdf} or \code{svg}.
#' @return four plot files will be output to the specified path, if the bridge stability is calculated, the outputs will also contain the bridge stability plot.
#' @export
#'
#' @examples
#'data('mtcars')
#'stability <- Stability(mtcars, nboot = 10)
#'get_stability_plot(stability, prefix = 'test', path = tempdir())
#'

get_stability_plot <- function(stability, prefix = '', path = '.', device = 'pdf', width = 10, height = 7, get.table = TRUE, ...){

  if (str_sub(prefix,-1) %in% c('_','.','')) {
    prefix <- prefix
  } else {
    prefix <- paste0(prefix,'_')
  }

  device <- match.arg(device, c("pdf", "svg"))
  plot_specs <- list(
    edge_weight_CI_plot = stability$edge_weight_CI_plot,
    edge_weight_diff_plot = stability$edge_weight_diff_plot,
    centrality_stability_plot = stability$centrality_stability_plot,
    centrality_diff_plot = stability$centrality_diff_plot
  )
  if (!is.null(stability$bridge_stability_plot)) {
    plot_specs$bridge_stability_plot <- stability$bridge_stability_plot
  }
  for (plot_name in names(plot_specs)) {
    if (is.null(plot_specs[[plot_name]])) next
    quicknet_plot_to_device(
      filename = path_join(c(path, paste0(prefix, plot_name, ".", device))),
      device = device,
      width = width,
      height = height,
      plot_function = local({
        current_plot <- plot_specs[[plot_name]]
        function() print(current_plot)
      }),
      ...
    )
  }

  if (get.table && !is.null(stability$CS_coefficient)) {
    stability$CS_coefficient %>%
      as.data.frame() %>%
      `colnames<-`('CS-coefficient') %>%
      mutate(Measure = rownames(.)) %>%
      select(Measure, everything()) %>%
      write.csv(path_join(c(path,
                            paste0(prefix,
                                   'CS_coefficient_table.csv'))),
                row.names = FALSE)
  }
}

