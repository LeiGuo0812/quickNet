#' @title get stability plot from stability results.
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
#'stability <- Stability(mtcars,nboot = 100)
#'get_stability_plot(stability,prefix = 'test')
#'

get_stability_plot <- function(stability, prefix = '', path = '.', device = 'pdf', width = 10, height = 7, get.table = TRUE, ...){

  if (str_sub(prefix,-1) %in% c('_','.','')) {
    prefix <- prefix
  } else {
    prefix <- paste0(prefix,'_')
  }

  if (device == 'pdf') {
    pdf(file = path_join(c(path, paste0(prefix,'edge_weight_CI_plot.pdf'))), width = width, height = height, ...)
    print(stability$edge_weight_CI_plot)
    dev.off()

    pdf(file = path_join(c(path, paste0(prefix,'edge_weight_diff_plot.pdf'))), width = width, height = height, ...)
    try(print(stability$edge_weight_diff_plot))
    dev.off()

    pdf(file = path_join(c(path, paste0(prefix,'centrality_stability_plot.pdf'))), width = width, height = height, ...)
    print(stability$centrality_stability_plot)
    dev.off()

    pdf(file = path_join(c(path, paste0(prefix,'centrality_diff_plot.pdf'))), width = width, height = height, ...)
    print(stability$centrality_diff_plot)
    dev.off()

    if (!is.null(stability$brige_stability_plot)) {
      pdf(file = path_join(c(path, paste0(prefix,'bridge_stability_plot.pdf'))), width = width, height = height, ...)
      print(stability$brige_stability_plot)
      dev.off()
    }

  } else if (device == 'svg'){

    svg(file = path_join(c(path, paste0(prefix,'edge_weight_CI_plot.svg'))), width = width, height = height, ...)
    print(stability$edge_weight_CI_plot)
    dev.off()

    svg(file = path_join(c(path, paste0(prefix,'edge_weight_diff_plot.svg'))), width = width, height = height, ...)
    try(print(stability$edge_weight_diff_plot))
    dev.off()

    svg(file = path_join(c(path, paste0(prefix,'centrality_stability_plot.svg'))), width = width, height = height, ...)
    print(stability$centrality_stability_plot)
    dev.off()

    svg(file = path_join(c(path, paste0(prefix,'centrality_diff_plot.svg'))), width = width, height = height, ...)
    print(stability$centrality_diff_plot)
    dev.off()

    if (!is.null(stability$brige_stability_plot)) {
      svg(file = path_join(c(path, paste0(prefix,'bridge_stability_plot.svg'))), width = width, height = height, ...)
      print(stability$brige_stability_plot)
      dev.off()
    }
  }

  if (get.table) {
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

