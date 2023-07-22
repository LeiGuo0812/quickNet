#' @title get network plot from qgraph objects.
#' @param network a qgraph object.
#' @param prefix the prefix of output plot files.
#' @param path the path of output files, can be either a relative or absolute path.
#' @param device 'pdf' or 'svg', deciding the output plot format.
#' @param width the width of plot, in inch.
#' @param height the height of plot, in inch.
#' @param get.matrix logical. whether get the csv file of graph matrix. Default is TRUE.
#' @param ... other parameter from \code{pdf} or \code{svg}.
#'
#' @return a network plot will be output to the specified path.
#' @export
#'
#' @examples
#'
#' data('mtcars')
#' network <- quickNet(mtcars)
#' get_network_plot(network, prefix = 'test')
#'

get_network_plot <- function(network, prefix = '', path = '.', device = 'pdf', width = 10, height = 7, get.matrix = TRUE, ...){

  if (str_sub(prefix,-1) %in% c('_','.','')) {
    prefix <- prefix
  } else {
    prefix <- paste0(prefix,'_')
  }

  if (device == 'pdf') {
    pdf(file = path_join(c(path,
                           paste0(prefix,
                                  'network_plot.pdf'))),
        width = width,
        height = height,
        ...)

    plot(network)

    dev.off()

  } else if (device == 'svg'){

    svg(file = path_join(c(path,
                           paste0(prefix,
                                  'network_plot.svg'))),
        width = width,
        height = height,
        ...)

    plot(network)

    dev.off()
  }

  if (get.matrix) {
    write.csv(network$graphData$graph, path_join(c(path,
                                                   paste0(prefix,
                                                          'network_matrix.csv'))),
              row.names = TRUE)
  }
}
