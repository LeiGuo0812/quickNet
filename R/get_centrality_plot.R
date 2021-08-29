#' @title get network plot from stability results.
#' @description a wrapper function of \code{ggplot2::ggsave}
#' @importFrom stringr str_sub
#' @importFrom dplyr select contains mutate everything
#' @importFrom fs path_join
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggsave
#' @param centrality output from \code{quickNet::Centrality}
#' @param prefix the prefix of output plot files.
#' @param path the path of output files, can be either a relative or absolute path.
#' @param device any format supported by \code{ggplot2::ggsave}.
#' @param width the width of plot, in inch.
#' @param height the height of plot, in inch.
#' @param get.table logical. whether get the csv file of centrality. Default is TRUE.
#' @param ... other parameter from \code{ggplot2::ggsave}.
#'
#' @return a centrality plot will be output to the specified path.
#' @export
#'
#' @examples
#' data('mtcars')
#' centrality <- Centrality(EBICglassoNet(mtcars))
#' get_centrality_plot(centrality, prefix = 'test')
#'

get_centrality_plot <- function(centrality, prefix = '', path = '.', device = 'pdf', width = 10, height = 7, get.table = TRUE,...) {

  if (str_sub(prefix,-1) %in% c('_','.','')) {
    prefix <- prefix
  } else {
    prefix <- paste0(prefix,'_')
  }

  ggsave(filename = path_join(c(path,
                                paste0(prefix,
                                       'centrality_plot',
                                       '.',
                                       device))),
         plot = centrality$centralityPlot,
         width = width,
         height = height, ...)

  if (get.table) {
    centrality$centrality_data %>%
      as.data.frame() %>%
      select(-contains('ShortestPath')) %>%
      mutate(Nodes = row.names(.)) %>%
      select(Nodes, everything()) %>%
      write.csv(path_join(c(path,
                            paste0(prefix,
                                   'centrality_table.csv'))),
                row.names = FALSE)
  }
}
