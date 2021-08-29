#' @title get bridge plot from bridge results.
#' @description a wrapper function of \code{ggplot2::ggsave}
#' @importFrom stringr str_sub
#' @importFrom dplyr select contains mutate everything
#' @importFrom fs path_join
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggsave
#' @importFrom purrr map
#' @param bridge output from \code{quickNet::Bridge}
#' @param prefix the prefix of output plot files.
#' @param path the path of output files, can be either a relative or absolute path.
#' @param device any format supported by \code{ggplot2::ggsave}.
#' @param width the width of plot, in inch.
#' @param height the height of plot, in inch.
#' @param get.table logical. whether get the csv file of bridge coefficient. Default is TRUE.
#' @param ... other parameter from \code{ggplot2::ggsave}.
#'
#' @return a Bridge plot will be output to the specified path.
#' @export
#'
#' @examples
#' data('mtcars')
#' Bridge <- Bridge(quickNet(mtcars), communities = c(rep(1,5),rep(2,6)))
#' get_bridge_plot(Bridge, prefix = 'test')
#'

get_bridge_plot <- function(bridge, prefix = '', path = '.', device = 'pdf', width = 10, height = 7, get.table = TRUE,...) {

  if (str_sub(prefix,-1) %in% c('_','.','')) {
    prefix <- prefix
  } else {
    prefix <- paste0(prefix,'_')
  }

  ggsave(filename = path_join(c(path,
                                paste0(prefix,
                                       'bridge_plot',
                                       '.',
                                       device))),
         plot = bridge$bridgePlot,
         width = width,
         height = height, ...)

  if (get.table) {
    bridge$bridge_data %>%
      map(~ .x) %>%
      as.data.frame() %>%
      mutate(Nodes = row.names(.)) %>%
      select(Nodes, everything()) %>%
      write.csv(path_join(c(path,
                            paste0(prefix,
                                   'bridge_table.csv'))),
                row.names = FALSE)
  }
}
