#' Title Calculate Bridge Coefficients
#' @importFrom networktools bridge
#' @importFrom forcats fct_relevel
#' @importFrom dplyr select mutate bind_cols all_of
#' @importFrom tidyr gather
#' @importFrom magrittr %>%
#' @import ggplot2
#' @param net_G a qgraph object, could also be the result of quickNet.
#' @param communities an object of class "communities" (igraph) OR a character vector of community assignments for each node (e.g., c("Comm1", "Comm1", "Comm2", "Comm2)). The ordering of this vector should correspond to the vector from argument "nodes". Can also be in list format (e.g., list("Comm1"=c(1:10), "Comm2"=c(11:20))).
#' @param useCommunities character vector specifying which communities should be included. Default set to "all".
#' @param include the statistics calculated. Default "basic" calculates Bridge strength, Bridge betweenness and Bridge closeness. if "all" or "All", the Bridge expected influence (1-step) and Bridge expected influence (2-step) will also be calculated. You can also specify the statistics you want. See \code{networktools::bridge}.
#' @param normalize logical. Bridge centralities are divided by their highest possible value (assuming max edge strength=1) in order to normalize by different community sizes.
#' @param ... other parameters from \code{networktools::bridge}
#'
#' @return a list contains the bridge coefficient information:\itemize{
#' \item\code{bridgePlot:} the plot of bridge coefficients of each node.
#' \item\code{centrailty_data:} the result of \code{networktools::bridge}.}
#' @export
#'
#' @examples
#' data('mtcars')
#' Bridge <- Bridge(quickNet(mtcars), communities = list(c1 = 1:5, c2 = 6:11))
#'
Bridge <- function(net_G, communities = NULL, useCommunities = "all", include = 'basic', normalize = T,...){

  results <- list()

  bridge_data <- bridge(net_G, communities = communities, useCommunities = useCommunities, normalize = normalize,...)

  plot_data <- suppressMessages(bridge_data %>%
    lapply(as.data.frame) %>%
    bind_cols() %>%
    `colnames<-`(names(bridge_data)) %>%
    mutate(nodes = rownames(.)))

  if (include %in% c('all', 'All')){
    plot_data <- plot_data
  }

  else if (include == 'basic') {
    plot_data <- plot_data %>%
      select(-c(`Bridge Expected Influence (1-step)`,
                       `Bridge Expected Influence (2-step)`))
  }

  else {
    plot_data <- plot_data %>%
      select(all_of(include),communities,nodes)
  }

  plot_data_long <- plot_data %>%
    gather(key = 'measure', value = 'value', -c('communities', 'nodes')) %>%
    mutate(communities = as.factor(communities))

  suppressWarnings(plot_data_long %>%
                     mutate(measure = fct_relevel(measure, c(
                       'Bridge Strength',
                       'Bridge Betweenness',
                       'Bridge Closeness',
                       'Bridge Expected Influence (1-step)',
                       'Bridge Expected Influence (2-step)'
                     ))) %>%
                     ggplot(aes(x = nodes, y = value)) +
                     geom_line(group = 1) +
                     geom_point(aes(color = communities)) +
                     theme_bw() +
                     coord_flip() +
                     facet_wrap(~measure, scales = 'free_x') -> p)

  p

  results$bridgePlot <- p
  results$bridge_data <- bridge_data

  return(results)
}
