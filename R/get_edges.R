#' Get edge index of one or two networks
#' @param net1 a network. Should be either the product of \code{qgraph} or \code{quickNet}.
#' @param net2 alternative. a network. Should be either the product of \code{qgraph} or \code{quickNet}.
#' @param method only works when net2 is provided. \itemize{
#' \item{"union": return the edges that exist in net1 or net2, default.}
#' \item{"intersect": return the edges that both exist in net1 and net2.}
#' }
#'
#' @return a list with n vectors of length 2. The vectors represent the node index. n is the number of edges. The result can be provided to the \code{edges} parameter of \code{NetCompare} or \code{NCT} to specify the edges that you want to test.
#' @export
#'
#' @examples
#'
#' data("mtcars")
#' net1 <- quickNet(mtcars)
#' net2 <- quickNet(mtcars^3)
#' edges1 <- get_edges(net1)
#' edges2 <- get_edges(net1, net2, method = 'intersect')
#'
get_edges <- function(net1, net2 = NULL, method = 'union') {

  if (is.null(net2)) {

    edge_data <- net1$Edgelist %>% as.data.frame()

    edges <- list()

    for (i in seq(nrow(edge_data))) {
      edges[[i]] <- c(edge_data$from[i],edge_data$to[i])
    }

    } else {

        edge_data1 <- net1$Edgelist %>% as.data.frame() %>%
          mutate(pair = paste(from,to,sep = '_'))
        edge_data2 <- net2$Edgelist %>% as.data.frame()%>%
          mutate(pair = paste(from,to,sep = '_'))

        if (!(method %in% c('union','intersect'))) {
          stop('method should be one of "union" and "intersect".')
        }

        if (method == 'union') {

          edge_data_combine <- rbind(edge_data1,edge_data2) %>%
            dplyr::filter(!duplicated(pair))

          edges <- list()

          for (i in seq(nrow(edge_data_combine))) {
            edges[[i]] <- c(edge_data_combine$from[i],edge_data_combine$to[i])
          }

        } else if (method == 'intersect') {

            edge_data_intersect <- edge_data1 %>%
              dplyr::filter(pair %in% edge_data2$pair)

            edges <- list()

            for (i in seq(nrow(edge_data_intersect))) {
              edges[[i]] <- c(edge_data_intersect$from[i],edge_data_intersect$to[i])
          }
        }
  }
  return(edges)
}
