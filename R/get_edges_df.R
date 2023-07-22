#' Get data frame of edges and weights of one or two networks
#' @param net1 a network. Should be either the product of \code{qgraph} or \code{quickNet}.
#' @param net2 alternative. a network. Should be either the product of \code{qgraph} or \code{quickNet}.
#' @param method only works when net2 is provided. \itemize{
#' \item{"union": return the edges that exist in net1 or net2, default.}
#' \item{"intersect": return the edges that both exist in net1 and net2.}
#' }
#' @param labels the name of each node. If provided, the nodes will be named by labels.
#' @return a data frame with 3 columns. The vectors in from and to represent the node index.
#' @export
#'
#' @examples
#'
#' data("mtcars")
#' net1 <- quickNet(mtcars)
#' net2 <- quickNet(mtcars^3)
#' edges1 <- get_edges_df(net1)
#' edges2 <- get_edges_df(net1, net2, method = 'intersect')
#'
get_edges_df <- function(net1, net2 = NULL, method = 'union', labels = NULL) {

  edges <- list(
    from = NA,
    to = NA,
    weight = NA
  )

  if (is.null(net2)) {

    edge_data <- net1$Edgelist %>% as.data.frame()

    edges$from <- edge_data$from
    edges$to <- edge_data$to
    edges$weight <- edge_data$weight

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

      edges$from <- edge_data_combine$from
      edges$to <- edge_data_combine$to
      edges$weight <- edge_data_combine$weight

    } else if (method == 'intersect') {

      edge_data_intersect <- edge_data1 %>%
        dplyr::filter(pair %in% edge_data2$pair)

      edges$from <- edge_data_intersect$from
      edges$to <- edge_data_intersect$to
      edges$weight <- edge_data_intersect$weight
    }
  }

  edges <- as.data.frame(edges)

  edges_result <- edges

  if (!is.null(labels)) {
    for (i in 1:nrow(edges_result)) {
      edges_result[i,'from'] <- labels[edges[i,'from']]
      edges_result[i,'to'] <- labels[edges[i,'to']]
    }
  }

  return(edges_result)
}

