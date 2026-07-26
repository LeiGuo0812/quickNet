#' Get data frame of edges and weights of one or two networks
#' @param net1 a network. Should be either the product of \code{qgraph} or \code{quickNet}.
#' @param net2 alternative. a network. Should be either the product of \code{qgraph} or \code{quickNet}.
#' @param method only works when net2 is provided. \itemize{
#' \item \code{"union"}: return the edges that exist in net1 or net2, default.
#' \item \code{"intersect"}: return the edges that both exist in net1 and net2.
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

  if (is.null(net2)) {

    edge_data <- quicknet_edgelist(
      quicknet_network_matrix(net1),
      directed = inherits(net1, "quicknet_fit") && isTRUE(net1$meta$directed)
    ) %>% as.data.frame()

    edges <- edge_data[, c("from", "to", "weight"), drop = FALSE]

  } else {

    edge_data1 <- quicknet_edgelist(
      quicknet_network_matrix(net1),
      directed = inherits(net1, "quicknet_fit") && isTRUE(net1$meta$directed)
    ) %>% as.data.frame() %>%
      mutate(pair = paste(from,to,sep = '_'))
    edge_data2 <- quicknet_edgelist(
      quicknet_network_matrix(net2),
      directed = inherits(net2, "quicknet_fit") && isTRUE(net2$meta$directed)
    ) %>% as.data.frame()%>%
      mutate(pair = paste(from,to,sep = '_'))

    if (!(method %in% c('union','intersect'))) {
      stop('method should be one of "union" and "intersect".')
    }

    if (method == 'union') {

      edge_data_combine <- rbind(edge_data1,edge_data2) %>%
        dplyr::filter(!duplicated(pair))

      edges <- edge_data_combine[, c("from", "to", "weight"), drop = FALSE]

    } else if (method == 'intersect') {

      edge_data_intersect <- edge_data1 %>%
        dplyr::filter(pair %in% edge_data2$pair)

      edges <- edge_data_intersect[, c("from", "to", "weight"), drop = FALSE]
    }
  }

  edges_result <- as.data.frame(edges)

  if (!is.null(labels)) {
    edge_indices <- c(edges_result$from, edges_result$to)
    if (length(edge_indices) > 0 && (
      any(!is.finite(edge_indices)) ||
      any(edge_indices < 1 | edge_indices > length(labels))
    )) {
      stop("labels must contain one entry for every node index.", call. = FALSE)
    }
    for (i in seq_len(nrow(edges_result))) {
      edges_result[i,'from'] <- labels[edges_result[i,'from']]
      edges_result[i,'to'] <- labels[edges_result[i,'to']]
    }
  }

  return(edges_result)
}

