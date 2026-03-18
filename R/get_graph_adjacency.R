#' Extract adjacency matrix from graph object
#'
#' Returns the adjacency matrix of the graph with node names as rownames and
#' colnames. Non-zero entries indicate edges; values correspond to edge weights
#' when the graph is weighted.
#'
#' @param graph_obj A graph object from \code{build_graph_from_mat} or \code{build_graph_from_df}.
#'
#' @returns A numeric matrix with rownames and colnames set to node IDs (\code{name}).
#'
#' @export
#'
#' @examples NULL
get_graph_adjacency <- function(graph_obj) {
  nodes <- tidygraph::as_tibble(graph_obj)
  adj_mat <- as.matrix(igraph::as_adjacency_matrix(tidygraph::as.igraph(graph_obj)))
  rownames(adj_mat) <- colnames(adj_mat) <- nodes$name
  adj_mat
}
