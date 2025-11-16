#' Create network topology
#'
#' @param graph_obj An graph object from build_graph_from_mat or build_graph_from_df.
#'   The network object to be visualized.
#'
#' @returns data frame of network topolog
#' @export
#'
#' @examples NULL
get_network_topology <- function(graph_obj, bootstrap = 1000){


  # get node and edge number
  .get_topology <- function(graph_obj){
    # create igraph object
    ig <- tidygraph::as.igraph(graph_obj)

    # compute network topology
    # degree
    degree_vals <- igraph::degree(ig, mode = "all")

    # mean_distance
    avg_path <- igraph::mean_distance(ig, directed = FALSE)

    # diameter
    dia <- igraph::diameter(ig, directed = FALSE)

    # transitivity (global/local)
    clust_global <- igraph::transitivity(ig, type = "global")
    clust_local  <- igraph::transitivity(ig, type = "local")

    # betweenness
    betweenness_vals <- igraph::betweenness(ig)

    # closeness
    closeness_vals <- igraph::closeness(ig)

    # eigen_centrality
    eigen_vals <- igraph::eigen_centrality(ig)$vector

    # 模块度 / 社区划分
    modularity <- igraph::modularity(fast_greedy_com)

    # Robustness

    # Stability

    # Cohension

    # Position Cohension

    # Negative Cohension

    # Vulenrability


  }

  random_topology <- list()
  # random network and topology
  for (i in 1:bootstrap) {

  }



  # output
  out <- data.frame(
    degree_vals = degree_vals,
    avg_path = avg_path,
    dia = dia,
    clust_global = clust_global,
    clust_local = clust_local,
    betweenness_vals = betweenness_vals,
    closeness_vals = closeness_vals,
    eigen_vals = eigen_vals,
    modularity = modularity

  )

  return(out)


}
