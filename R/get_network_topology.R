#' Create network topology
#'
#' Generates a data frame containing network topology information from either a
#' pre-built graph object or directly from an adjacency matrix.
#'
#' @param graph_obj An graph object from build_graph_from_mat or build_graph_from_df.
#'   The network object to be visualized.
#' @param matrix The matrix to build graph_obj
#' @param bootstrap Numeric  (default = 100).
#' Number of bootstrap iterations for stability analysis
#'
#' @returns data frame of network topolog
#' @export
#'
#' @examples NULL
get_network_topology <- function(graph_obj,
                                 matrix,
                                 bootstrap = 100){

  # create igraph object
  ig <- tidygraph::as.igraph(graph_obj)

  .network.efficiency <- function(ig){
    dd <- 1/igraph::distances(ig)
    diag(dd) <- NA
    efficiency <- mean(dd, na.rm=T)
    return(efficiency)
  }

  .info.centrality.vertex <- function(ig, net=NULL, verbose=F){
    if(is.null(net)) {
      net <- .network.efficiency(ig)
      }
    if(is.numeric(net)==F){
      net <- .network.efficiency(ig)
    }
    count <- c()
    for(i in 1:length(igraph::V(ig))){
      count <- c(count, (net-.network.efficiency(igraph::delete_vertices(ig, i)))/net)
      if(verbose){
        print(paste("node",i,"current\ info\ score", count[i], collapse="\t"))
      }
    }
    return(count)
  }


  # get node and edge number
  .get_topology <- function(ig){

    # compute network topology
    # node number
    node_number <- igraph::vcount(ig)

    # edge number
    edge_number <- igraph::ecount(ig)

    # degree
    degree_average <- mean(igraph::degree(ig, mode = "all"))
    degree_average_out <- mean(igraph::degree(ig, mode = "out"))
    degree_average_in <- mean(igraph::degree(ig, mode = "in"))
    degree_average_total <- mean(igraph::degree(ig, mode = "total"))
    # same output

    # mean_distance
    distance_mean <- igraph::mean_distance(ig, directed = FALSE)

    # diameter
    dia <- igraph::diameter(ig, directed = FALSE)

    # transitivity (global/local)
    clust_global <- igraph::transitivity(ig, type = "global")
    clust_local  <- mean(igraph::transitivity(ig, type = "local"), na.rm = T)

    # density
    density <- igraph::edge_density(ig)

    # shortest path between vertices
    network_efficiency <- .network.efficiency(ig)

    # Information Centrality
    network_info.centrality <- sum(.info.centrality.vertex(ig))

    # betweenness
    betweenness_vals <- mean(igraph::betweenness(ig), na.rm = T)
    betweenness_edge_vals <- mean(igraph::edge_betweenness(ig), na.rm = T)

    # closeness
    closeness_vals <- mean(igraph::closeness(ig), na.rm = T)
    closeness_vals_out <- mean(igraph::closeness(ig, mode = "out"), na.rm = T)
    closeness_vals_in <- mean(igraph::closeness(ig, mode = "in"), na.rm = T)
    closeness_vals_all <- mean(igraph::closeness(ig, mode = "all"), na.rm = T)
    closeness_vals_total <- mean(igraph::closeness(ig, mode = "total"), na.rm = T)
    # same output


    # eigen_centrality
    eigen_vals <- mean(igraph::eigen_centrality(ig)$vector, na.rm = T)

    # modularity
    fc <- igraph::cluster_fast_greedy(ig)
    modularity <- igraph::modularity(ig, igraph::membership(fc))

    # K-core decomposition of graphs
    kcore_vals  <- igraph::coreness(ig)
    kcore_mean  <- mean(kcore_vals, na.rm = T)
    kcore_max   <- max(kcore_vals, na.rm = T)
    kcore_min   <- min(kcore_vals, na.rm = T)

    # Cohension from igraph
    cohesion <- igraph::cohesion(ig)
    vertex_cohesion <- igraph::vertex_connectivity(ig)
    edge_cohesion   <- igraph::edge_connectivity(ig)

    # Cohension
    ## Positive Cohension


    ## Negative Cohension


    # Robustness
    ## Robustness_weight

    ## Robustness_unweight


    # Vulenrability


    # Stability

    out <- data.frame(
      Node = node_number,
      Edge = edge_number,
      Degree = degree_average,
      Distance = distance_mean,
      Diameter = dia,
      Density = density,
      Transitivity_global = clust_global,
      Transitivity_local = clust_local,
      Betweenness = betweenness_vals,
      Betweenness_edge = betweenness_edge_vals,
      Closeness = closeness_vals,
      Eigen_centrality = eigen_vals,
      Modularity = modularity,
      K_core_mean = kcore_mean,
      K_core_max = kcore_max,
      K_core_min = kcore_min,
      Network_efficiency = network_efficiency,
      Network_info.centrality = network_info.centrality,
      Cohension_Positive = NA,
      Cohension_Negative = NA,
      Robustness_weight = NA,
      Robustness_unweight = NA,
      Vulenrability = NA,
      Stability = NA
      )

    return(out)


  }

  # node topology
  network_topology <- .get_topology(ig = ig) %>%
    tibble::rownames_to_column(var = "ID") %>%
    tidyr::pivot_longer(cols = -ID, names_to = "Kind", values_to = "Value") %>%
    dplyr::select(-ID) %>%
    purrr::set_names(c("Topology", "Target_network"))

  network_topology

  # random topology
  random_topology <- list()
  # random network and topology
  for (i in 1:bootstrap) {

    random_graph <- igraph::sample_gnm(n = igraph::vcount(ig),
                                       m = igraph::ecount(ig),
                                       directed = F,
                                       loops = F)
    # 获取属性
    random_topology[[i]] <- .get_topology(ig = random_graph)
  }

  random_topology_df <- do.call(rbind, random_topology)

  # output
  random_topology_df_mean <- colMeans(random_topology_df) %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "Kind") %>%
    purrr::set_names(c("Topology", "Random_nerwork"))



  # output topology
  out <- dplyr::left_join(network_topology,
                          random_topology_df_mean,
                          by = "Topology"
                          )

  return(out)


}
