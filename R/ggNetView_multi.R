#' Visualize a network with custom layouts in groups
#'
#' @param mat Numeric matrix.
#' A numeric matrix with samples in rows and variables in columns.
#' @param group DataFrame
#' The dataframe of group
#' @param layout Character string.
#' Custom layouts; one of "gephi", "square", "square2", "petal",
#' "petal2", "heart_centered","diamond", "star", "star_concentric","rectangle,
#' "rightiso_layers" etc.
#' @param node_add Integer (default = 7).
#' Number of nodes to add in each layer of the layout.
#' @param ring_n Numeric (default = 7)
#' Numbers of ring in rings layout.
#' @param r Numeric (default = 1).
#' Radius increment for concentric or layered layouts.
#' @param center Logical (default = TRUE).
#' Whether to place a node at the center of the layout.
#' @param idx Optional.
#' Index of nodes to be emphasized or centered in the layout
#' @param shrink Numeric (default = 1).
#' Shrinkage factor applied to the center points.
#' @param k_nn Numeric (default = 8).
#' Number of nearest neighbors used to build the local adjacency graph.
#' @param push_others_delta Numeric (default = 0).
#' Radial offset applied to the "Others" module to slightly
#' @param layout.module Character  (default = "random")
#‘ - random : modules are distributed more randomly and independently.
#' - adjacent : modules are positioned close to each other, minimizing inter-module gaps.
#' - order : modules are distributed by order, applicable to `Bipartite, Tripartite, Quadripartite, Multipartite, Pentapartite Layout`
#' @param shape Integer  (default = 21).
#' The point shape likely in ggplot2.
#' @param pointalpha Integer  (default = 1).
#' The point alpha
#' @param pointsize Vector (default =  c(1,10))
#' The point size rang.
#' @param pointstroke Integer  (default = 0.3).
#' @param group.by Character (default = "Modularity").
#' Change group for nodes
#' @param fill.by Character (default = "Modularity").
#' Change fill for nodes
#' @param jitter Logical (default = FALSE).
#' Whether to apply jitter to points.
#' @param jitter_sd  Integer  (default = 0.1).
#' The standard deviation of the jitter applied when `jitter = TRUE`.
#' @param mapping_line  Logical (default = FALSE).
#' Whether to mapping line in ggNetView.
#' @param linealpha  Integer  (default = 0.25).
#' Change  line alpha.
#' @param linecolor Character  (default = "grey70").
#' Change  line color.
#' @param label Logical (default = FALSE).
#' Whether to display node labels in the center points.
#' @param labelsize Integer  (default = 10).
#' Change Module label size.
#' @param labelsegmentsize Integer  (default = 1).
#' Change  label segment size.
#' @param labelsegmentalpha Integer  (default = 1).
#' Change  label segment alpha.
#' @param add_outer Logical (default = FALSE).
#' Whether to add an outer circle/border around the layout.
#' @param outerwidth Integer  (default = 1.25).
#' Change  outer linewidth.
#' @param outerlinetype Integer  (default = 2).
#' Change  outer linetype.
#' @param outeralpha Integer  (default = 0.5).
#' Change  outer alpha.
#' @param nodelabsize Integer  (default = 5).
#' Change  node label size.
#' @param remove Logical (default = FALSE).
#' Delect nodes that are not modules.
#' @param orientation Character string.
#' Custom orientation; one of "up","down","left","right".
#' @param angle Integer  (default = 0).
#' Change  orientation angle.
#' @param scale Logical  (default = T).
#' modules applicable to `Bipartite, Tripartite, Quadripartite, Multipartite, Pentapartite Layout` to scale the radius
#' @param anchor_dist Integer (default = 10)
#' the distance of each modules, applicable to `Bipartite, Tripartite, Quadripartite, Multipartite, Pentapartite Layout`
#' @param seed Integer (default = 1115).
#' Random seed for reproducibility.

#'
#' @returns A ggplot object representing the network visualization.
#' @export
#'
#' @examples NULL
ggNetView_multi <- function(mat,
                            group,
                            layout = NULL,
                            node_add = 7,
                            ring_n = NULL,
                            r = 1,
                            center = TRUE,
                            idx = NULL,
                            shrink = 1,
                            k_nn = 8,
                            push_others_delta = 0,
                            layout.module = c("random", "adjacent", "order"),
                            shape = 21,
                            pointalpha = 1,
                            pointsize = c(1,10),
                            pointstroke = 0.3,
                            group.by = "Modularity",
                            jitter = FALSE,
                            jitter_sd = 0.1,
                            mapping_line = FALSE,
                            linealpha = 0.25,
                            linecolor = "grey70",
                            label = FALSE,
                            labelsize = 10,
                            labelsegmentsize = 1,
                            labelsegmentalpha = 1,
                            add_outer = FALSE,
                            outerwidth = 1.25,
                            outerlinetype = 2,
                            outeralpha = 0.5,
                            nodelabsize = 5,
                            remove = FALSE,
                            orientation = "up",
                            angle = 0,
                            scale = T,
                            anchor_dist = 6,
                            seed = 1115){

}
