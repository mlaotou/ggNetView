#' Visualize multi-orientation environmental–species correlation heatmaps2
#'
#' @param Environment character
#' The file Path of Environment data
#' @param Experiment character
#' The file Path of Experiment data
#' @param edge character
#' The file Path of edge data
#' @param node character
#' The file Path of node data
#' @param r numeric (default = 6)
#'
#' @returns a ggplot2 object
#' @export
#'
#' @examples NULL
gglink_heatmap_triple <- function(
    Environment,
    Experiment,
    edge,
    node,
    r = 6
){

  # Environment Data
  Environment <- readr::read_delim(file = Environment, delim = ",") %>%
    tibble::column_to_rownames(var = "Sample")

  # Experiment Data
  Experiment <- readr::read_delim(file = Experiment, delim = ",") %>%
    tibble::column_to_rownames(var = "Sample")

  # edge Data
  edge <- readr::read_delim(file = edge, delim = ",")

  # node Data
  node <- readr::read_delim(file = node, delim = ",")


  # Correlation
  stat_out <- cor_test2(Environment, Experiment)

  graph_obj <- tidygraph::tbl_graph(nodes = node, edges = edge)

  # layout
  layout_manual <- create_layout2(graph_obj,
                                  hub_names = NULL,
                                  r = r)

  p <- ggraph::ggraph(layout_manual)  +
    ggraph::geom_edge_link(aes(color = weight, width = weight)) +
    ggraph::scale_edge_color_gradientn(colors = c("#74add1","#abd9e9","#ffffbf","#fdae61","#f46d43"),
                               guide = ggraph::guide_edge_colorbar(direction = "horizontal",
                                                           title.position = "top")) +
    ggraph::scale_edge_width(range = c(0.1, 1),
                     guide = ggplot2::guide_legend(nrow = 2,
                                          direction = "horizontal",
                                          title.position = "top"))  +
    ggnewscale::new_scale_fill() +
    ggplot2::geom_tile(data = stat_out[[1]],
              aes(x = ID2, y = Type2), fill = "white", color = "#000000", linewidth = 0.5, inherit.aes = F) +
    ggplot2::geom_text(data = stat_out[[1]],
              aes(x = ID2, y = 13, label = ID),inherit.aes = F) +
    ggplot2::geom_text(data = stat_out[[1]],
              aes(x = 13, y = Type2, label = Type), hjust = "left") +
    ggplot2::geom_point(data = stat_out[[1]],
               aes(x = ID2, y = Type2, fill = Value, size = abs(Value)), shape = 21, color = "black") +
    ggplot2::geom_text(data = stat_out[[2]],
              aes(x = ID2, y = Type2, label = p_value),
              size = 7.5) +
    ggplot2::scale_fill_gradient(low = "#edf8b1", high = "#1d91c0", name = "Env Cor",
                        guide = ggplot2::guide_colorbar(direction = "horizontal",
                                               title.position = "top")) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::scale_y_discrete(position = "right") +
    ggplot2::scale_size(range = c(6,16),
               guide = ggplot2::guide_legend(direction = "horizontal",
                                    title.position = "top"),
               name = "Env Cor Size") +
    ggplot2::xlab('') +
    ggplot2::ylab('')  +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::geom_segment(data = stat_out[[3]] ,
                 mapping = aes(x = p_start1, y = p_end1, xend = start2, yend = end2,
                               linetype = p_value,
                               color = Value,
                               linewidth = abs(Value))) +
    ggplot2::scale_color_gradient(low = "#e0f3db", high = "#4eb3d3", name = "Correlation",
                         guide = ggplot2::guide_colorbar(direction = "horizontal",
                                                title.position = "top")) +
    ggplot2::scale_linewidth(range = c(1, 2.5),
                    name = "Cor",
                    guide = ggplot2::guide_legend(direction = "horizontal",
                                         nrow = 2,
                                         title.position = "top")) +
    ggplot2::scale_linetype(name = "PValue",
                   guide = ggplot2::guide_legend(direction = "horizontal",
                                        nrow = 2,
                                        title.position = "top")) +
    ggplot2::geom_point(data = stat_out[[3]],
               mapping = aes(x = p_start1, y = p_end1), fill = "#9e9ac8", size = 5, shape = 21) +
   ggnewscale::new_scale_fill() +
    ggraph::geom_node_point(data = layout_manual, aes(fill = annotation, shape = annotation), size = 13.5) +
    ggplot2::scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f"),
                      guide = ggplot2::guide_legend(title.position = "top",
                                           nrow = 2)) +
    ggplot2::scale_shape_manual(values = c(21,21:25),
                       guide = ggplot2::guide_legend(title.position = "top",
                                            nrow = 2)) +
    ggraph::geom_node_text(data = layout_manual %>% tidygraph::slice(-c(1:31)),
                   aes(label = node)) +
    ggraph::geom_node_text(
      data = layout_manual %>% tidygraph::slice(1:31),
      aes(x =  x + 0.25,
          y =  y,
          label = node,
      ),
      color = "#000000",
      size = 3,
      hjust = 'outward'
    ) +
    ggplot2::guides(shape = ggplot2::guide_legend(nrow = 2)) +
    ggplot2::coord_equal(clip = "off") +
    ggraph::theme_graph() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, family = "bold"),
      plot.margin = ggplot2::margin(1,1,1,1,"cm"),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(hjust = 0.5),
      legend.ticks = ggplot2::element_line(color = "#000000"),
      legend.frame = ggplot2::element_rect(color = "#000000")
    )

  return(p)
}
