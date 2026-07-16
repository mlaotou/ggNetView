#' Extract subgrah from graph object
#'
#' @param graph_obj An graph object from build_graph_from_mat or build_graph_from_df.
#' The network object to be visualized.
#' @param select_module a character vectors
#' Select the module name in graph object
#'
#' @returns list
#' @export
#'
#' @examples
#' data(ppi_example)
#' obj <- build_graph_from_df(
#'   df              = ppi_example$ppi,
#'   node_annotation = ppi_example$annotation
#' )
#' sg <- get_subgraph(obj, select_module = "1")
#' names(sg)
#' sg$stat_module
get_subgraph <- function(graph_obj, select_module = NULL){

  # get obj
  obj <- graph_obj

  node_df <- obj %>%
    tidygraph::activate(nodes) %>%
    tidygraph::as_tibble()

  # module labels derived from the actual split groups, so this works whether
  # `Modularity` is a factor OR a plain character column. (The old code used
  # levels(), which returns NULL for a character vector and then silently
  # produced an empty subgraph list.)
  module_splits <- dplyr::group_split(node_df, Modularity)
  module_name <- vapply(module_splits,
                        function(g) as.character(g$Modularity[[1]]),
                        character(1))

  # get module list
  module_list <- module_splits
  names(module_list) <- module_name

  # get module ID: prefer the explicit `name` column instead of assuming the
  # first column is the node identifier.
  id_list <- purrr::map(module_splits,
                        function(g) if ("name" %in% names(g)) g[["name"]] else g[[1]])
  names(id_list) <- module_name

  # create sub_graph object
  sub_graph <- list()

  for (i in module_name) {

    # induced_subgraph() replaces the soft-deprecated igraph::subgraph();
    # both select vertices by name/id with identical semantics.
    sub_graph[[i]] <- igraph::induced_subgraph(tidygraph::as.igraph(obj),
                                               id_list[[i]]) %>%
      tidygraph::as_tbl_graph()

  }

  # stat
  stat_module <- purrr::map(id_list, ~length(.x)) %>%
    do.call(rbind, .) %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "Module") %>%
    dplyr::rename(Number = V1)

  # use message() instead of print() so non-interactive use (knitr, plumber,
  # tests) does not have its stdout polluted with the module summary table.
  message(paste(utils::capture.output(stat_module), collapse = "\n"))


  if (!is.null(select_module)) {
    graph_select <- obj %>%
      tidygraph::filter(as.character(Modularity) %in% select_module)
  }else{
    graph_select <- NULL
  }


  return(list(sub_graph_all = sub_graph,
              stat_module = stat_module,
              sub_graph_select = graph_select))

}

