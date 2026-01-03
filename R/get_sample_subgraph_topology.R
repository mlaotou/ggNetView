#' Create network topology
#'
#' Generates a data frame containing network topology information from either a
#' pre-built graph object or directly from an adjacency matrix.
#'
#' @param graph_obj An graph object from build_graph_from_mat or build_graph_from_df.
#'   The network object to be visualized.
#' @param mat Numeric Matrix
#' The matrix to build graph_obj
#' @param bootstrap Numeric  (default = 100).
#' Number of bootstrap iterations for stability analysis
#'
#' @returns data frame of network topolog
#' @export
#'
#' @examples NULL
get_sample_subgraph_topology <- function(graph_obj,
                                         mat = NULL,
                                         transfrom.method = c("none", "scale", "center", "log2", "log10", "ln", "rrarefy", "rrarefy_relative"),
                                         r.threshold = 0.7,
                                         p.threshold = 0.05,
                                         method = c("WGCNA", "SpiecEasi", "SPARCC", "cor"),
                                         cor.method = c("pearson", "kendall", "spearman"),
                                         proc = c("Bonferroni", "Holm", "Hochberg", "SidakSS", "SidakSD","BH", "BY","ABH","TSBH"),
                                         bootstrap = 100){
  # argument
  transfrom.method <-  match.arg(transfrom.method)
  method <- match.arg(method)
  cor.method <- match.arg(cor.method)
  proc <- match.arg(proc)

  # create igraph object
  ig <- tidygraph::as.igraph(graph_obj)

  # mat
  mat <- switch (
    transfrom.method,
    none = mat,
    scale = t(scale(t(mat), scale = T, center = T)),
    center = t(scale(t(mat), scale = F, center = T)),
    log2 = log2(mat + 1),
    log10 = log10(mat + 1),
    ln = log(mat + 1),
    rrarefy = t(vegan::rrarefy(t(mat), min(colSums(mat)))),
    rrarefy_relative = t(vegan::rrarefy(t(mat), min(colSums(mat)))) / colSums(t(vegan::rrarefy(t(mat), min(colSums(mat)))))
  )

}
