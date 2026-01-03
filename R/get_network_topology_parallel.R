#' Create network topology in parallel
#'
#' Generates a data frame containing network topology information from either a
#' pre-built graph object or directly from an adjacency matrix.
#'
#' @param graph_obj An graph object from build_graph_from_mat or build_graph_from_df.
#'   The network object to be visualized.
#' @param mat Numeric Matrix (default = NULL)
#' The matrix to build graph_obj
#' @param transfrom.method Character.
#'Data transformation methods applied before correlation analysis.
#' Options include:
#' "none" (raw data),
#' "scale" (z-score standardization),
#' "center" (mean centering only),
#' "log2" (log2 transfrom),
#' "log10" (log10 transfrom),
#' "ln" (natural transfrom ),
#' "rrarefy" (random rarefaction using \code{vegan::rrarefy}),
#' "rrarefy_relative" (rarefy then convert to relative abundance).
#' @param r.threshold Numeric.
#' Correlation coefficient threshold; edges are kept only if |r| >= r.threshold.
#' @param p.threshold Numeric.
#' Significance threshold for correlations; edges are kept only if p < p.threshold.
#' @param method Character.
#' Relationship analysis methods.
#' Options include: "WGCNA", "SpiecEasi", "SPARCC" and "cor".
#' @param cor.method Character.
#' Correlation analysis method.
#' Options include "pearson", "kendall", and "spearman".
#' @param proc Character.
#' Correlation p-value adjustment methods.
#' Options include:
#' "Bonferroni", "Holm", "Hochberg", "
#' SidakSS", "SidakSD","BH",
#' "BY", "ABH", and "TSBH".
#' @param bootstrap Numeric  (default = 100).
#' Number of bootstrap iterations for stability analysis
#'
#' @returns data frame of network topolog
#' @export
#'
#' @examples NULL
#' @param parallel
#' @param n_workers
#'
#' @returns data frame of network topolog
#' @export
#'
#' @examples NULL
get_network_topology_parallel <- function(graph_obj,
                                          mat = NULL,
                                          transfrom.method = c("none", "scale", "center", "log2", "log10", "ln", "rrarefy", "rrarefy_relative"),
                                          r.threshold = 0.7,
                                          p.threshold = 0.05,
                                          method = c("WGCNA", "SpiecEasi", "SPARCC", "cor"),
                                          cor.method = c("pearson", "kendall", "spearman"),
                                          proc = c("Bonferroni", "Holm", "Hochberg", "SidakSS", "SidakSD","BH", "BY","ABH","TSBH"),
                                          bootstrap = 100,
                                          parallel = FALSE,
                                          n_workers = 1
                                          ){

}
