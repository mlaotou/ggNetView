#' Visualize multiple networks and show the connections between them to highlight key members within the networks
#'
#' @param mat Numeric matrix.
#' A numeric matrix with samples in rows and variables in columns.
#' @param group_info DataFrame
#' The group information contains: Sample and Group
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
#' #' Significance threshold for correlations; edges are kept only if p < p.threshold.
#' @param method Character.
#' Relationship analysis methods.
#' Options include: "WGCNA", "SpiecEasi", "SPARCC" and "cor".
#' @param cor.method Character.
#' Correlation analysis method.
#' Options include "pearson", "kendall", and "spearman".
#' @param proc  Character.
#' Correlation p-value adjustment methods.
#' Options include:
#' "Bonferroni", "Holm", "Hochberg", "
#' SidakSS", "SidakSD","BH",
#' "BY", "ABH", and "TSBH".
#' @param module.method Character.
#' Network community detection (module identification) method.
#' Options include "Fast_greedy", "Walktrap", "Edge_betweenness", and "Spinglass".
#' @param SpiecEasi.method Character.
#' Method used in \code{SpiecEasi} network inference; options include "mb" and "glasso".
#' @param node_annotation Data frame.
#' Optional node annotation table, containing metadata such as taxonomy or functional categories.
#' @param top_modules Integer.
#' Number of top-ranked modules to retain for downstream visualization or analysis.
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
#' @param pointstroke Integer  (default = 0.3).
#' @param group.by Character (default = "Modularity").
#' Change group for nodes
#' @param fill.by Character (default = "Modularity").
#' Change fill for nodes
#' @param fill Named vector of colors for node/module fill (e.g. \code{c("M1" = "red", "M2" = "blue")}).
#' If \code{NULL} (default), uses viridis discrete fill scale
#' (\code{scale_fill_viridis_d});
#' if provided, uses \code{scale_fill_manual(values = fill)}.
#' @param color Color setting for node/module border.
#' Supports either a single color string (fixed border color) or a named vector
#' (module-to-color mapping, similar to \code{fill}).
#' If \code{NULL}, mapped borders use viridis discrete color scale
#' (\code{scale_color_viridis_d}).
#' @param pointsize Numeric vector of length 2 (default = \code{c(1, 5)}).
#' The range of point size when mapping \code{Degree} to size.
#' First value is minimum size, second is maximum size.
#' @param jitter Logical (default = FALSE).
#' Whether to apply jitter to points.
#' @param jitter_sd  Integer  (default = 0.1).
#' The standard deviation of the jitter applied when `jitter = TRUE`.
#' @param mapping_line Logical or Character (default = FALSE).
#' Whether to map line color in ggNetView. If a character string is provided,
#' it must be a variable name in edge data.
#' @param linealpha  Integer  (default = 0.25).
#' Change  line alpha.
#' @param linecolor Character  (default = "grey70").
#' Change  line color.
#' @param inner_curve Logical (default = FALSE).
#' Whether to draw within-group edges as curves.
#' @param inner_curvature Numeric (default = 0.12).
#' Curvature for within-group edges when \code{inner_curve = TRUE}.
#' @param inner_curve_adaptive Logical (default = TRUE).
#' Whether to adapt within-group edge curvature by edge length when
#' \code{inner_curve = TRUE}.
#' @param inner_curve_adaptive_range Numeric vector of length 2 (default = c(0.7, 1.3)).
#' Multipliers applied to \code{inner_curvature} for shortest and longest
#' within-group edges.
#' @param inner_curve_adaptive_bins Integer (default = 7).
#' Number of bins used to approximate per-edge adaptive curvature for
#' within-group edges.
#' @param add_outer Logical or Character (default = "circle").
#' Add outer boundaries for matched modules.
#' Supported values: \code{"circle"} (use \code{ggforce::geom_mark_circle}),
#' \code{"manual"} (use smoothed polygon boundary like \code{ggNetView}),
#' and \code{"none"} (disable). Logical \code{TRUE}/\code{FALSE} are
#' accepted and mapped to \code{"circle"}/\code{"none"}.
#' @param q_outer Numeric (default = 0.88).
#' Quantile of radial distance used to construct the smooth outer boundary
#' when \code{add_outer = "manual"}.
#' @param expand_outer Numeric (default = 1.02).
#' Global scaling factor applied to the smooth outer boundary when
#' \code{add_outer = "manual"}.
#' @param outerwidth Numeric (default = 1.25).
#' Line width for module outer boundaries.
#' @param outerlinetype Integer or character (default = 2).
#' Linetype for module outer boundaries (e.g. 1 = solid, 2 = dashed).
#' @param outeralpha Numeric (default = 0.5).
#' Alpha for module outer boundaries.
#' @param link_level Character (default = "Module").
#' Cross-group link granularity. One of \code{"None"}, \code{"Module"}, \code{"Node"},
#' \code{"NodeinModule"}, or \code{"Module&Node"}.
#' \code{"None"} draws no cross-group links.
#' \code{"Module"} links significant module matches (module centroids).
#' \code{"Node"} links shared node names across groups.
#' \code{"NodeinModule"} links both module centroids and nodes within significantly overlapping
#' modules (module links show which modules overlap; node links show shared nodes).
#' \code{"Module&Node"} draws both module-level links and node-level links.
#' @param link_curve Logical (default = FALSE).
#' Whether to draw cross-group links as curves (\code{geom_curve}) instead of straight segments.
#' @param link_curvature Numeric (default = 0.2).
#' Curvature used when \code{link_curve = TRUE}.
#' @param link_curve_mode Character (default = "outward").
#' Curve direction strategy used when \code{link_curve = TRUE}.
#' \code{"outward"} bends links away from the global center.
#' \code{"inward"} bends links toward the global center.
#' \code{"cross"} follows a cross-axis rule: left links bend left, right links bend right,
#' upper links bend up, and lower links bend down.
#' @param link_curve_adaptive Logical (default = TRUE).
#' Whether to adapt link curvature by link length when \code{link_curve = TRUE}.
#' Longer links get larger curvature and shorter links get smaller curvature.
#' @param link_curve_adaptive_range Numeric vector of length 2 (default = c(0.7, 1.3)).
#' Multipliers applied to \code{link_curvature} for shortest and longest links.
#' The first value is the minimum multiplier; the second is the maximum multiplier.
#' @param link_curve_adaptive_bins Integer (default = 7).
#' Number of bins used to approximate per-link adaptive curvature.
#' @param link_linetype_node Integer or character (default = 2).
#' Linetype for node-to-node cross-group links (e.g. 2 = dashed, 1 = solid).
#' @param link_linetype_module Integer or character (default = 1).
#' Linetype for module-to-module cross-group links (e.g. 1 = solid, 2 = dashed).
#' @param link_linealpha_node Numeric (default = 0.25).
#' Alpha (transparency) for node-to-node cross-group links.
#' @param link_linealpha_module Numeric (default = 0.5).
#' Alpha (transparency) for module-to-module cross-group links.
#' @param dropOthers Logical (default = FALSE).
#' If TRUE, remove nodes in the \code{"Others"} module from each group's
#' \code{graph_obj} before layout, plotting, and module-overlap comparison.
#' @param calculate_topology Logical (default = FALSE).
#' Whether to compute topology for each group using
#' \code{get_network_topology_parallel()} and
#' \code{get_sample_subgraph_topology_parallel()}.
#' @param scale_groups Logical (default = TRUE).
#' Whether to normalize each group to a comparable coordinate scale before
#' placing groups on anchors for cross-group visual comparison.
#' @param orientation Character string.
#' Custom orientation; one of "up","down","left","right".
#' @param angle Integer  (default = 0).
#' Change  orientation angle.
#' @param anchor_dist Numeric (default = 6).
#' Distance between groups when placing each group network on the outer circle
#' in \code{ggNetView_multi_link}.
#' @param layout_anchor_dist Numeric (default = NULL).
#' Anchor distance passed to the single-group layout function (module spacing
#' within each group). If \code{NULL}, it falls back to \code{anchor_dist}
#' for backward compatibility.
#' @param nrow Integer (default = NULL).
#' Number of layout rows for grid-style layouts (e.g. \code{"consensus_module_equal_gephi"}).
#' @param ncol Integer (default = NULL).
#' Number of layout columns for grid-style layouts (e.g. \code{"consensus_module_equal_gephi"}).
#' @param seed Integer (default = 1115).
#' Random seed for reproducibility.
#'
#' @returns A list containing plot, module-overlap info, link info, group graphs,
#' and optional topology results.
#' @export
#'
#' @examples NULL
ggNetView_multi_link <- function(mat,
                                 group_info,
                                 transfrom.method = c("none", "scale", "center", "log2", "log10", "ln", "rrarefy", "rrarefy_relative"),
                                 r.threshold = 0.7,
                                 p.threshold = 0.05,
                                 method = c("WGCNA", "SpiecEasi", "SPARCC", "cor"),
                                 cor.method = c("pearson", "kendall", "spearman"),
                                 proc = c("Bonferroni", "Holm", "Hochberg", "SidakSS", "SidakSD", "BH", "BY", "ABH", "TSBH"),
                                 module.method = c("Fast_greedy", "Walktrap", "Edge_betweenness", "Spinglass"),
                                 SpiecEasi.method = c("mb", "glasso"),
                                 node_annotation = NULL,
                                 top_modules = 15,
                                 layout = NULL,
                                 node_add = 7,
                                 ring_n = NULL,
                                 r = 1,
                                 center = TRUE,
                                 idx = NULL,
                                 shrink = 1,
                                 k_nn = 12,
                                 push_others_delta = 0,
                                 layout.module = c("random", "adjacent", "order"),
                                 group.by = "Modularity",
                                 fill.by = "Modularity",
                                 fill = NULL,
                                 color = NULL,
                                 pointsize = c(1, 5),
                                 jitter = FALSE,
                                 jitter_sd = 0.01,
                                 mapping_line = FALSE,
                                 linealpha = 0.25,
                                 linecolor = "grey70",
                                 inner_curve = FALSE,
                                 inner_curvature = 0.12,
                                 inner_curve_adaptive = TRUE,
                                 inner_curve_adaptive_range = c(0.7, 1.3),
                                 inner_curve_adaptive_bins = 7,
                                 add_outer = "circle",
                                 q_outer = 0.88,
                                 expand_outer = 1.02,
                                 outerwidth = 1.25,
                                 outerlinetype = 2,
                                 outeralpha = 0.5,
                                 link_level = "Module",
                                 link_curve = FALSE,
                                 link_curvature = 0.2,
                                 link_curve_mode = "outward",
                                 link_curve_adaptive = TRUE,
                                 link_curve_adaptive_range = c(0.7, 1.3),
                                 link_curve_adaptive_bins = 7,
                                 link_linetype_node = 2,
                                 link_linetype_module = 1,
                                 link_linealpha_node = 0.25,
                                 link_linealpha_module = 0.5,
                                 dropOthers = FALSE,
                                 calculate_topology = FALSE,
                                 scale_groups = TRUE,
                                 orientation = "up",
                                 angle = 0,
                                 anchor_dist = 6,
                                 layout_anchor_dist = NULL,
                                 nrow = NULL,
                                 ncol = NULL,
                                 seed = 1115
){
  # 参数

  color_v <- c('#1f78b4', '#e7298a', '#41ab5d', '#807dba',
               '#fb9a99', '#4eb3d3', '#bc80bd', '#e31a1c',
               '#a6cee3', '#b2df8a', '#33a02c', '#fdbf6f',
               '#ff7f00', '#cab2d6', '#6a3d9a', '#ffff99',
               '#8dd3c7', '#ffffb3', '#bebada', '#fb8072',
               '#80b1d3', '#fdb462', '#b3de69', '#fccde5',
               '#d9d9d9', '#bc80bd', '#ccebc5', '#ffed6f'
               )

  if (is.logical(mapping_line)) {
    if (length(mapping_line) != 1 || is.na(mapping_line)) {
      stop("`mapping_line` must be a single logical or character string.")
    }
  } else if (is.character(mapping_line)) {
    if (length(mapping_line) != 1 || is.na(mapping_line) || trimws(mapping_line) == "") {
      stop("`mapping_line` must be a single logical or character string.")
    }
  } else {
    stop("`mapping_line` must be a single logical or character string.")
  }
  if (!is.logical(calculate_topology) || length(calculate_topology) != 1 || is.na(calculate_topology)) {
    stop("`calculate_topology` must be TRUE or FALSE.")
  }
  if (!is.data.frame(group_info) || !all(c("Sample", "Group") %in% colnames(group_info))) {
    stop("`group_info` must be a data.frame containing columns `Sample` and `Group`.")
  }
  if (!is.null(fill) && !is.character(fill)) {
    stop("`fill` must be NULL or a character vector (preferably named).")
  }
  if (!is.null(color) && !is.character(color)) {
    stop("`color` must be NULL, a single color string, or a named character vector.")
  }
  if (is.logical(add_outer)) {
    if (length(add_outer) != 1 || is.na(add_outer)) {
      stop("`add_outer` must be a single logical or character string.")
    }
    add_outer <- if (isTRUE(add_outer)) "circle" else "none"
  } else if (is.character(add_outer)) {
    if (length(add_outer) != 1 || is.na(add_outer) || trimws(add_outer) == "") {
      stop("`add_outer` must be a single logical or character string.")
    }
    add_outer <- tolower(trimws(add_outer))
    if (!add_outer %in% c("circle", "manual", "none")) {
      stop("`add_outer` must be one of: 'circle', 'manual', 'none'.")
    }
  } else {
    stop("`add_outer` must be a single logical or character string.")
  }
  if (!is.character(link_level) || length(link_level) != 1 || is.na(link_level)) {
    stop("`link_level` must be one of: 'None', 'Module', 'Node', 'NodeinModule', 'Module&Node'.")
  }
  link_level <- tolower(trimws(link_level))
  if (!link_level %in% c("none", "module", "node", "nodeinmodule", "module&node")) {
    stop("`link_level` must be one of: 'None', 'Module', 'Node', 'NodeinModule', 'Module&Node'.")
  }
  if (!is.logical(link_curve) || length(link_curve) != 1 || is.na(link_curve)) {
    stop("`link_curve` must be TRUE or FALSE.")
  }
  if (!is.numeric(link_curvature) || length(link_curvature) != 1 || is.na(link_curvature)) {
    stop("`link_curvature` must be a single numeric value.")
  }
  if (!is.character(link_curve_mode) || length(link_curve_mode) != 1 || is.na(link_curve_mode)) {
    stop("`link_curve_mode` must be one of: 'outward', 'inward', 'cross'.")
  }
  link_curve_mode <- tolower(trimws(link_curve_mode))
  if (!link_curve_mode %in% c("outward", "inward", "cross")) {
    stop("`link_curve_mode` must be one of: 'outward', 'inward', 'cross'.")
  }
  if (!is.logical(link_curve_adaptive) || length(link_curve_adaptive) != 1 || is.na(link_curve_adaptive)) {
    stop("`link_curve_adaptive` must be TRUE or FALSE.")
  }
  if (!is.numeric(link_curve_adaptive_range) || length(link_curve_adaptive_range) != 2 ||
      any(is.na(link_curve_adaptive_range))) {
    stop("`link_curve_adaptive_range` must be a numeric vector of length 2.")
  }
  if (link_curve_adaptive_range[1] <= 0 || link_curve_adaptive_range[2] <= 0 ||
      link_curve_adaptive_range[1] > link_curve_adaptive_range[2]) {
    stop("`link_curve_adaptive_range` must be positive and increasing, e.g. c(0.7, 1.3).")
  }
  if (!is.numeric(link_curve_adaptive_bins) || length(link_curve_adaptive_bins) != 1 || is.na(link_curve_adaptive_bins)) {
    stop("`link_curve_adaptive_bins` must be a single integer >= 1.")
  }
  link_curve_adaptive_bins <- as.integer(link_curve_adaptive_bins)
  if (link_curve_adaptive_bins < 1) {
    stop("`link_curve_adaptive_bins` must be >= 1.")
  }
  if (!is.logical(inner_curve) || length(inner_curve) != 1 || is.na(inner_curve)) {
    stop("`inner_curve` must be TRUE or FALSE.")
  }
  if (!is.numeric(inner_curvature) || length(inner_curvature) != 1 || is.na(inner_curvature)) {
    stop("`inner_curvature` must be a single numeric value.")
  }
  if (!is.logical(inner_curve_adaptive) || length(inner_curve_adaptive) != 1 || is.na(inner_curve_adaptive)) {
    stop("`inner_curve_adaptive` must be TRUE or FALSE.")
  }
  if (!is.numeric(inner_curve_adaptive_range) || length(inner_curve_adaptive_range) != 2 ||
      any(is.na(inner_curve_adaptive_range))) {
    stop("`inner_curve_adaptive_range` must be a numeric vector of length 2.")
  }
  if (inner_curve_adaptive_range[1] <= 0 || inner_curve_adaptive_range[2] <= 0 ||
      inner_curve_adaptive_range[1] > inner_curve_adaptive_range[2]) {
    stop("`inner_curve_adaptive_range` must be positive and increasing, e.g. c(0.7, 1.3).")
  }
  if (!is.numeric(inner_curve_adaptive_bins) || length(inner_curve_adaptive_bins) != 1 || is.na(inner_curve_adaptive_bins)) {
    stop("`inner_curve_adaptive_bins` must be a single integer >= 1.")
  }
  inner_curve_adaptive_bins <- as.integer(inner_curve_adaptive_bins)
  if (inner_curve_adaptive_bins < 1) {
    stop("`inner_curve_adaptive_bins` must be >= 1.")
  }
  if (!is.numeric(anchor_dist) || length(anchor_dist) != 1 || is.na(anchor_dist)) {
    stop("`anchor_dist` must be a single numeric value.")
  }
  if (!is.null(layout_anchor_dist) &&
      (!is.numeric(layout_anchor_dist) || length(layout_anchor_dist) != 1 || is.na(layout_anchor_dist))) {
    stop("`layout_anchor_dist` must be NULL or a single numeric value.")
  }
  layout.module <- match.arg(layout.module)
  layout_anchor_dist_use <- if (is.null(layout_anchor_dist)) anchor_dist else layout_anchor_dist

  graph_list <- list()

  graph_info <- list()

  graph_stat <- list()
  topology_network <- list()
  topology_sample <- list()

  for (g in unique(group_info$Group)) {
    group_info_sub <- group_info %>%
      dplyr::filter(Group %in% g)

    mat_sub <- mat %>%
      as.data.frame() %>%
      dplyr::select(all_of(group_info_sub$Sample)) %>%
      tibble::rownames_to_column(var = "ID") %>%
      dplyr::rowwise() %>%
      dplyr::mutate(sum = sum(dplyr::c_across(where(is.numeric)))) %>%
      dplyr::ungroup() %>%
      dplyr::filter(sum != 0) %>%
      dplyr::select(-sum) %>%
      tibble::column_to_rownames(var = "ID")

    graph <- build_graph_from_mat(
      mat = mat_sub,
      transfrom.method = transfrom.method,
      r.threshold = r.threshold,
      p.threshold = p.threshold,
      method = method,
      cor.method = cor.method,
      proc = proc,
      module.method = module.method,
      SpiecEasi.method = SpiecEasi.method,
      node_annotation = node_annotation,
      top_modules = top_modules,
      seed = seed
    )

    # dropOthers acts on the source graph_obj BEFORE layout:
    # it removes "Others" nodes first, then downstream layout/plot are rebuilt.
    if (isTRUE(dropOthers)) {
      node_tbl <- graph %>%
        tidygraph::activate(nodes) %>%
        tidygraph::as_tibble()

      module_candidates <- c("Modularity", "modularity3", "modularity2")
      module_col <- module_candidates[module_candidates %in% colnames(node_tbl)]
      module_col <- if (length(module_col) > 0) module_col[[1]] else NULL

      if (!is.null(module_col)) {
        if ("name" %in% colnames(node_tbl)) {
          keep_names <- node_tbl %>%
            dplyr::filter(as.character(.data[[module_col]]) != "Others") %>%
            dplyr::pull(name)

          graph <- graph %>%
            tidygraph::activate(nodes) %>%
            tidygraph::filter(name %in% keep_names)
        } else {
          graph <- graph %>%
            tidygraph::activate(nodes) %>%
            tidygraph::filter(as.character(.data[[module_col]]) != "Others")
        }
      } else {
        warning("`dropOthers = TRUE` but no module column found in `graph_obj` nodes.")
      }
    }

    # 添加布局
    # find layout function
    func_name <- paste0("create_layout_", layout)

    # find layout functions from ggNetView package
    lay_func <- utils::getFromNamespace(func_name, "ggNetView")

    # get ly1 (auto-pass only arguments supported by the chosen layout function)
    lay_args <- list(
      graph_obj = graph,
      node_add = node_add,
      r = r,
      scale = scale_groups,
      anchor_dist = layout_anchor_dist_use,
      orientation = orientation,
      angle = angle,
      nrow = nrow,
      ncol = ncol
    )
    lay_args <- lay_args[names(lay_args) %in% names(formals(lay_func))]
    ly1 <- do.call(lay_func, lay_args)

    # get ly1_1
    # 圆形布局 添加模块化 获取模块
    if (layout.module == "random") {
      ly1_1 <- module_layout(graph,
                             layout = ly1,
                             center = center,
                             idx = idx,
                             shrink = shrink,
                             jitter,
                             jitter_sd# ,
                             # seed = seed
      )
    }

    if (layout.module == "adjacent") {
      k_nn_try <- k_nn
      k_nn_cap <- max(1, nrow(ly1) - 1)
      ly1_1 <- NULL
      while (is.null(ly1_1)) {
        ly_try <- tryCatch(
          module_layout3(graph,
                         layout = ly1,
                         center = center,
                         k_nn = k_nn_try,
                         push_others_delta = push_others_delta,
                         shrink = shrink,
                         jitter = jitter,
                         jitter_sd = jitter_sd
                         # seed = seed
          ),
          error = function(e) e
        )

        if (!inherits(ly_try, "error")) {
          ly1_1 <- ly_try
          break
        }

        err_msg <- conditionMessage(ly_try)
        is_slot_error <- grepl("连续 slot|consecutive slots", err_msg, ignore.case = TRUE)
        if (!is_slot_error || k_nn_try >= k_nn_cap) {
          stop(ly_try)
        }

        next_k <- min(k_nn_cap, max(k_nn_try + 20, ceiling(k_nn_try * 1.25)))
        if (next_k <= k_nn_try) {
          stop(ly_try)
        }
        warning(sprintf(
          "`layout.module = 'adjacent'` failed at k_nn = %d; retrying with k_nn = %d.",
          k_nn_try, next_k
        ))
        k_nn_try <- next_k
      }
    }

    if (layout.module == "order" & func_name != "create_layout_rings") {
      ly1_1 <- module_layout4(graph,
                              layout = ly1,
                              center = center,
                              k_nn = k_nn,
                              push_others_delta = push_others_delta,
                              shrink = shrink,
                              jitter,
                              jitter_sd
                              # seed = seed
      )
    }

    if (layout.module == "order" & func_name == "create_layout_multirings") {
      ly1_1 <- module_layout5(graph,
                              layout = ly1,
                              center = center,
                              k_nn = k_nn,
                              push_others_delta = push_others_delta,
                              shrink = shrink,
                              jitter,
                              jitter_sd
                              # seed = seed
      )
    }

    graph_list[[g]] <- graph

    graph_info[[g]] <- ly1_1$ggplot_data

    graph_stat[[g]] <- stat_graph(graph, mapping_line)

    if (isTRUE(calculate_topology)) {
      topology_network[[g]] <- tryCatch(
        get_network_topology_parallel(
          graph_obj = graph,
          mat = mat_sub,
          transfrom.method = transfrom.method,
          r.threshold = r.threshold,
          p.threshold = p.threshold,
          method = method,
          cor.method = cor.method,
          proc = proc,
          seed = seed
        ),
        error = function(e) {
          warning(paste0("Topology (network) failed for group `", g, "`: ", e$message), call. = FALSE)
          NULL
        }
      )

      topology_sample[[g]] <- tryCatch(
        get_sample_subgraph_topology_parallel(
          graph_obj = graph,
          mat = mat_sub,
          transfrom.method = transfrom.method,
          r.threshold = r.threshold,
          p.threshold = p.threshold,
          method = method,
          cor.method = cor.method,
          proc = proc,
          seed = seed
        ),
        error = function(e) {
          warning(paste0("Topology (sample subgraph) failed for group `", g, "`: ", e$message), call. = FALSE)
          NULL
        }
      )
    }
  }

  graph_list_length <- length(graph_list)
  # 获区比较的分组
  if (graph_list_length >= 2) {
    compare_matrix <- utils::combn(names(graph_list), 2)
  } else {
    compare_matrix <- matrix(character(0), nrow = 2, ncol = 0)
  }

  if (link_level %in% c("module", "nodeinmodule", "module&node") && ncol(compare_matrix) > 0) {
    compare_out_list <- list()
    for (i in seq_len(ncol(compare_matrix))) {
      tmp <- compare_modules_by_overlap(graph_info[[compare_matrix[1, i]]]$ggplot_node_df %>%
                                          dplyr::select(name, Modularity) %>%
                                          dplyr::mutate(Group = compare_matrix[1, i]),
                                        graph_info[[compare_matrix[2, i]]]$ggplot_node_df %>%
                                          dplyr::select(name, Modularity) %>%
                                          dplyr::mutate(Group = compare_matrix[2, i])) %>%
        dplyr::mutate(Group = stringr::str_c(compare_matrix[1, i],
                                             "to",
                                             compare_matrix[2, i],
                                             sep = "_"))

      compare_out_list[[stringr::str_c(compare_matrix[1, i],
                                       "to",
                                       compare_matrix[2, i],
                                       sep = "_")]] <- tmp
    }

    Module_information <- do.call(rbind, compare_out_list) %>%
      dplyr::filter(pvalue < 0.05) %>%
      tidyr::separate(col = Group, sep = "_", into = c("GroupA", "to", "GroupB"), remove = F) %>%
      dplyr::select(-to)
  } else {
    Module_information <- tibble::tibble(
      modA = character(),
      modB = character(),
      overlap = integer(),
      sizeA = integer(),
      sizeB = integer(),
      overlap_coef = numeric(),
      pvalue = numeric(),
      FDR = numeric(),
      Group = character(),
      GroupA = character(),
      GroupB = character()
    )
  }

  Module_information_plot <- if (link_level %in% c("module", "nodeinmodule", "module&node")) {
    if (isTRUE(dropOthers)) {
      Module_information
    } else {
      Module_information %>%
        dplyr::filter(modA != "Others" | modB != "Others")
    }
  } else {
    Module_information
  }

  Module_information

  # if (isTRUE(scale_groups)) {
  #   anchor_dist = 1
  # }else{
  #   anchor_dist = anchor_dist * 30
  # }

  # 寻找点
  angles <- pi/2 - 2 * pi * (0:(graph_list_length - 1)) / graph_list_length
  anchors <- lapply(angles, function(a) {
    c(anchor_dist * cos(a), anchor_dist * sin(a))
  })

  anchors_df <- do.call(rbind, anchors) %>%
    as.data.frame() %>%
    purrr::set_names(c("x", "y"))


  # 是否是等齐的, 同时进行位置调整
  if (isTRUE(scale_groups)) {
    # 如果需要做标准化的话, 把坐标都要归一化一下
    for (i in seq_along(names(graph_info))) {
      graph_info[[i]]$ggplot_node_df <- graph_info[[i]]$ggplot_node_df %>%
        dplyr::mutate(Group = names(graph_info)[i]) %>%
        # dplyr::select(name, x, y, Group) %>%
        dplyr::mutate(
          ymin = min(y),
          ymax = max(y),
          xmin = min(x),
          xmax = max(x),
          xmind = (max(x) + min(x)) / 2,
          ymind = (max(y) + min(y)) / 2,
          scale_v = max(xmax - xmin, ymax - ymin),
          x = (x - xmind)/scale_v,
          y = (y - ymind)/scale_v
        ) %>%
        dplyr::mutate(
          x = x + anchors_df[i,1],
          y = y + anchors_df[i,2]
        )

      graph_info[[i]]$ggplot_edge_df <- graph_info[[i]]$ggplot_edge_df %>%
        dplyr::mutate(xmid = unique(graph_info[[i]]$ggplot_node_df$xmind),
                      ymid = unique(graph_info[[i]]$ggplot_node_df$ymind),
                      scale_v = unique(graph_info[[i]]$ggplot_node_df$scale_v),
                      from_x = (from_x - xmid)/scale_v,
                      from_y = (from_y - ymid)/scale_v,
                      to_x = (to_x - xmid)/scale_v,
                      to_y = (to_y - ymid)/scale_v) %>%
        dplyr::mutate(
          from_x = from_x + anchors_df[i,1],
          from_y = from_y + anchors_df[i,2],
          to_x = to_x + anchors_df[i,1],
          to_y = to_y + anchors_df[i,2]
        )

    }
    # jitter TRUE
    if (isTRUE(jitter)) {
      for (i in seq_along(names(graph_info))) {
        graph_info[[i]]$ggplot_node_df <- graph_info[[i]]$ggplot_node_df %>%
          dplyr::mutate(
            x = x + stats::rnorm(dplyr::n(), mean = 0, sd = jitter_sd),
            y = y + stats::rnorm(dplyr::n(), mean = 0, sd = jitter_sd)
          )

        graph_info[[i]]$ggplot_edge_df <- graph_info[[i]]$ggplot_edge_df %>%
          dplyr::select(from, to, weight, correlation, corr_direction, from_id, to_id) %>%
          dplyr::left_join(graph_info[[i]]$ggplot_node_df %>%
                             dplyr::select(name, x, y),
                           by = c("from_id" = "name")) %>%
          dplyr::rename(from_x = x,
                        from_y = y) %>%
          dplyr::left_join(graph_info[[i]]$ggplot_node_df %>%
                             dplyr::select(name, x, y),
                           by = c("to_id" = "name")) %>%
          dplyr::rename(to_x = x,
                        to_y = y)
      }
    }
  }

  if (!isTRUE(scale_groups)) {
    for (i in seq_along(names(graph_info))) {
      graph_info[[i]]$ggplot_node_df <- graph_info[[i]]$ggplot_node_df %>%
        dplyr::mutate(
          x = x + anchors_df[i,1],
          y = y + anchors_df[i,2]
        )

      graph_info[[i]]$ggplot_edge_df <- graph_info[[i]]$ggplot_edge_df %>%
        dplyr::select(from, to, weight, correlation, corr_direction, from_id, to_id) %>%
        dplyr::left_join(graph_info[[i]]$ggplot_node_df %>%
                           dplyr::select(name, x, y),
                         by = c("from_id" = "name")) %>%
        dplyr::rename(from_x = x,
                      from_y = y) %>%
        dplyr::left_join(graph_info[[i]]$ggplot_node_df %>%
                           dplyr::select(name, x, y),
                         by = c("to_id" = "name")) %>%
        dplyr::rename(to_x = x,
                      to_y = y)

    # jitter TRUE
    if (isTRUE(jitter)) {
      for (i in seq_along(names(graph_info))) {
        graph_info[[i]]$ggplot_node_df <- graph_info[[i]]$ggplot_node_df %>%
          dplyr::mutate(
            x = x + stats::rnorm(dplyr::n(), mean = 0, sd = jitter_sd),
            y = y + stats::rnorm(dplyr::n(), mean = 0, sd = jitter_sd)
          )

        graph_info[[i]]$ggplot_edge_df <- graph_info[[i]]$ggplot_edge_df %>%
          dplyr::select(from, to, weight, correlation, corr_direction, from_id, to_id) %>%
          dplyr::left_join(graph_info[[i]]$ggplot_node_df %>%
                             dplyr::select(name, x, y),
                           by = c("from_id" = "name")) %>%
          dplyr::rename(from_x = x,
                        from_y = y) %>%
          dplyr::left_join(graph_info[[i]]$ggplot_node_df %>%
                             dplyr::select(name, x, y),
                           by = c("to_id" = "name")) %>%
          dplyr::rename(to_x = x,
                        to_y = y)
       }
     }
    }
  }




  # plot data
  # 那么可以直接进行可视化了
  # raw data to plot data

  p <- ggplot()

  add_inner_link_geom <- function(edge_data, mapping_obj, color_fixed = NULL) {
    if (!isTRUE(inner_curve)) {
      return(
        ggplot2::geom_segment(
          data = edge_data,
          mapping = mapping_obj,
          alpha = linealpha,
          color = color_fixed
        )
      )
    }

    if (!isTRUE(inner_curve_adaptive) || nrow(edge_data) < 2) {
      return(
        ggplot2::geom_curve(
          data = edge_data,
          mapping = mapping_obj,
          curvature = inner_curvature,
          alpha = linealpha,
          color = color_fixed
        )
      )
    }

    edge_dist <- sqrt((edge_data$to_x - edge_data$from_x)^2 + (edge_data$to_y - edge_data$from_y)^2)
    if (all(!is.finite(edge_dist)) || diff(range(edge_dist, na.rm = TRUE)) <= .Machine$double.eps) {
      return(
        ggplot2::geom_curve(
          data = edge_data,
          mapping = mapping_obj,
          curvature = inner_curvature,
          alpha = linealpha,
          color = color_fixed
        )
      )
    }

    n_bins <- min(inner_curve_adaptive_bins, length(edge_dist))
    breaks <- unique(stats::quantile(edge_dist, probs = seq(0, 1, length.out = n_bins + 1), na.rm = TRUE))
    if (length(breaks) <= 2) {
      return(
        ggplot2::geom_curve(
          data = edge_data,
          mapping = mapping_obj,
          curvature = inner_curvature,
          alpha = linealpha,
          color = color_fixed
        )
      )
    }

    edge_data <- edge_data %>%
      dplyr::mutate(.edge_dist = edge_dist,
                    .curve_bin = as.integer(cut(.edge_dist, breaks = breaks, include.lowest = TRUE)))

    bins_present <- sort(unique(stats::na.omit(edge_data$.curve_bin)))
    if (length(bins_present) == 0) {
      return(
        ggplot2::geom_curve(
          data = edge_data,
          mapping = mapping_obj,
          curvature = inner_curvature,
          alpha = linealpha,
          color = color_fixed
        )
      )
    }

    bin_mult <- stats::setNames(
      seq(inner_curve_adaptive_range[1], inner_curve_adaptive_range[2], length.out = length(bins_present)),
      bins_present
    )

    layers <- lapply(bins_present, function(b) {
      ggplot2::geom_curve(
        data = edge_data %>% dplyr::filter(.curve_bin == b),
        mapping = mapping_obj,
        curvature = inner_curvature * bin_mult[as.character(b)],
        alpha = linealpha,
        color = color_fixed
      )
    })

    return(layers)
  }

  # 我们先添加点和线
  for (index in seq_along(names(graph_info))) {
    edge_df <- graph_info[[index]]$ggplot_edge_df

    # plot link
    if (isFALSE(mapping_line)) {
      p <- p + add_inner_link_geom(
        edge_data = edge_df,
        mapping_obj = ggplot2::aes(x = from_x, xend = to_x, y = from_y, yend = to_y),
        color_fixed = linecolor
      )
    } else if (isTRUE(mapping_line)) {
      if (!"corr_direction" %in% colnames(edge_df)) {
        stop("`mapping_line = TRUE` requires `corr_direction` in edge data.")
      }
      p <- p +
        ggnewscale::new_scale_color() +
        add_inner_link_geom(
          edge_data = edge_df,
          mapping_obj = ggplot2::aes(x = from_x, xend = to_x, y = from_y, yend = to_y, colour = corr_direction)
        ) +
        ggplot2::scale_color_manual(values = c("Positive" = "#d6604d", "Negative" = "#4393c3"))
    } else {
      if (!mapping_line %in% colnames(edge_df)) {
        stop("`mapping_line` must be a variable name in edge data.")
      }
      line_values <- edge_df[[mapping_line]]
      line_scale <- if (is.numeric(line_values)) {
        ggplot2::scale_color_gradient(low = "#4393c3", high = "#d6604d")
      } else {
        scale_color_ggnetview(unique(as.character(line_values)))
      }

      p <- p +
        ggnewscale::new_scale_color() +
        add_inner_link_geom(
          edge_data = edge_df,
          mapping_obj = ggplot2::aes(x = from_x, xend = to_x, y = from_y, yend = to_y, colour = .data[[mapping_line]])
        ) +
        line_scale
    }

    module_targets <- character(0)
    if (link_level %in% c("module", "nodeinmodule", "module&node") && nrow(Module_information_plot) > 0) {
      module_targets <- Module_information_plot %>%
        dplyr::filter(stringr::str_detect(Group, pattern = names(graph_list)[index])) %>%
        dplyr::filter(GroupA == names(graph_list)[index] | GroupB == names(graph_list)[index]) %>%
        dplyr::mutate(mod_target = dplyr::case_when(
          GroupA == names(graph_list)[index] ~ modA,
          GroupB == names(graph_list)[index] ~ modB,
          .default = NA
        )) %>%
        dplyr::pull(mod_target) %>%
        unique()

      if (!isTRUE(dropOthers)) {
        module_targets <- module_targets[module_targets != "Others"]
      }
    } else {
      # For link_level = "none"/"node" (or when no module match exists),
      # outer boundaries should still be visible for the current group's modules.
      module_targets <- graph_info[[index]]$ggplot_node_df %>%
        dplyr::mutate(Modularity = as.character(Modularity)) %>%
        dplyr::pull(Modularity) %>%
        unique()
      module_targets <- module_targets[module_targets != "Others"]
    }

    outer_node_df <- graph_info[[index]]$ggplot_node_df %>%
      dplyr::filter(name %in% (graph_list[[index]] %>%
                                 tidygraph::activate(nodes) %>%
                                 tidygraph::as_tibble() %>%
                                 dplyr::mutate(Modularity = as.character(Modularity)) %>%
                                 dplyr::filter(Modularity %in% module_targets) %>%
                                 dplyr::pull(name)))

    module_levels_group <- graph_info[[index]]$ggplot_node_df %>%
      dplyr::mutate(Modularity = as.character(Modularity)) %>%
      dplyr::pull(Modularity) %>%
      unique()

    fill_scale_group <- if (is.null(fill)) {
      ggplot2::scale_fill_viridis_d(drop = FALSE, limits = module_levels_group)
    } else {
      ggplot2::scale_fill_manual(values = fill, drop = FALSE)
    }

    color_is_fixed <- !is.null(color) && length(color) == 1 && (is.null(names(color)) || names(color)[1] == "")
    color_scale_group <- if (is.null(color)) {
      ggplot2::scale_color_viridis_d(drop = FALSE, limits = module_levels_group)
    } else if (isTRUE(color_is_fixed)) {
      NULL
    } else {
      ggplot2::scale_color_manual(values = color, drop = FALSE)
    }

    if (is.null(color)) {
      p <- p +
        ggnewscale::new_scale_fill() +
        ggplot2::geom_point(data = graph_info[[index]]$ggplot_node_df,
                            mapping = ggplot2::aes(x = x,
                                                   y = y,
                                                   fill = Modularity,
                                                   size = Degree),
                            shape = 21) +
        fill_scale_group +
        ggnewscale::new_scale_fill()
    } else if (isTRUE(color_is_fixed)) {
      p <- p +
        ggnewscale::new_scale_fill() +
        ggplot2::geom_point(data = graph_info[[index]]$ggplot_node_df,
                            mapping = ggplot2::aes(x = x,
                                                   y = y,
                                                   fill = Modularity,
                                                   size = Degree),
                            shape = 21,
                            color = color) +
        fill_scale_group +
        ggnewscale::new_scale_fill()
    } else {
      p <- p +
        ggnewscale::new_scale_color() +
        ggnewscale::new_scale_fill() +
        ggplot2::geom_point(data = graph_info[[index]]$ggplot_node_df,
                            mapping = ggplot2::aes(x = x,
                                                   y = y,
                                                   fill = Modularity,
                                                   color = Modularity,
                                                   size = Degree),
                            shape = 21) +
        fill_scale_group +
        color_scale_group +
        ggnewscale::new_scale_fill()
    }

    p <- p + ggplot2::scale_size(range = pointsize, guide = "none")

    if (add_outer == "circle" && nrow(outer_node_df) > 0) {
      circle_n <- max(40, min(300, as.integer(round(8 * sqrt(nrow(outer_node_df))))))
      p <- p +
        ggnewscale::new_scale_color() +
        {
          if (isTRUE(color_is_fixed)) {
            ggforce::geom_mark_circle(
              data = outer_node_df,
              mapping = ggplot2::aes(x = x, y = y, fill = Modularity),
              color = color,
              n = circle_n,
              expand = grid::unit(1, "mm")
            )
          } else {
            ggforce::geom_mark_circle(
              data = outer_node_df,
              mapping = ggplot2::aes(x = x, y = y, fill = Modularity, color = Modularity),
              n = circle_n,
              expand = grid::unit(1, "mm")
            )
          }
        } +
        fill_scale_group +
        {
          if (isTRUE(color_is_fixed)) ggplot2::guides(color = "none") else color_scale_group
        }
    }

    if (add_outer == "manual" && nrow(outer_node_df) > 2) {
      maskTable <- generateMask_ggnetview(
        dims = outer_node_df %>% dplyr::select(x, y),
        clusters = outer_node_df %>% dplyr::pull(Modularity),
        q = q_outer,
        expand = expand_outer
      ) %>%
        dplyr::mutate(cluster = as.character(cluster))
      p <- p +
        ggnewscale::new_scale_color() +
        {
          if (isTRUE(color_is_fixed)) {
            ggplot2::geom_polygon(
              data = maskTable,
              mapping = ggplot2::aes(x = x, y = y, group = cluster, fill = cluster),
              color = color,
              linewidth = outerwidth,
              linetype = outerlinetype,
              alpha = outeralpha,
              show.legend = FALSE
            )
          } else {
            ggplot2::geom_polygon(
              data = maskTable,
              mapping = ggplot2::aes(x = x, y = y, group = cluster, fill = cluster, color = cluster),
              linewidth = outerwidth,
              linetype = outerlinetype,
              alpha = outeralpha,
              show.legend = FALSE
            )
          }
        } +
        fill_scale_group +
        {
          if (isTRUE(color_is_fixed)) ggplot2::guides(color = "none") else color_scale_group
        }
    }

    p <- p +
      ggplot2::annotate(
        geom = "text",
        x = mean(graph_info[[index]]$ggplot_node_df$x),
        y = max(graph_info[[index]]$ggplot_node_df$y) + anchor_dist/30,
        label = if (isTRUE(mapping_line)) {
          paste0("Group = ", names(graph_info[index]), "\n",
                 "Node = ", graph_stat[[index]]$node, "\n",
                 "Edge = ", graph_stat[[index]]$edge, "\n",
                 "Positive = ", graph_stat[[index]]$position_edge, "\n",
                 "Negative = ", graph_stat[[index]]$negative_edge, "\n")
        } else {
          paste0("Group = ", names(graph_info[index]), "\n",
                 "Node = ", graph_stat[[index]]$node, "\n",
                 "Edge = ", graph_stat[[index]]$edge, "\n")
        },
        size = 4,
        fontface = "bold"
      )

  }

  # 增加跨组连线
  link_info <- tibble::tibble(
    link_level = character(),
    group_a = character(),
    group_b = character(),
    source = character(),
    target = character(),
    x = numeric(),
    y = numeric(),
    xend = numeric(),
    yend = numeric()
  )

  # When link_curve_mode == "cross": assign one curvature sign per group-pair so
  # same-pair links form a bundle along a channel (left/right/bottom arc), not through center.
  # Cross uses the OPPOSITE of the inward rule per pair so that cross = outer arc vs inward = inner arc (they then look distinct).
  cross_channel_lookup <- list()
  circle_centers_x <- circle_centers_y <- circle_radius <- NULL
  circle_pad <- 0
  module_circle_tbl <- NULL
  module_circle_pad <- 0
  if (identical(link_curve_mode, "cross")) {
    gnames <- names(graph_info)
    ngrp <- length(gnames)
    cx <- vapply(gnames, function(g) mean(graph_info[[g]]$ggplot_node_df$x), numeric(1))
    cy <- vapply(gnames, function(g) mean(graph_info[[g]]$ggplot_node_df$y), numeric(1))
    names(cx) <- gnames
    names(cy) <- gnames
    circle_centers_x <- cx
    circle_centers_y <- cy
    # Forbidden region should cover the visible group orbit, not only the center hole.
    circle_radius <- vapply(
      gnames,
      function(g) {
        node_df <- graph_info[[g]]$ggplot_node_df
        d <- sqrt((node_df$x - cx[g])^2 + (node_df$y - cy[g])^2)
        q_outer <- as.numeric(stats::quantile(d, probs = 0.98, na.rm = TRUE))
        r_max <- max(d, na.rm = TRUE)
        max(max(q_outer, r_max * 0.98), 1e-6)
      },
      numeric(1)
    )
    circle_pad <- max(stats::median(circle_radius, na.rm = TRUE) * 0.06, 1e-6)
    # Also forbid entering module circles (small circles in each group).
    module_rows <- list()
    rid <- 1L
    for (g in gnames) {
      node_df <- graph_info[[g]]$ggplot_node_df
      if (!"Modularity" %in% colnames(node_df)) next
      mods <- unique(as.character(node_df$Modularity))
      mods <- mods[!is.na(mods)]
      for (m in mods) {
        md <- node_df[as.character(node_df$Modularity) == m, , drop = FALSE]
        if (nrow(md) == 0) next
        mcx <- mean(md$x)
        mcy <- mean(md$y)
        mr <- max(sqrt((md$x - mcx)^2 + (md$y - mcy)^2))
        if (!is.finite(mr) || mr <= 0) next
        module_rows[[rid]] <- data.frame(
          group = g,
          module = m,
          key = paste0(g, "|", m),
          cx = mcx,
          cy = mcy,
          r = mr,
          stringsAsFactors = FALSE
        )
        rid <- rid + 1L
      }
    }
    if (length(module_rows) > 0) {
      module_circle_tbl <- dplyr::bind_rows(module_rows)
      module_circle_pad <- max(stats::median(module_circle_tbl$r, na.rm = TRUE) * 0.10, 1e-6)
    }
    top_g <- left_g <- right_g <- NULL
    if (ngrp >= 2L) {
      if (ngrp == 3L) {
        top_ix <- which.max(cy)
        bottom_ix <- setdiff(seq_len(3L), top_ix)
        left_ix <- bottom_ix[which.min(cx[bottom_ix])]
        right_ix <- setdiff(bottom_ix, left_ix)
        top_g <- gnames[top_ix]
        left_g <- gnames[left_ix]
        right_g <- gnames[right_ix]
        # Use opposite of inward so cross = outer arc (bend away from center), inward = inner arc.
        outward_tl <- (cx[top_g] + cx[left_g])/2 * (cy[top_g] - cy[left_g]) + (cy[top_g] + cy[left_g])/2 * (cx[left_g] - cx[top_g])
        cross_channel_lookup[[paste(sort(c(top_g, left_g)), collapse = "|")]] <- as.integer(if (outward_tl != 0) -sign(outward_tl) else -1L)
        outward_tr <- (cx[top_g] + cx[right_g])/2 * (cy[top_g] - cy[right_g]) + (cy[top_g] + cy[right_g])/2 * (cx[right_g] - cx[top_g])
        cross_channel_lookup[[paste(sort(c(top_g, right_g)), collapse = "|")]] <- as.integer(if (outward_tr != 0) -sign(outward_tr) else 1L)
        outward_lr <- (cx[left_g] + cx[right_g])/2 * (cy[left_g] - cy[right_g]) + (cy[left_g] + cy[right_g])/2 * (cx[right_g] - cx[left_g])
        cross_channel_lookup[[paste(sort(c(left_g, right_g)), collapse = "|")]] <- as.integer(if (outward_lr != 0) -sign(outward_lr) else 1L)
      } else {
        cx_cent <- mean(cx)
        cy_cent <- mean(cy)
        for (i in seq_len(ngrp - 1L)) {
          for (j in (i + 1L):ngrp) {
            gA <- gnames[i]
            gB <- gnames[j]
            dx <- cx[gB] - cx[gA]
            dy <- cy[gB] - cy[gA]
            mx <- (cx[gA] + cx[gB]) / 2
            my <- (cy[gA] + cy[gB]) / 2
            s <- as.integer(sign((cx_cent - mx) * dy - (cy_cent - my) * dx))
            key_ij <- paste(sort(c(gA, gB)), collapse = "|")
            cross_channel_lookup[[key_ij]] <- if (s != 0L) s else 1L
          }
        }
      }
    }
  }

  add_link_layer <- function(p_obj, df_link, col_link, link_type = c("node", "module"), cross_channel_sign = NULL) {
    link_type <- match.arg(link_type)
    link_lt <- if (link_type == "module") link_linetype_module else link_linetype_node
    link_al <- if (link_type == "module") link_linealpha_module else link_linealpha_node
    if (nrow(df_link) == 0) return(p_obj)
    if (isTRUE(link_curve)) {
      df_draw <- df_link
      dx <- df_draw$xend - df_draw$x
      dy <- df_draw$yend - df_draw$y
      mx <- (df_draw$x + df_draw$xend) / 2
      my <- (df_draw$y + df_draw$yend) / 2
      dist_link <- sqrt(dx^2 + dy^2)

      base_abs_curv <- abs(link_curvature)
      curv_abs <- rep(base_abs_curv, nrow(df_draw))
      if (identical(link_curve_mode, "cross")) {
        # Cross mode reset:
        # Prefer the pre-assigned pair channel so a group-pair consistently uses
        # the same left/right or up/down side.
        preferred_sign <- if (!is.null(cross_channel_sign) && length(cross_channel_sign) == 1 && is.finite(cross_channel_sign)) {
          as.integer(ifelse(cross_channel_sign >= 0, 1L, -1L))
        } else {
          NA_integer_
        }
        curv_sign <- rep(ifelse(is.na(preferred_sign), 1L, preferred_sign), nrow(df_draw))
        curv_abs <- rep(max(base_abs_curv, .Machine$double.eps), nrow(df_draw))
        if (!is.null(circle_radius) && length(circle_radius) > 0) {
          # We care more about the visible body of the link channel.
          # Keep a small margin near both ends and enforce avoidance on the middle body.
          t_seq <- seq(0.05, 0.95, length.out = 81)
          endpoint_allow <- 0.26
          ctrl_scale <- 0.42
          # From almost-straight to larger bends; first feasible is the shortest.
          # We expand the upper range so the solver can route links fully outside group circles.
          candidate_abs <- max(base_abs_curv, 0.02) * c(0.10, 0.14, 0.20, 0.27, 0.36, 0.48, 0.64, 0.84, 1.1, 1.45, 1.9, 2.5, 3.1, 4.0, 5.2, 6.6)
          path_inside_count <- function(i, k_signed) {
            x0 <- df_draw$x[i]
            y0 <- df_draw$y[i]
            x1 <- df_draw$xend[i]
            y1 <- df_draw$yend[i]
            dd <- sqrt((x1 - x0)^2 + (y1 - y0)^2)
            if (!is.finite(dd) || dd <= .Machine$double.eps) return(0L)
            midx <- (x0 + x1) / 2
            midy <- (y0 + y1) / 2
            ux <- -(y1 - y0) / dd
            uy <- (x1 - x0) / dd
            cx_ctrl <- midx + ux * (k_signed * dd * ctrl_scale)
            cy_ctrl <- midy + uy * (k_signed * dd * ctrl_scale)
            bx <- (1 - t_seq)^2 * x0 + 2 * (1 - t_seq) * t_seq * cx_ctrl + t_seq^2 * x1
            by <- (1 - t_seq)^2 * y0 + 2 * (1 - t_seq) * t_seq * cy_ctrl + t_seq^2 * y1
            gsrc <- NA_character_
            gtgt <- NA_character_
            if (!is.null(df_draw$group_a) && !is.null(df_draw$group_b)) {
              gsrc <- as.character(df_draw$group_a[i])
              gtgt <- as.character(df_draw$group_b[i])
            }
            inside_any <- rep(FALSE, length(t_seq))
            for (g in names(circle_radius)) {
              rr <- (circle_radius[[g]] + circle_pad)^2
              dist2 <- (bx - circle_centers_x[[g]])^2 + (by - circle_centers_y[[g]])^2
              inside <- dist2 < rr
              # Allow entering source/target group circles near endpoints so links
              # can actually connect back to module outliers.
              if (!is.na(gsrc) && identical(g, gsrc)) {
                inside <- inside & (t_seq > endpoint_allow)
              }
              if (!is.na(gtgt) && identical(g, gtgt)) {
                inside <- inside & (t_seq < (1 - endpoint_allow))
              }
              inside_any <- inside_any | inside
            }
            sum(inside_any)
          }
          path_hits_circle <- function(i, k_signed) {
            path_inside_count(i, k_signed) > 0L
          }
          for (ii in seq_len(nrow(df_draw))) {
            found <- FALSE
            best_hit <- Inf
            best_sign <- ifelse(is.na(preferred_sign), 1L, preferred_sign)
            best_abs <- max(base_abs_curv, .Machine$double.eps)
            for (cand_abs in candidate_abs) {
              if (cand_abs <= .Machine$double.eps) {
                sign_trials <- c(ifelse(is.na(preferred_sign), 1L, preferred_sign))
              } else if (is.na(preferred_sign)) {
                sign_trials <- c(-1L, 1L)
              } else {
                sign_trials <- c(preferred_sign, -preferred_sign)
              }
              for (sgn_i in sign_trials) {
                hit_n <- path_inside_count(ii, sgn_i * cand_abs)
                if (hit_n < best_hit) {
                  best_hit <- hit_n
                  best_sign <- sgn_i
                  best_abs <- cand_abs
                }
                if (hit_n == 0L) {
                  curv_sign[ii] <- sgn_i
                  curv_abs[ii] <- max(cand_abs, .Machine$double.eps)
                  found <- TRUE
                  break
                }
              }
              if (found) break
            }
            if (!found) {
              # Hard fallback: choose the candidate with the smallest circle intrusion.
              curv_sign[ii] <- best_sign
              curv_abs[ii] <- max(best_abs, .Machine$double.eps)
            }
          }
        }
      } else if (identical(link_curve_mode, "inward")) {
        # Inward rule: bend toward global center (0,0), opposite of outward.
        outward_score <- mx * (-dy) + my * dx
        curv_sign <- ifelse(outward_score >= 0, 1, -1)
      } else {
        # Outward rule relative to global center (0,0).
        outward_score <- mx * (-dy) + my * dx
        curv_sign <- ifelse(outward_score >= 0, -1, 1)
      }

      if (identical(link_curve_mode, "cross")) {
        # Render cross links with ggforce bezier curves (same geometry as avoidance solver).
        # The visible segment is clipped to the source/target group orbit boundary so
        # cross-group links do not run through the interior of each group circle.
        ctrl_scale <- 0.42
        lerp2 <- function(p, q, t) p + (q - p) * t
        split_quad <- function(P0, P1, P2, t) {
          P01 <- lerp2(P0, P1, t)
          P12 <- lerp2(P1, P2, t)
          P0112 <- lerp2(P01, P12, t)
          list(
            left = list(P0, P01, P0112),
            right = list(P0112, P12, P2)
          )
        }
        bezier_ctrl <- vector("list", nrow(df_draw))
        for (ii in seq_len(nrow(df_draw))) {
          x0 <- df_draw$x[ii]
          y0 <- df_draw$y[ii]
          x1 <- df_draw$xend[ii]
          y1 <- df_draw$yend[ii]
          dd <- sqrt((x1 - x0)^2 + (y1 - y0)^2)
          if (!is.finite(dd) || dd <= .Machine$double.eps) next
          midx <- (x0 + x1) / 2
          midy <- (y0 + y1) / 2
          ux <- -(y1 - y0) / dd
          uy <- (x1 - x0) / dd
          k_signed <- curv_sign[ii] * curv_abs[ii]
          P0 <- c(x0, y0)
          P1 <- c(midx + ux * (k_signed * dd * ctrl_scale), midy + uy * (k_signed * dd * ctrl_scale))
          P2 <- c(x1, y1)
          t0 <- 0
          t1 <- 1
          # Extract the trimmed quadratic sub-curve on [t0, t1].
          sp1 <- split_quad(P0, P1, P2, t0)
          R0 <- sp1$right[[1]]
          R1 <- sp1$right[[2]]
          R2 <- sp1$right[[3]]
          u <- (t1 - t0) / (1 - t0)
          sp2 <- split_quad(R0, R1, R2, u)
          S0 <- sp2$left[[1]]
          S1 <- sp2$left[[2]]
          S2 <- sp2$left[[3]]
          bezier_ctrl[[ii]] <- data.frame(
            x = c(S0[1], S1[1], S2[1]),
            y = c(S0[2], S1[2], S2[2]),
            .curve_id = ii,
            .pt = 1:3
          )
        }
        bezier_df <- dplyr::bind_rows(bezier_ctrl)
        if (nrow(bezier_df) == 0) return(p_obj)
        bezier_df <- bezier_df[order(bezier_df$.curve_id, bezier_df$.pt), , drop = FALSE]
        return(
          p_obj + ggforce::geom_bezier2(
            data = bezier_df,
            mapping = ggplot2::aes(x = x, y = y, group = .curve_id),
            linetype = link_lt,
            linewidth = 1,
            alpha = link_al,
            color = col_link,
            lineend = "round"
          )
        )
      }

      df_draw$.curv_abs <- curv_abs
      df_pos <- df_draw[curv_sign > 0, , drop = FALSE]
      df_neg <- df_draw[curv_sign <= 0, , drop = FALSE]
      dist_pos <- dist_link[curv_sign > 0]
      dist_neg <- dist_link[curv_sign <= 0]

      p_new <- p_obj
      add_curve_side <- function(p_side, df_side, dist_side, sign_side) {
        if (nrow(df_side) == 0) return(p_side)

        if (identical(link_curve_mode, "cross") && ".curv_abs" %in% colnames(df_side) && nrow(df_side) > 1) {
          curv_bins <- min(10L, nrow(df_side))
          breaks_curv <- unique(stats::quantile(df_side$.curv_abs, probs = seq(0, 1, length.out = curv_bins + 1), na.rm = TRUE))
          if (length(breaks_curv) > 2) {
            df_side$.curv_bin <- as.integer(cut(df_side$.curv_abs, breaks = breaks_curv, include.lowest = TRUE))
            bin_levels_curv <- sort(unique(df_side$.curv_bin))
            p_tmp <- p_side
            for (bid in bin_levels_curv) {
              df_bin <- df_side[df_side$.curv_bin == bid, , drop = FALSE]
              p_tmp <- p_tmp + ggplot2::geom_curve(
                data = df_bin,
                mapping = ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
                curvature = sign_side * stats::median(df_bin$.curv_abs, na.rm = TRUE),
                linetype = link_lt,
                linewidth = 1,
                alpha = link_al,
                color = col_link
              )
            }
            return(p_tmp)
          }
        }

        if (!isTRUE(link_curve_adaptive) || nrow(df_side) == 1 || diff(range(dist_side)) == 0) {
          return(
            p_side + ggplot2::geom_curve(
              data = df_side,
              mapping = ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
              curvature = sign_side * stats::median(df_side$.curv_abs, na.rm = TRUE),
              linetype = link_lt,
              linewidth = 1,
              alpha = link_al,
              color = col_link
            )
          )
        }

        bins_n <- min(link_curve_adaptive_bins, nrow(df_side))
        cuts <- seq(min(dist_side), max(dist_side), length.out = bins_n + 1)
        # Guard against duplicated cut boundaries in edge cases
        cuts <- unique(cuts)
        if (length(cuts) <= 2) {
          return(
            p_side + ggplot2::geom_curve(
              data = df_side,
              mapping = ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
              curvature = sign_side * stats::median(df_side$.curv_abs, na.rm = TRUE),
              linetype = link_lt,
              linewidth = 1,
              alpha = link_al,
              color = col_link
            )
          )
        }

        bin_id <- cut(dist_side, breaks = cuts, include.lowest = TRUE, labels = FALSE)
        bin_levels <- sort(unique(bin_id))
        mul_levels <- seq(link_curve_adaptive_range[1], link_curve_adaptive_range[2], length.out = length(bin_levels))

        p_tmp <- p_side
        for (k in seq_along(bin_levels)) {
          bid <- bin_levels[k]
          df_bin <- df_side[bin_id == bid, , drop = FALSE]
          curv_k <- sign_side * stats::median(df_bin$.curv_abs, na.rm = TRUE) * mul_levels[k]
          p_tmp <- p_tmp + ggplot2::geom_curve(
            data = df_bin,
            mapping = ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
            curvature = curv_k,
            linetype = link_lt,
            linewidth = 1,
            alpha = link_al,
            color = col_link
          )
        }
        p_tmp
      }

      p_new <- add_curve_side(p_new, df_pos, dist_pos, 1)
      p_new <- add_curve_side(p_new, df_neg, dist_neg, -1)
      p_new
    } else {
      p_obj + ggplot2::geom_segment(
        data = df_link,
        mapping = ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
        linetype = link_lt,
        linewidth = 1,
        alpha = link_al,
        color = col_link
      )
    }
  }

  if ((link_level == "module" || link_level == "module&node" || link_level == "nodeinmodule") && nrow(Module_information_plot) > 0) {
    tmpi <- 1
    for (link_type in names(table(Module_information_plot$Group))) {
      modA_tmp <- Module_information_plot %>%
        dplyr::filter(Group == link_type) %>%
        dplyr::select(modA, GroupA) %>%
        dplyr::pull(modA)

      GroupA_tmp <- Module_information_plot %>%
        dplyr::filter(Group == link_type) %>%
        dplyr::select(modA, GroupA) %>%
        dplyr::pull(GroupA) %>%
        unique()

      modB_tmp <- Module_information_plot %>%
        dplyr::filter(Group == link_type) %>%
        dplyr::select(modB, GroupB) %>%
        dplyr::pull(modB)

      GroupB_tmp <- Module_information_plot %>%
        dplyr::filter(Group == link_type) %>%
        dplyr::select(modB, GroupB) %>%
        dplyr::pull(GroupB) %>%
        unique()

      Module_location <- Module_information_plot %>%
        dplyr::filter(Group == link_type) %>%
        dplyr::left_join(
          graph_info[[GroupA_tmp]]$ggplot_node_df %>%
            dplyr::mutate(Modularity = as.character(Modularity)) %>%
            dplyr::filter(Modularity %in% modA_tmp) %>%
            dplyr::select(x, y, Modularity) %>%
            dplyr::group_by(Modularity) %>%
            dplyr::summarise(x_center = mean(x), y_center = mean(y)) %>%
            purrr::set_names(c("GroupA_Module", "GroupA_x_center", "GroupA_y_center")),
          by = c("modA" = "GroupA_Module")
        ) %>%
        dplyr::left_join(
          graph_info[[GroupB_tmp]]$ggplot_node_df %>%
            dplyr::mutate(Modularity = as.character(Modularity)) %>%
            dplyr::filter(Modularity %in% modB_tmp) %>%
            dplyr::select(x, y, Modularity) %>%
            dplyr::group_by(Modularity) %>%
            dplyr::summarise(x_center = mean(x), y_center = mean(y)) %>%
            purrr::set_names(c("GroupB_Module", "GroupB_x_center", "GroupB_y_center")),
          by = c("modB" = "GroupB_Module")
        )

      Module_location_plot <- Module_location %>%
        dplyr::transmute(
          x = GroupA_x_center,
          y = GroupA_y_center,
          xend = GroupB_x_center,
          yend = GroupB_y_center,
          group_a = GroupA,
          group_b = GroupB
        )

      link_info <- dplyr::bind_rows(
        link_info,
        Module_location %>%
          dplyr::transmute(
            link_level = "module",
            group_a = GroupA,
            group_b = GroupB,
            source = as.character(modA),
            target = as.character(modB),
            x = GroupA_x_center,
            y = GroupA_y_center,
            xend = GroupB_x_center,
            yend = GroupB_y_center
          )
      )

      col_i <- color_v[((tmpi - 1) %% length(color_v)) + 1]
      pair_key <- paste(sort(c(GroupA_tmp, GroupB_tmp)), collapse = "|")
      cross_sign <- cross_channel_lookup[[pair_key]]
      p <- add_link_layer(p, Module_location_plot, col_i, link_type = "module", cross_channel_sign = cross_sign)
      tmpi <- tmpi + 1
    }
  }

  if (link_level == "nodeinmodule" && nrow(Module_information_plot) > 0) {
    tmpi <- 1
    for (ri in seq_len(nrow(Module_information_plot))) {
      row_i <- Module_information_plot[ri, , drop = FALSE]
      gA <- row_i$GroupA
      gB <- row_i$GroupB
      modA <- as.character(row_i$modA)
      modB <- as.character(row_i$modB)

      nodes_A <- graph_info[[gA]]$ggplot_node_df %>%
        dplyr::filter(as.character(Modularity) == modA) %>%
        dplyr::select(name, x, y)
      nodes_B <- graph_info[[gB]]$ggplot_node_df %>%
        dplyr::filter(as.character(Modularity) == modB) %>%
        dplyr::select(name, x, y)

      nodeinmodule_links <- nodes_A %>%
        dplyr::inner_join(nodes_B, by = "name", suffix = c("_A", "_B")) %>%
        dplyr::transmute(
          x = x_A,
          y = y_A,
          xend = x_B,
          yend = y_B,
          group_a = gA,
          group_b = gB
        )

      nodeinmodule_links_info <- nodes_A %>%
        dplyr::inner_join(nodes_B, by = "name", suffix = c("_A", "_B")) %>%
        dplyr::transmute(
          link_level = "nodeinmodule",
          group_a = gA,
          group_b = gB,
          source = as.character(name),
          target = as.character(name),
          x = x_A,
          y = y_A,
          xend = x_B,
          yend = y_B
        )

      if (nrow(nodeinmodule_links) > 0) {
        link_info <- dplyr::bind_rows(link_info, nodeinmodule_links_info)
        col_i <- color_v[((tmpi - 1) %% length(color_v)) + 1]
        pair_key <- paste(sort(c(gA, gB)), collapse = "|")
        cross_sign <- cross_channel_lookup[[pair_key]]
        p <- add_link_layer(p, nodeinmodule_links, col_i, link_type = "node", cross_channel_sign = cross_sign)
        tmpi <- tmpi + 1
      }
    }
  }

  if ((link_level == "node" || link_level == "module&node") && ncol(compare_matrix) > 0) {
    tmpi <- 1
    for (i in seq_len(ncol(compare_matrix))) {
      gA <- compare_matrix[1, i]
      gB <- compare_matrix[2, i]

      node_links <- graph_info[[gA]]$ggplot_node_df %>%
        dplyr::select(name, x, y) %>%
        dplyr::inner_join(
          graph_info[[gB]]$ggplot_node_df %>%
            dplyr::select(name, x, y),
          by = "name",
          suffix = c("_A", "_B")
        ) %>%
        dplyr::transmute(
          x = x_A,
          y = y_A,
          xend = x_B,
          yend = y_B,
          group_a = gA,
          group_b = gB
        )

      node_links_info <- graph_info[[gA]]$ggplot_node_df %>%
        dplyr::select(name, x, y) %>%
        dplyr::inner_join(
          graph_info[[gB]]$ggplot_node_df %>%
            dplyr::select(name, x, y),
          by = "name",
          suffix = c("_A", "_B")
        ) %>%
        dplyr::transmute(
          link_level = "node",
          group_a = gA,
          group_b = gB,
          source = as.character(name),
          target = as.character(name),
          x = x_A,
          y = y_A,
          xend = x_B,
          yend = y_B
        )

      link_info <- dplyr::bind_rows(link_info, node_links_info)

      col_i <- color_v[((tmpi - 1) %% length(color_v)) + 1]
      pair_key <- paste(sort(c(gA, gB)), collapse = "|")
      cross_sign <- cross_channel_lookup[[pair_key]]
      p <- add_link_layer(p, node_links, col_i, link_type = "node", cross_channel_sign = cross_sign)
      tmpi <- tmpi + 1
    }
  }

  p <- p +
    coord_fixed(clip = F) +
    theme_ggnetview() +
    theme(legend.position = "none")


  return(
    list(p = p,
         info = as_tibble(Module_information),
         link_info = as_tibble(link_info),
         graph = graph_list,
         topology = if (isTRUE(calculate_topology)) {
           list(network = topology_network,
                sample = topology_sample)
         } else {
           NULL
         }
         )
    )


}
