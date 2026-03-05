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
#' @param outerlinetype Integer (default = 2).
#' Line type for module outer boundaries.
#' @param outeralpha Numeric (default = 0.5).
#' Alpha for module outer boundaries.
#' @param link_level Character (default = "Module").
#' Cross-group link granularity. One of \code{"None"}, \code{"Module"}, or \code{"Node"}.
#' \code{"None"} draws no cross-group links; \code{"Module"} links significant module matches;
#' \code{"Node"} links shared node names across groups.
#' @param link_curve Logical (default = FALSE).
#' Whether to draw cross-group links as curves (\code{geom_curve}) instead of straight segments.
#' @param link_curvature Numeric (default = 0.2).
#' Curvature used when \code{link_curve = TRUE}.
#' @param link_curve_mode Character (default = "outward").
#' Curve direction strategy used when \code{link_curve = TRUE}.
#' \code{"outward"} bends links away from the global center.
#' \code{"cross"} follows a cross-axis rule: left links bend left, right links bend right,
#' upper links bend up, and lower links bend down.
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
                                 jitter = FALSE,
                                 jitter_sd = 0.01,
                                 mapping_line = FALSE,
                                 linealpha = 0.25,
                                 linecolor = "grey70",
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
    stop("`link_level` must be one of: 'None', 'Module', 'Node'.")
  }
  link_level <- tolower(trimws(link_level))
  if (!link_level %in% c("none", "module", "node")) {
    stop("`link_level` must be one of: 'None', 'Module', 'Node'.")
  }
  if (!is.logical(link_curve) || length(link_curve) != 1 || is.na(link_curve)) {
    stop("`link_curve` must be TRUE or FALSE.")
  }
  if (!is.numeric(link_curvature) || length(link_curvature) != 1 || is.na(link_curvature)) {
    stop("`link_curvature` must be a single numeric value.")
  }
  if (!is.character(link_curve_mode) || length(link_curve_mode) != 1 || is.na(link_curve_mode)) {
    stop("`link_curve_mode` must be one of: 'outward', 'cross'.")
  }
  link_curve_mode <- tolower(trimws(link_curve_mode))
  if (!link_curve_mode %in% c("outward", "cross")) {
    stop("`link_curve_mode` must be one of: 'outward', 'cross'.")
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

  if (link_level == "module" && ncol(compare_matrix) > 0) {
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

  Module_information_plot <- if (link_level == "module") {
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

  # 我们先添加点和线
  for (index in seq_along(names(graph_info))) {
    edge_df <- graph_info[[index]]$ggplot_edge_df

    # plot link
    if (isFALSE(mapping_line)) {
      p <- p +
        ggplot2::geom_segment(data = edge_df,
                              mapping = ggplot2::aes(x = from_x,
                                                     xend = to_x,
                                                     y = from_y,
                                                     yend = to_y),
                              color = linecolor,
                              alpha = linealpha)
    } else if (isTRUE(mapping_line)) {
      if (!"corr_direction" %in% colnames(edge_df)) {
        stop("`mapping_line = TRUE` requires `corr_direction` in edge data.")
      }
      p <- p +
        ggnewscale::new_scale_color() +
        ggplot2::geom_segment(data = edge_df,
                              mapping = ggplot2::aes(x = from_x,
                                                     xend = to_x,
                                                     y = from_y,
                                                     yend = to_y,
                                                     colour = corr_direction),
                              alpha = linealpha) +
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
        ggplot2::geom_segment(data = edge_df,
                              mapping = ggplot2::aes(x = from_x,
                                                     xend = to_x,
                                                     y = from_y,
                                                     yend = to_y,
                                                     colour = .data[[mapping_line]]),
                              alpha = linealpha) +
        line_scale
    }

    module_targets <- character(0)
    if (link_level == "module" && nrow(Module_information_plot) > 0) {
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
    }

    outer_node_df <- graph_info[[index]]$ggplot_node_df %>%
      dplyr::filter(name %in% (graph_list[[index]] %>%
                                 tidygraph::activate(nodes) %>%
                                 tidygraph::as_tibble() %>%
                                 dplyr::mutate(Modularity = as.character(Modularity)) %>%
                                 dplyr::filter(Modularity %in% module_targets) %>%
                                 dplyr::pull(name)))

    p <- p +
      ggnewscale::new_scale_fill() +
      ggplot2::geom_point(data = graph_info[[index]]$ggplot_node_df,
                          mapping = ggplot2::aes(x = x,
                                                 y = y,
                                                 fill = Modularity,
                                                 size = Degree),
                          shape = 21) +
      ggnewscale::new_scale_fill()

    if (add_outer == "circle" && nrow(outer_node_df) > 0) {
      circle_n <- max(40, min(300, as.integer(round(8 * sqrt(nrow(outer_node_df))))))
      p <- p +
        ggforce::geom_mark_circle(
          data = outer_node_df,
          mapping = ggplot2::aes(x = x, y = y, fill = Modularity),
          n = circle_n,
          expand = grid::unit(1, "mm")
        )
    }

    if (add_outer == "manual" && nrow(outer_node_df) > 2) {
      maskTable <- generateMask_ggnetview(
        dims = outer_node_df %>% dplyr::select(x, y),
        clusters = outer_node_df %>% dplyr::pull(Modularity),
        q = q_outer,
        expand = expand_outer
      )
      p <- p +
        ggplot2::geom_polygon(
          data = maskTable,
          mapping = ggplot2::aes(x = x, y = y, group = cluster, fill = cluster, color = cluster),
          linewidth = outerwidth,
          linetype = outerlinetype,
          alpha = outeralpha,
          show.legend = FALSE
        )
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

  add_link_layer <- function(p_obj, df_link, col_link) {
    if (nrow(df_link) == 0) return(p_obj)
    if (isTRUE(link_curve)) {
      dx <- df_link$xend - df_link$x
      dy <- df_link$yend - df_link$y
      mx <- (df_link$x + df_link$xend) / 2
      my <- (df_link$y + df_link$yend) / 2

      if (identical(link_curve_mode, "cross")) {
        # Cross-axis rule:
        # choose one of left/right/up/down by midpoint position, then bend toward that side.
        use_horizontal <- abs(mx) >= abs(my)
        dir_x <- ifelse(use_horizontal, ifelse(mx >= 0, 1, -1), 0)
        dir_y <- ifelse(!use_horizontal, ifelse(my >= 0, 1, -1), 0)
        target_score <- dir_x * (-dy) + dir_y * dx
        # In ggplot2::geom_curve, sign is opposite to this geometric score.
        curv_sign <- ifelse(target_score >= 0, -1, 1)
      } else {
        # Outward rule relative to global center (0,0).
        outward_score <- mx * (-dy) + my * dx
        # In ggplot2::geom_curve, sign is opposite to this geometric score.
        curv_sign <- ifelse(outward_score >= 0, -1, 1)
      }

      df_pos <- df_link[curv_sign > 0, , drop = FALSE]
      df_neg <- df_link[curv_sign <= 0, , drop = FALSE]

      p_new <- p_obj
      if (nrow(df_pos) > 0) {
        p_new <- p_new + ggplot2::geom_curve(
          data = df_pos,
          mapping = ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
          curvature = abs(link_curvature),
          linetype = 2,
          linewidth = 1,
          alpha = linealpha,
          color = col_link
        )
      }
      if (nrow(df_neg) > 0) {
        p_new <- p_new + ggplot2::geom_curve(
          data = df_neg,
          mapping = ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
          curvature = -abs(link_curvature),
          linetype = 2,
          linewidth = 1,
          alpha = linealpha,
          color = col_link
        )
      }
      p_new
    } else {
      p_obj + ggplot2::geom_segment(
        data = df_link,
        mapping = ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
        linetype = 2,
        linewidth = 1,
        alpha = linealpha,
        color = col_link
      )
    }
  }

  if (link_level == "module" && nrow(Module_information_plot) > 0) {
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
          yend = GroupB_y_center
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
      p <- add_link_layer(p, Module_location_plot, col_i)
      tmpi <- tmpi + 1
    }
  }

  if (link_level == "node" && ncol(compare_matrix) > 0) {
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
          yend = y_B
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
      p <- add_link_layer(p, node_links, col_i)
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
