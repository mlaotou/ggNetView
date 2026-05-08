# testthat sources every file matching `helper-*.R` in this directory
# BEFORE running any test_that() block, so the helpers below are
# guaranteed to be defined by the time tests in `test-inner_shrink.R`
# reference them.
#
# Previously these helpers lived at the bottom of `test-inner_shrink.R`,
# which broke under R CMD check: testthat parses the file top-to-bottom
# and executes test_that() blocks as it encounters them, so functions
# defined after the tests were not yet visible when the tests ran.

make_test_wgcna_graph <- function() {
  # 3 dense modules + 1 sparse "Others"; all with `modularity3` column.
  set.seed(7)

  modA <- 1:8       # 8 nodes, fully-connected ring
  modB <- 9:14      # 6 nodes, ring
  modC <- 15:22     # 8 nodes, ring
  others <- 23:25   # 3 isolated nodes (no edges)

  ring_edges <- function(v) data.frame(from = v, to = c(v[-1], v[1]))
  el <- rbind(ring_edges(modA), ring_edges(modB), ring_edges(modC))
  # Add a few inter-module bridges so there's something to lay out.
  el <- rbind(el, data.frame(from = c(1, 9, 15), to = c(9, 15, 1)))

  ig <- igraph::graph_from_data_frame(
    el,
    directed = FALSE,
    vertices = data.frame(
      name = as.character(c(modA, modB, modC, others)),
      modularity3 = c(rep("M1", length(modA)),
                      rep("M2", length(modB)),
                      rep("M3", length(modC)),
                      rep("Others", length(others)))
    )
  )
  # Edge weights are needed by `centrality_degree(weights = weight)` below
  # and by ggNetView()'s downstream code paths.  A constant weight of 1
  # turns weighted centrality into a no-op (= unweighted degree) which is
  # the right semantic for an unweighted ring graph.
  igraph::E(ig)$weight <- 1

  # Augment the bare tbl_graph with the same vertex schema that the
  # `build_graph_from_*()` family produces.  Without this, downstream
  # callers like `ggNetView() -> module_layout()` fail with
  # "object 'Degree' not found" the moment they try to sort nodes by
  # degree, even on a graph that visibly has degree information.
  #
  # The factor levels for `modularity / modularity2 / Modularity` mirror
  # `modularity3` so the synthetic test labels (M1, M2, M3, Others) line
  # up with the modularity-aware code paths inside ggNetView() instead
  # of being silently overwritten by an auto-detected community.
  tidygraph::as_tbl_graph(ig) %>%
    tidygraph::mutate(
      modularity  = factor(modularity3),
      modularity2 = factor(modularity3),
      Modularity  = factor(modularity3),
      Degree      = tidygraph::centrality_degree(mode = "out"),
      Segree      = tidygraph::centrality_degree(mode = "out"),
      Strength    = tidygraph::centrality_degree(weights = weight)
    )
}

expected_module_geometry <- function(g) {
  # Re-derive the disc centres (cx, cy) and radii the WGCNA layout would
  # assign, using the same packing the layout uses internally.
  node_df <- g %>% tidygraph::activate(nodes) %>% tidygraph::as_tibble()

  mod_levels_df <- node_df %>%
    dplyr::count(modularity3, name = "size") %>%
    dplyr::arrange(dplyr::desc(size)) %>%
    dplyr::mutate(modularity4 = factor(
      modularity3,
      levels = c(setdiff(modularity3, "Others"), "Others"),
      ordered = TRUE
    )) %>%
    dplyr::arrange(modularity4) %>%
    dplyr::mutate(modularity4 = as.character(modularity4))
  mod_levels <- mod_levels_df$modularity4

  counts <- node_df %>%
    dplyr::count(modularity3, name = "n") %>%
    dplyr::mutate(modularity3 = as.character(modularity3))
  counts <- counts[match(mod_levels, counts$modularity3), , drop = FALSE]
  counts$radius <- sqrt(counts$n) * 1   # r = 1

  packing <- ggNetView:::.pack_wgcna_modules(counts$radius)
  counts$cx <- packing$cx
  counts$cy <- packing$cy

  # Determine which modules have internal edges.
  ig <- igraph::as.igraph(g)
  vmod <- igraph::vertex_attr(ig, "modularity3")
  with_edges <- list()
  no_edges <- list()
  for (i in seq_len(nrow(counts))) {
    name <- counts$modularity3[i]
    keep <- which(vmod == name)
    sub  <- igraph::induced_subgraph(ig, vids = keep)
    rec <- list(name = name, cx = counts$cx[i], cy = counts$cy[i],
                radius = counts$radius[i])
    if (igraph::ecount(sub) == 0L) no_edges[[length(no_edges) + 1L]] <- rec
    else                            with_edges[[length(with_edges) + 1L]] <- rec
  }
  list(with_edges = with_edges, no_edges = no_edges)
}

attach_module_to_points <- function(ly, g) {
  # The layout returns rows in the order produced by mod_levels traversal;
  # rebuild that ordering to label each row with its module.
  node_df <- g %>% tidygraph::activate(nodes) %>% tidygraph::as_tibble()
  mod_levels_df <- node_df %>%
    dplyr::count(modularity3, name = "size") %>%
    dplyr::arrange(dplyr::desc(size)) %>%
    dplyr::mutate(modularity4 = factor(
      modularity3,
      levels = c(setdiff(modularity3, "Others"), "Others"),
      ordered = TRUE
    )) %>%
    dplyr::arrange(modularity4) %>%
    dplyr::mutate(modularity4 = as.character(modularity4))
  mod_levels <- mod_levels_df$modularity4

  ig <- igraph::as.igraph(g)
  vmod <- igraph::vertex_attr(ig, "modularity3")
  ordered_mods <- unlist(lapply(mod_levels, function(m) {
    rep(m, sum(vmod == m))
  }))
  data.frame(x = ly$x, y = ly$y, modularity3 = ordered_mods,
             stringsAsFactors = FALSE)
}

module_centroids <- function(ly, g) {
  pts <- attach_module_to_points(ly, g)
  pts %>%
    dplyr::group_by(modularity3) %>%
    dplyr::summarise(cx = mean(x), cy = mean(y), .groups = "drop") %>%
    dplyr::arrange(modularity3)
}
