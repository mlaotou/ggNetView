# ggNetView (development version)

## Breaking changes

* The module outer boundary drawn by `ggNetView(add_outer = TRUE)` is now
  computed via 2D kernel density estimation followed by a Highest-Density-Region
  (HDR) contour, replacing the previous polar-quantile / radial-spline
  algorithm. The user-facing parameters `q_outer` and `expand_outer` are kept,
  but their meaning has been reinterpreted in HDR terms (see `?ggNetView`).
  Visual results from `add_outer = TRUE` therefore differ from prior versions;
  in particular, sparse satellite / outlier nodes will now generally fall
  outside the contour rather than dragging the boundary toward themselves.
* `generateMask_ggnetview()` now returns an additional `polygon_id` column to
  support clusters whose HDR contour has multiple disconnected components.
  Downstream `geom_polygon()` aesthetics in `ggNetView()`,
  `ggnetview_modularity_heatmaps()` and `ggNetView_multi_link()` were updated
  accordingly. Custom callers reusing the mask table should switch
  `group = cluster` to `group = interaction(cluster, polygon_id)`.
* The multipartite layout family (`create_layout_tripartite_*`,
  `create_layout_quadripartite_*`, `create_layout_cross_quadripartite_*`,
  `create_layout_pentapartite_*`) now requires the graph to have exactly
  3 / 4 / 5 modules and raises an error otherwise. The previous behaviour
  silently truncated the layout to the first N modules while leaving the
  remaining nodes in the graph, which produced row-count mismatches in
  downstream `bind_cols()` calls. Filter `graph_obj` to the expected number
  of modules before calling these layouts.
* `build_graph_from_igraph()` now raises an error when an explicit
  `module_attr` is supplied but is not present on the graph, instead of
  silently falling back to community detection.
* `trans_adjacency_matrix_to_df()` now returns a `from`, `to`, `weight`
  data frame (previously the `weight` column was silently dropped even
  though the graph was built with `weighted = TRUE`).

## Bug fixes

* `create_layout_bipartite_layout()` and `create_layout_bipartite_gephi_layout()`
  no longer crash on default invocation: the broken `scale = scale` default
  (which resolved to `base::scale`, a function) is now `scale = TRUE`.
* `build_graph_from_double_mat()` and
  `build_graph_from_double_mat_with_module()` now respect the user's
  `directed` argument. A duplicate `igraph::graph_from_data_frame()` call
  had been silently hard-coding `directed = FALSE`.
* In `build_graph_from_module()`, `build_graph_from_double_mat_with_module()`
  and `build_graph_from_adj_mat_module()`, nodes assigned to `"Others"`
  are no longer turned into `NA` after the `factor()` step. The internal
  `factor_levels` vector now includes `"Others"`.
* The significance-bin `case_when()` chains in `gglink_heatmaps.R`,
  `gglink_heatmaps_2.R`, `build_graph_from_double_mat.R`,
  `build_graph_from_double_mat_with_module.R`,
  `build_graph_from_multi_mat.R` and `use_function.R` no longer return
  `NA` for `Pvalue` values that fall exactly on a boundary (`0.05`,
  `0.01`, `0.001`).
* Functions across the package now handle empty / single-element /
  no-bootstrap inputs gracefully where they previously crashed or
  produced silently wrong output. Affected files include
  `get_location.R`, `get_network_topology.R`,
  `get_network_topology_parallel.R`, `get_geo_neighbors.R`,
  `create_layout_multirings.R`, `use_function.R`, the
  `create_layout_petal*`, `create_layout_square*`,
  `create_layout_rectangle`, `create_layout_gephi`,
  `create_layout_circular_modules_grid_layout`, and the
  `build_graph_*` family (`1:max_model` / `1:top_modules` -> `seq_len(...)`).
* `gglink_heatmap_triple()` no longer hard-codes the layout split
  position at 31; `create_layout2()` now attaches the correct
  non-hub / hub split point as an attribute on the returned layout.
* Added numerical guards across `get_network_topology*`,
  `build_graph_from_consensus.R`, `ggNetView_RMT.R`, `sparcc_matrix.R`
  and `compare_modules_info.R` for `mean(numeric(0))`, `min(numeric(0))`,
  `log(<= 0)` and divisions by zero / negative `phyper` parameters.
* `generateMask_ggnetview()` now returns `NULL` (so the convex-hull
  fallback is used) when the KDE-derived HDR threshold is NA.

# ggNetView 0.1.0

## Initial CRAN release

* Initial submission of `ggNetView` to CRAN.
* Provides a unified, reproducible framework for analyzing and
  visualizing complex biological, ecological, and microbial association
  networks.
* Includes tools for building correlation and co-occurrence networks
  via `WGCNA`, `SpiecEasi`, `SparCC`, and standard correlation methods
  (Pearson, Spearman, Kendall).
* Computes node-level and network-level topological metrics, including
  robustness analyses.
* Supports module-level analyses (modularity detection, Zi-Pi
  classification, sample-level subgraph topology).
* Offers a large family of deterministic layout generators built on
  `ggraph` and `ggplot2` (bipartite, tripartite, quadripartite,
  pentapartite, circular-modules, petal, diamond, heart, star, and
  more), all with reproducible seeds.
* Ships 18 example datasets covering OTU tables, taxonomy tables,
  environmental metadata, PPI networks, and modularity examples.
