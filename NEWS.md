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
