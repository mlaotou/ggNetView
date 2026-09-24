# Development history

## Changelog (newest first)

### 2026-09-24 — Restore the smooth-spline module outline (`module_outline_method`)

- **Type**: feat
- **Motivation**: Module outlines drawn with `module_outline = TRUE` were
  rounded blobs up to 0.2.0. The 2026-05-22 switch to a KDE + HDR contour
  (with a convex-hull fallback) made them jagged or straight-edged, which no
  longer matched the figures the user wanted.
- **What**: `R/generate_mask.R` gains `method = c("spline", "kde")` and
  `.mask_polar_spline()`, the pre-0.2.1 algorithm (angular bins, per-bin radius
  quantile `q`, `stats::smooth.spline(spar = 0.5)`), now also returning
  `polygon_id`. New argument `module_outline_method` is added to `ggNetView()`
  and `ggNetView_multi_link()`, and read from `...` / `sub_args` in
  `ggnetview_modularity_heatmaps()` and `ggnetview_subgraph()`. The man pages
  were regenerated. No new dependencies (only `stats`).
- **Impact**: Default outline shape changes back to the spline one; use
  `module_outline_method = "kde"` to reproduce 0.2.1 output.
  `module_outline_bandwidth` only applies to `"kde"`.
- **Verification**: New `tests/testthat/test-generate-mask.R` (13 expectations
  pass); `test-ggnetview-arguments.R` passes (35 pass, 1 skip). Rendered
  `otu_tab` with `layout = "gephi"`, `layout_module = "adjacent"`: spline
  gives rounded per-module blobs, and `"kde"` output is identical to before.
