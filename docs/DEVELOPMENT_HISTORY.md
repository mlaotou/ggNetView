# Development history

## Changelog (newest first)

### 2026-09-24 — Document module-outline tuning in the README

- **Type**: docs
- **Motivation**: On the user's food-web data, the restored spline outline
  still looked worse than their 2026-03-25 figure. Rendering that figure's data
  with the 2026-03 build (`ff8f669`) and with HEAD showed the outline algorithm
  is identical. The difference came from arguments:
  `module_outline_expand = 1.25` (was 1.1) magnifies small dents, the default
  line is now solid width 1 (was dashed 1.25), and the legacy `color = pal`
  now also sets `edge_color_values`.
- **What**: README.Rmd / README.md gain a "tune the module outline" note after
  the `p2` example, covering `module_outline_method`, `_q`, `_expand`,
  `_linetype` and `_width`. The misleading "outer hull/line" comments now say
  "smooth outline". No code changes. The gephi ring-offset precedence fix
  (which moves where modules sit versus 2026-03) is kept on purpose.
- **Impact**: Documentation only; README figures were not re-rendered.
- **Verification**: `R CMD check --no-manual --ignore-vignettes` on a clean
  copy of the tree; user's P1 script re-rendered with the documented
  arguments and compared visually with `p_network.pdf`.

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
