#' @noRd
create_layout_pack <- function(
    graph_obj,
    node_add = NULL,
    r = NULL,
    scale = TRUE,
    anchor_dist = 10,
    orientation = c("up", "down", "left", "right"),
    angle = 0
) {
  # A COMPACT, module-agnostic pool of node slots: N equal circles packed into a
  # tight disk (via ggraph's 'circlepack'). Like the other silhouette layouts
  # (circle / star / heart), this ONLY draws the overall shape -- it does not
  # decide which module goes where. That is the job of `layout.module`
  # (random / adjacent / order) inside ggNetView(), and the inter-module spacing
  # comes from `shrink`. So the layout is intentionally tight; the real
  # visualisation is produced by combining it with ggNetView().
  #
  # `node_add` / `r` / `scale` are accepted only for signature compatibility with
  # the ggNetView() dispatcher and are unused (the packing fixes the spacing);
  # `anchor_dist` sets the overall span and `orientation` / `angle` rotate it,
  # matching the other create_layout_* functions. The packing is deterministic
  # for a given seed (ggNetView sets the seed before dispatching the layout).
  orientation <- match.arg(orientation)
  base_angle <- switch(
    orientation,
    up = 0, right = -pi / 2, down = pi, left = pi / 2
  )
  theta_shift <- base_angle + .normalize_angle(angle)

  node_df <- graph_obj %>%
    tidygraph::activate(nodes) %>%
    tibble::as_tibble()
  n <- nrow(node_df)
  if (n <= 0) return(data.frame(x = numeric(), y = numeric()))
  if (n == 1) return(data.frame(x = 0, y = 0))

  # Pack N equal unit circles into a compact disk; the circle centres are the
  # node slots. A flat root -> N-leaves hierarchy with equal leaf weights makes
  # every packed circle the same size (module-agnostic).
  leaves <- paste0("__ggnv_leaf__", seq_len(n))
  edges_h <- data.frame(from = "__ggnv_root__", to = leaves, stringsAsFactors = FALSE)
  verts <- data.frame(name = c("__ggnv_root__", leaves), stringsAsFactors = FALSE)
  verts$node_weight <- c(0, rep(1, n))
  h <- igraph::graph_from_data_frame(edges_h, directed = TRUE, vertices = verts)

  lay <- ggraph::create_layout(h, layout = "circlepack", weight = node_weight)
  leaf <- lay[lay$leaf %in% TRUE, , drop = FALSE]

  ly <- data.frame(x = as.numeric(leaf$x), y = as.numeric(leaf$y))

  # Centre and rescale to a span comparable to the other silhouette layouts
  # (their extent is roughly 2 * anchor_dist).
  ly$x <- ly$x - mean(range(ly$x))
  ly$y <- ly$y - mean(range(ly$y))
  ext <- max(diff(range(ly$x)), diff(range(ly$y)), 1e-10)
  f <- (2 * anchor_dist) / ext
  ly$x <- ly$x * f
  ly$y <- ly$y * f

  if (theta_shift != 0) {
    Rm <- matrix(
      c(cos(theta_shift), -sin(theta_shift),
        sin(theta_shift),  cos(theta_shift)),
      nrow = 2
    )
    xy <- as.matrix(ly[, c("x", "y")])
    ly[, c("x", "y")] <- t(Rm %*% t(xy))
  }

  rownames(ly) <- NULL
  ly
}
