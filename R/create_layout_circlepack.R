#' @noRd
create_layout_circlepack <- function(
    graph_obj,
    node_add = NULL,
    r = 1,
    anchor_dist = 10,
    scale = TRUE,
    orientation = c("up", "down", "left", "right"),
    angle = 0
) {
  # Circle-packing (a la ggClusterNet's model_maptree): every module becomes a
  # disk whose radius scales with its node count, disks are packed compactly,
  # and each module's nodes are packed inside its own disk. The packing is
  # reproducible for a given seed (ggNetView sets the seed before dispatching the
  # layout), keeping it in line with ggNetView's reproducibility guarantee.
  #
  # `node_add` / `scale` are accepted only for signature compatibility with the
  # ggNetView() dispatcher (which passes them unconditionally) and are ignored:
  # the packing derives all positions/radii from module sizes. `anchor_dist`
  # sets the overall span of the packed figure, `r` is a final scale multiplier,
  # and `orientation` / `angle` rotate it -- matching the other create_layout_*.
  orientation <- match.arg(orientation)
  base_angle <- switch(
    orientation,
    up = 0, right = -pi / 2, down = pi, left = pi / 2
  )
  theta_shift <- base_angle + .normalize_angle(angle)

  node_df <- graph_obj %>%
    tidygraph::activate(nodes) %>%
    tidygraph::as_tibble()

  if (!"name" %in% names(node_df)) {
    stop("`create_layout_circlepack()` requires a `name` node column.", call. = FALSE)
  }
  if (!"Modularity" %in% names(node_df)) {
    stop("`create_layout_circlepack()` requires a `Modularity` node column.", call. = FALSE)
  }

  node_names <- as.character(node_df$name)
  mod_chr    <- as.character(node_df$Modularity)

  # Sentinel-prefixed hierarchy vertices so module labels can never collide with
  # a real node name (which would merge them into one vertex and corrupt the
  # tree). Leaves keep their raw node names so we can realign coordinates later.
  if (any(node_names %in% c("__ggnv_root__")) ||
      any(startsWith(node_names, "__ggnv_mod__"))) {
    stop("Node names collide with the circle-pack layout's internal sentinels ",
         "('__ggnv_root__' / '__ggnv_mod__*'). Rename the offending nodes.",
         call. = FALSE)
  }
  mod_ids <- unique(mod_chr)
  mod_v   <- paste0("__ggnv_mod__", mod_ids)

  edges_h <- rbind(
    data.frame(from = "__ggnv_root__",
               to   = mod_v, stringsAsFactors = FALSE),
    data.frame(from = paste0("__ggnv_mod__", mod_chr),
               to   = node_names, stringsAsFactors = FALSE)
  )
  verts <- data.frame(
    name = c("__ggnv_root__", mod_v, node_names),
    stringsAsFactors = FALSE
  )
  # Unit leaf weight -> a module's disk area scales with its node count (the
  # "bubble" look); internal disks get the sum of their children from the engine.
  verts$node_weight <- ifelse(verts$name %in% node_names, 1, 0)

  h <- igraph::graph_from_data_frame(edges_h, directed = TRUE, vertices = verts)

  lay <- ggraph::create_layout(h, layout = "circlepack", weight = node_weight)

  leaf <- lay[lay$leaf %in% TRUE, , drop = FALSE]
  # Realign leaves to graph_obj's node order: downstream bind_cols() attaches
  # coordinates to node rows BY POSITION, so the order must match exactly.
  idx <- match(node_names, as.character(leaf$name))
  if (anyNA(idx)) {
    stop("Internal error: circle-pack layout dropped some nodes.", call. = FALSE)
  }
  x <- as.numeric(leaf$x[idx])
  y <- as.numeric(leaf$y[idx])

  # Center and rescale so the packed figure spans roughly the same extent as the
  # circular module layouts (keeps node/edge sizing visually comparable).
  x <- x - mean(range(x))
  y <- y - mean(range(y))
  extent <- max(diff(range(x)), diff(range(y)), 1e-10)
  scale_f <- (2 * anchor_dist * r) / extent
  x <- x * scale_f
  y <- y * scale_f

  if (theta_shift != 0) {
    Rm <- matrix(
      c(cos(theta_shift), -sin(theta_shift),
        sin(theta_shift),  cos(theta_shift)),
      nrow = 2
    )
    xy <- Rm %*% rbind(x, y)
    x <- xy[1, ]
    y <- xy[2, ]
  }

  ly <- ggraph::create_layout(graph_obj, layout = "manual", x = x, y = y)
  rownames(ly) <- NULL
  ly
}
