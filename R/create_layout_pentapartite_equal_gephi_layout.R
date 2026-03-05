create_layout_pentapartite_equal_gephi_layout <- function(
    graph_obj,
    r = 1,
    node_add = 7,
    anchor_dist = 10,
    scale = TRUE,
    orientation = c("up","down","left","right"),
    angle = 0
){
  orientation <- match.arg(orientation)
  base_angle <- switch(
    orientation,
    up    = 0,
    right = -pi/2,
    down  = pi,
    left  = pi/2
  )
  theta_shift <- base_angle + angle

  # ---- 正五边形的 5 个锚点（类似五角星五个顶点）----
  # 先按“上方一个顶点”的构型放置，再统一旋转
  angles <- pi/2 - 2 * pi * (0:4) / 5
  anchors <- lapply(angles, function(a) {
    c(anchor_dist * cos(a), anchor_dist * sin(a))
  })

  # ---- 获取节点和模块（保留原始节点顺序，方便最后对齐）----
  node_df <- graph_obj %>%
    tidygraph::activate(nodes) %>%
    tidygraph::as_tibble() %>%
    dplyr::mutate(.node_index__ = dplyr::row_number())

  mod_levels <- node_df$Modularity %>%
    droplevels() %>%
    levels() %>%
    as.character()

  module_list <- node_df %>%
    dplyr::group_split(Modularity, .keep = TRUE)

  n_vec <- purrr::map_int(module_list, nrow)

  if (length(n_vec) < 5) {
    stop("Pentapartite layout 需要至少 5 个模块（来自列 Modularity）。")
  }
  if (length(n_vec) > 5) {
    message("检测到超过 5 个模块，仅使用前 5 个模块进行五边形布局。")
    module_list <- module_list[1:5]
    n_vec       <- n_vec[1:5]
    mod_levels  <- mod_levels[1:5]
  }

  # ---- 同心圆节点分层 ----
  circle_layout <- function(n, node_add) {
    counts <- 1
    total  <- 1
    i <- 2
    while (total < n) {
      add <- node_add * (i - 1)
      if (total + add <= n) {
        counts <- c(counts, add)
        total  <- total + add
      } else {
        counts <- c(counts, n - total)
        total  <- n
      }
      i <- i + 1
    }
    counts
  }

  # 每个模块需要的圈数（用于统一“最大半径”标尺）
  n_circle_vec <- purrr::map_int(n_vec, ~ length(circle_layout(.x, node_add)))
  n_circle_max <- max(n_circle_vec)

  # ---- 用最大模块定标：最大模块圈间距为 r，其余模块填充到同一外半径 ----
  R_max <- (n_circle_max - 1) * r

  n_vec_node <- purrr::map(n_vec, ~{
    data.frame(
      number_circle = seq_along(circle_layout(.x, node_add)),
      number_node   = circle_layout(.x, node_add)
    )
  })

  # ---- 同心圆布局函数 ----
  concentric_from_anchor <- function(cx, cy, info_df, r_step) {
    # 第一圈：单点在锚点处
    ly <- data.frame(x = cx, y = cy)
    offset <- 0
    prev_n <- info_df$number_node

    if (nrow(info_df) >= 2) {
      for (index in 2:nrow(info_df)) {
        if (index == 2) {
          # 第二圈：均匀分布
          l <- 2 * pi * (0:(prev_n[index] - 1)) / prev_n[index]
        } else {
          # 第三圈开始：每圈错开半个身位
          offset <- (offset + pi / prev_n[index]) %% (2 * pi)
          l <- offset + 2 * pi * (0:(prev_n[index] - 1)) / prev_n[index]
        }

        x <- cx + sin(l) * (index - 1) * r_step
        y <- cy + cos(l) * (index - 1) * r_step
        ly <- dplyr::bind_rows(ly, data.frame(x = x, y = y))
      }
    }
    ly
  }

  # ---- 按模块生成布局：每个模块最外圈半径都等于 R_max ----
  ly_list <- vector("list", 5L)
  for (i in 1:5) {
    cx <- anchors[[i]][1]
    cy <- anchors[[i]][2]
    info_df <- n_vec_node[[i]]
    n_circle_i <- nrow(info_df)

    if (n_circle_i <= 1) {
      r_step_i <- 0
    } else {
      r_step_i <- R_max / (n_circle_i - 1)
    }

    coords_i <- concentric_from_anchor(cx, cy, info_df, r_step = r_step_i)
    nodes_i <- module_list[[i]] %>%
      dplyr::select(.node_index__)

    if (nrow(coords_i) != nrow(nodes_i)) {
      stop("Internal error: coords length != node count. Please check circle_layout logic.")
    }

    ly_i <- dplyr::bind_cols(nodes_i, coords_i)
    ly_i$group <- mod_levels[i]
    ly_list[[i]] <- ly_i
  }

  ly <- dplyr::bind_rows(ly_list)

  # ---- 统一旋转 ----
  if (theta_shift != 0) {
    Rm <- matrix(
      c(cos(theta_shift), -sin(theta_shift),
        sin(theta_shift),  cos(theta_shift)),
      nrow = 2
    )
    xy <- as.matrix(ly[, c("x", "y")])
    ly[, c("x", "y")] <- t(Rm %*% t(xy))
  }

  # ---- scale（可选）：把整体缩放到 [-1, 1] ----
  if (isTRUE(scale)) {
    rescale01 <- function(v) {
      rng <- range(v, na.rm = TRUE)
      if (diff(rng) == 0) return(rep(0, length(v)))
      (v - rng[1]) / diff(rng)
    }
    ly$x <- rescale01(ly$x) * 2 - 1
    ly$y <- rescale01(ly$y) * 2 - 1
  }

  # ---- 恢复为原始节点顺序（只返回坐标与 group）----
  ly <- ly %>%
    dplyr::arrange(.node_index__) %>%
    dplyr::select(x, y, group)

  rownames(ly) <- NULL
  ly
}
