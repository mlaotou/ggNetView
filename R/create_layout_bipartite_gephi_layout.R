#' @noRd
create_layout_bipartite_gephi_layout <- function(
    graph_obj,
    r = 1,
    anchor_dist = 10,
    node_add = 7,
    scale = TRUE,
    orientation = c("up","down","left","right"),
    angle = 0
){


  # graph_obj = graph_sub[[3]]

  orientation <- match.arg(orientation)
  base_angle <- switch(orientation,
                       up = 0, right = -pi/2, down = pi, left = pi/2)
  theta_shift <- base_angle + .normalize_angle(angle)

  # set radius
  radius = r


  node_df <- graph_obj %>%
    tidygraph::activate(nodes) %>%
    tidygraph::as_tibble()

  Modularity_name <- node_df$Modularity  %>% droplevels() %>% levels() %>% as.character()

  mod_levels <- node_df$Modularity %>% droplevels() %>% levels() %>% as.character()

  module_list <- node_df %>% dplyr::group_split(Modularity)


  n_vec <- purrr::map(module_list, ~dim(.x)[1]) %>% unlist()

  if (length(n_vec) < 2) {
    stop("Bipartite layout requires at least 2 modules in `Modularity`.")
  }
  if (length(n_vec) > 2) {
    message("`Bipartite layout more than 2 modules detected.")
  }


  circle_layout <- function(n, node_add){

    counts <- 1
    total <- 1
    i <- 2
    while (total < n) {
      add <- node_add * (i-1)
      if (total + add <= n) {
        counts <- c(counts, add)
        total <- total + add
      }else{

        counts <- c(counts, n-total)
        total <- n
      }
      i <- i + 1
    }
    return(counts)
  }

  n_vec

  n_vec_node <- purrr::map(n_vec, function(x){
    data.frame(
      number_circle = seq_along(circle_layout(n = x, node_add = node_add)),
      number_node = circle_layout(n = x, node_add = node_add)
    )
  })

  # Helper: concentric-ring coordinates around a center placed on the x-axis.
  # Handles the single-ring case (module with only a center node) safely and
  # uses the correctly-parenthesised angular offset.
  ring_coords <- function(center_x, ring_counts) {
    ly <- data.frame(x = center_x, y = 0)
    prev_n <- ring_counts$number_node
    n_ring <- nrow(ring_counts)
    offset <- 0
    if (n_ring >= 2) {
      for (index in seq_len(n_ring)[-1L]) {
        if (index == 2) {
          l <- 2 * pi * (0:(prev_n[index] - 1)) / prev_n[index]
        } else {
          offset <- (offset + pi / prev_n[index]) %% (2 * pi)
          l <- offset + (2 * pi * (0:(prev_n[index] - 1)) / prev_n[index])
        }
        x <- center_x + sin(l) * (index - 1) * r
        y <- 0 + cos(l) * (index - 1) * r
        ly <- dplyr::bind_rows(ly, data.frame(x = x, y = y))
      }
    }
    ly
  }

  # Spread module centers symmetrically along the x-axis. For the canonical
  # 2-module (bipartite) case this reproduces centers at -anchor_dist and
  # +anchor_dist; >2 modules are placed evenly between the two poles.
  n_mod <- length(n_vec_node)
  centers_x <- seq(-anchor_dist, anchor_dist, length.out = n_mod)

  ly_list <- lapply(seq_len(n_mod), function(i) {
    ring_coords(centers_x[i], n_vec_node[[i]])
  })

  ly <- do.call(rbind, ly_list)


  if (theta_shift != 0) {
    Rm <- matrix(c(cos(theta_shift), -sin(theta_shift),
                   sin(theta_shift),  cos(theta_shift)), nrow = 2)
    xy <- as.matrix(ly[, c("x","y")])
    ly[, c("x","y")] <- t(Rm %*% t(xy))
  }

  return(ly)


}
