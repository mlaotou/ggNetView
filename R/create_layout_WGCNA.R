create_layout_WGCNA <- function(
    graph_obj,
    node_add = NULL,
    r = NULL,
    scale = T,
    anchor_dist = 10,
    orientation = c("up","down","left","right"),
    angle = 0,
    module_compactness = 0.6,
    inter_module_repulsion = 1.5
){
  # WGCNA风格布局：结合力导向布局和模块化结构
  # 特点：
  # 1. 整体呈圆形/有机分布（类似fr布局）
  # 2. 模块内部节点紧密聚集
  # 3. 模块之间相对分离
  # 4. 保持网络的整体连通性

  # 旋转角度
  orientation <- match.arg(orientation)
  base_angle <- switch(orientation,
                       up = 0, right = -pi/2, down = pi, left = pi/2)
  theta_shift <- base_angle + angle

  # 获取节点
  node_df <- graph_obj %>%
    tidygraph::activate(nodes) %>%
    tidygraph::as_tibble()

  # 获取边
  edge_df <- graph_obj %>%
    tidygraph::activate(edges) %>%
    tidygraph::as_tibble()

  # 转换为igraph对象
  igraph_obj <- tidygraph::as.igraph(graph_obj)

  # 检查是否有模块信息
  has_modularity <- "Modularity" %in% colnames(node_df)

  if (has_modularity) {
    # ---- 有模块信息的情况：两阶段布局 ----
    
    # 获取模块信息
    mod_levels <- node_df$Modularity %>%
      droplevels() %>%
      levels() %>%
      as.character()
    
    module_list <- node_df %>%
      dplyr::group_split(Modularity)
    
    n_vec <- purrr::map_int(module_list, nrow)
    n_mod <- length(n_vec)
    
    # 第一阶段：在大圆上分布模块中心
    # 模块中心按大小加权分布（大模块在外圈，小模块在内圈）
    module_sizes <- n_vec
    total_nodes <- sum(module_sizes)
    
    # 计算每个模块的半径（基于模块大小）
    # 大模块在外圈，小模块在内圈，但都在一个合理的范围内
    max_size <- max(module_sizes)
    min_size <- min(module_sizes)
    size_range <- max_size - min_size
    if (size_range == 0) size_range <- 1
    
    # 模块中心分布在一个大圆上，但根据大小略有调整
    base_radius <- anchor_dist * 0.8
    radius_variation <- anchor_dist * 0.3
    
    # 计算模块中心位置
    module_centers <- list()
    for (i in seq_len(n_mod)) {
      # 角度：均匀分布
      angle_i <- pi/2 - 2 * pi * (i - 1) / n_mod
      
      # 半径：根据模块大小调整（大模块稍远，小模块稍近）
      size_factor <- (module_sizes[i] - min_size) / size_range
      radius_i <- base_radius + radius_variation * size_factor
      
      module_centers[[i]] <- c(
        radius_i * cos(angle_i),
        radius_i * sin(angle_i)
      )
    }
    
    # 第二阶段：每个模块内部使用fr布局
    # 然后整体调整模块位置和内部节点位置
    
    # 先对整个图使用fr布局作为初始布局
    ly_initial <- ggraph::create_layout(graph_obj, layout = "fr")
    ly_initial <- as.data.frame(ly_initial)
    
    # 确保有name列，如果没有则从rownames获取
    if (!"name" %in% colnames(ly_initial)) {
      if (is.null(rownames(ly_initial))) {
        ly_initial$name <- node_df$name[1:nrow(ly_initial)]
      } else {
        ly_initial$name <- rownames(ly_initial)
      }
    }
    
    # 提取x和y坐标
    ly_initial <- ly_initial %>%
      dplyr::select(name, x, y)
    
    # 获取每个节点所属的模块
    node_mod_map <- node_df %>%
      dplyr::select(name, Modularity) %>%
      dplyr::mutate(Modularity = as.character(Modularity))
    
    # 计算每个模块的质心（基于初始fr布局）
    module_centroids <- list()
    for (i in seq_len(n_mod)) {
      mod_name <- mod_levels[i]
      mod_nodes <- node_mod_map %>%
        dplyr::filter(Modularity == mod_name) %>%
        dplyr::pull(name)
      
      mod_coords <- ly_initial %>%
        dplyr::filter(name %in% mod_nodes)
      
      if (nrow(mod_coords) > 0) {
        module_centroids[[i]] <- c(
          mean(mod_coords$x, na.rm = TRUE),
          mean(mod_coords$y, na.rm = TRUE)
        )
      } else {
        module_centroids[[i]] <- module_centers[[i]]
      }
    }
    
    # 计算缩放因子：将模块质心映射到目标位置
    # 使用质心到目标中心的平移和缩放
    current_centroids <- do.call(rbind, module_centroids)
    target_centers <- do.call(rbind, module_centers)
    
    # 计算当前质心的中心
    current_center <- colMeans(current_centroids, na.rm = TRUE)
    target_center <- colMeans(target_centers, na.rm = TRUE)
    
    # 计算缩放因子（基于模块间距离）
    if (nrow(current_centroids) > 1) {
      current_dist <- mean(dist(current_centroids), na.rm = TRUE)
      target_dist <- mean(dist(target_centers), na.rm = TRUE)
      if (current_dist > 0) {
        scale_factor <- target_dist / current_dist
      } else {
        scale_factor <- 1
      }
    } else {
      scale_factor <- 1
    }
    
    # 调整布局：先平移，再缩放，再平移
    ly_adjusted <- ly_initial
    ly_adjusted$x <- (ly_adjusted$x - current_center[1]) * scale_factor + target_center[1]
    ly_adjusted$y <- (ly_adjusted$y - current_center[2]) * scale_factor + target_center[2]
    
    # 第三阶段：模块内部紧凑化
    # 对每个模块内部的节点进行局部调整，使其更紧密
    ly_final <- ly_adjusted
    
    for (i in seq_len(n_mod)) {
      mod_name <- mod_levels[i]
      mod_nodes <- node_mod_map %>%
        dplyr::filter(Modularity == mod_name) %>%
        dplyr::pull(name)
      
      mod_coords <- ly_final %>%
        dplyr::filter(name %in% mod_nodes)
      
      if (nrow(mod_coords) > 0) {
        # 获取模块中心
        mod_center <- module_centers[[i]]
        
        # 计算节点到模块中心的距离
        mod_centroid <- c(
          mean(mod_coords$x, na.rm = TRUE),
          mean(mod_coords$y, na.rm = TRUE)
        )
        
        # 将节点向模块中心收缩（紧凑化）
        for (j in seq_len(nrow(mod_coords))) {
          node_name <- mod_coords$name[j]
          idx <- which(ly_final$name == node_name)
          
          if (length(idx) > 0) {
            # 计算节点相对于质心的位置
            vec_from_centroid <- c(
              mod_coords$x[j] - mod_centroid[1],
              mod_coords$y[j] - mod_centroid[2]
            )
            
            # 新位置 = 模块中心 + 收缩后的相对位置
            ly_final$x[idx] <- mod_center[1] + vec_from_centroid[1] * module_compactness
            ly_final$y[idx] <- mod_center[2] + vec_from_centroid[2] * module_compactness
          }
        }
      }
    }
    
    # 第四阶段：模块间排斥力调整
    # 确保模块之间有足够的分离
    if (inter_module_repulsion > 1) {
      for (iter in 1:3) {  # 迭代几次
        for (i in seq_len(n_mod - 1)) {
          for (j in (i + 1):n_mod) {
            mod_i_name <- mod_levels[i]
            mod_j_name <- mod_levels[j]
            
            mod_i_nodes <- node_mod_map %>%
              dplyr::filter(Modularity == mod_i_name) %>%
              dplyr::pull(name)
            mod_j_nodes <- node_mod_map %>%
              dplyr::filter(Modularity == mod_j_name) %>%
              dplyr::pull(name)
            
            mod_i_coords <- ly_final %>%
              dplyr::filter(name %in% mod_i_nodes)
            mod_j_coords <- ly_final %>%
              dplyr::filter(name %in% mod_j_nodes)
            
            if (nrow(mod_i_coords) > 0 && nrow(mod_j_coords) > 0) {
              # 计算两个模块的质心
              mod_i_centroid <- c(
                mean(mod_i_coords$x, na.rm = TRUE),
                mean(mod_i_coords$y, na.rm = TRUE)
              )
              mod_j_centroid <- c(
                mean(mod_j_coords$x, na.rm = TRUE),
                mean(mod_j_coords$y, na.rm = TRUE)
              )
              
              # 计算模块间距离
              mod_dist <- sqrt(sum((mod_j_centroid - mod_i_centroid)^2))
              
              # 如果距离太近，增加排斥
              min_dist <- anchor_dist * 0.3
              if (mod_dist < min_dist && mod_dist > 0) {
                # 计算排斥方向
                repulsion_vec <- (mod_j_centroid - mod_i_centroid) / mod_dist
                repulsion_strength <- (min_dist - mod_dist) * (inter_module_repulsion - 1) * 0.1
                
                # 轻微调整模块位置
                for (node_name in mod_i_nodes) {
                  idx <- which(ly_final$name == node_name)
                  if (length(idx) > 0) {
                    ly_final$x[idx] <- ly_final$x[idx] - repulsion_vec[1] * repulsion_strength
                    ly_final$y[idx] <- ly_final$y[idx] - repulsion_vec[2] * repulsion_strength
                  }
                }
                for (node_name in mod_j_nodes) {
                  idx <- which(ly_final$name == node_name)
                  if (length(idx) > 0) {
                    ly_final$x[idx] <- ly_final$x[idx] + repulsion_vec[1] * repulsion_strength
                    ly_final$y[idx] <- ly_final$y[idx] + repulsion_vec[2] * repulsion_strength
                  }
                }
              }
            }
          }
        }
      }
    }
    
    # 提取x和y坐标
    ly <- ly_final %>%
      dplyr::select(x, y)
    
  } else {
    # ---- 没有模块信息：直接使用fr布局 ----
    ly <- ggraph::create_layout(graph_obj, layout = "fr")
    ly <- as.data.frame(ly) %>% dplyr::select(1, 2)
    
    # 确保列名为 x 和 y
    if (!all(c("x", "y") %in% colnames(ly))) {
      colnames(ly) <- c("x", "y")
    }
  }

  # 开始旋转
  # 统一旋转（绕原点）
  if (theta_shift != 0) {
    Rm <- matrix(c(cos(theta_shift), -sin(theta_shift),
                   sin(theta_shift),  cos(theta_shift)), nrow = 2)
    xy <- as.matrix(ly[, c("x","y")])
    ly[, c("x","y")] <- t(Rm %*% t(xy))
  }

  # 确保返回的布局格式正确（与其他布局函数保持一致）
  # 其他布局函数通常设置 rownames(ly) <- NULL，但保留行名可能有助于调试
  # 为了兼容性，我们保持行名为 NULL（如果存在的话，会被后续处理）
  rownames(ly) <- NULL

  return(ly)
}
