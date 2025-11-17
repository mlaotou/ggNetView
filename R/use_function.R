cor_test2 <- function(Environment, Experiment){
  # Environment self
  cor_out_self <- psych::corr.test(Environment)

  cor_out_self_r <- cor_out_self$r %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "Sample") %>%
    tidyr::pivot_longer(cols = -Sample, names_to = "Experiment", values_to = "Value")

  cor_out_self_p <- cor_out_self$p %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "Sample") %>%
    tidyr::pivot_longer(cols = -Sample, names_to = "Experiment", values_to = "Pvalue")

  cor_out_self_stat <- cor_out_self_r %>%
    dplyr::left_join(cor_out_self_p, by = c("Sample", "Experiment"))

  # Environment and Experiment Correlation
  cor_out <- psych::corr.test(Environment, Experiment)

  cor_out_r <- cor_out$r %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "Sample") %>%
    tidyr::pivot_longer(cols = -Sample, names_to = "Experiment", values_to = "Value")

  cor_out_p <- cor_out$p %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "Sample") %>%
    tidyr::pivot_longer(cols = -Sample, names_to = "Experiment", values_to = "Pvalue")

  Sample_n <- cor_out_r$Sample %>% unique() %>% length()
  Sample_n_2 <- dim(Experiment)[2]

  cor_out_stat <- cor_out_r %>%
    dplyr::left_join(cor_out_p, by = c("Sample", "Experiment")) %>%
    dplyr::mutate(p_value = case_when(
      Pvalue > 0.05 ~ "P > 0.05",
      Pvalue > 0.01 & Pvalue < 0.05 ~ "0.01 < P < 0.05",
      Pvalue < 0.01 & Pvalue > 0.001 ~ "0.001 < P < 0.01",
      Pvalue < 0.001 ~ "P < 0.001"
    )) %>%
    dplyr::mutate(
      p_start1 = rep(1:Sample_n, each = Sample_n_2),
      p_end1 = rep(rev(0:(Sample_n-1)), each = Sample_n_2),
      start2 = rep(seq(1,Sample_n, 2)[1:Sample_n_2], times = Sample_n) - 4,
      end2 = rev(rep(seq(1,Sample_n, 2)[1:Sample_n_2], times = Sample_n))
    )

  ####----Plot----####
  cor_self <- cor_out_self$r %>% as.data.frame()
  cor_self[upper.tri(cor_self)] <- NA

  cor_self <- cor_self %>%
    tibble::rownames_to_column(var = "ID") %>%
    tidyr::pivot_longer(cols = -ID,
                        names_to = "Type",
                        values_to = "Value") %>%
    dplyr::mutate(ID = factor(ID, levels = unique(ID), ordered = T),
                  Type = factor(Type, levels = rev(unique(Type)), ordered = T),
                  ID2 = as.numeric(ID),
                  Type2 = as.numeric(Type)
    ) %>%
    na.omit()

  cor_self_p <- cor_out_self$p %>% as.data.frame()
  cor_self_p[upper.tri(cor_self_p)] <- NA
  cor_self_p <- cor_self_p %>%
    tibble::rownames_to_column(var = "ID") %>%
    tidyr::pivot_longer(cols = -ID,
                        names_to = "Type",
                        values_to = "Pvalue") %>%
    dplyr::mutate(ID = factor(ID, levels = unique(ID), ordered = T),
                  Type = factor(Type, levels = rev(unique(Type)), ordered = T),
                  ID2 = as.numeric(ID),
                  Type2 = as.numeric(Type)) %>%
    na.omit() %>%
    dplyr::mutate(p_value = case_when(
      Pvalue > 0.05 ~ "",
      Pvalue > 0.01 & Pvalue < 0.05 ~ "*",
      Pvalue < 0.01 & Pvalue > 0.001 ~ "**",
      Pvalue < 0.001 ~ "***"
    ))

  return(list(cor_self, cor_self_p, cor_out_stat))
}


create_layout2 <- function(graph, stat_out = stat_out, hub_names = NULL, r = 10) {
  # graph = graph_obj
  # r = 10
  hub_names = NULL
  # get nodes
  # 自定义布局
  # 构建自定义布局

  nodes <- graph %>%
    tidygraph::activate(nodes) %>%
    tidygraph::as_tibble()

  # get edges
  edges <- graph %>%
    tidygraph::activate(edges) %>%
    tidygraph::as_tibble()

  # 如果未指定 hub，自动选择度数最高的前两个节点
  if (is.null(hub_names)) {
    deg_df <- graph %>%
      tidygraph::mutate(degree = tidygraph::centrality_degree(mode = "out")) %>%
      tidygraph::as_tibble() %>%
      tidygraph::arrange(dplyr::desc(degree))

    hub_names <- deg_df$node[1:5]
  }

  # hub
  hub_names

  # non hub
  non_hub_names <- nodes$node[!nodes$node %in% hub_names]

  # 设置外圈的点
  n_points <- length(non_hub_names)
  radius <- r
  center_x <- -12
  center_y <- 4

  # 计算每一个点的角度
  angles <- seq(0, 2*pi, length.out = n_points + 1)[-(n_points+1)]

  # 计算坐标
  x <- center_x + radius * cos(angles)
  y <- center_y + radius * sin(angles)

  # 创建数据框

  circle_df <- data.frame(
    id = 1:n_points,
    x = x,
    y = y
  )

  circle_df

  hub_df <- stat_out[[3]] %>%
    dplyr::distinct(., Experiment, .keep_all = T) %>%
    dplyr::select(start2, end2) %>%
    purrr::set_names(c("x", "y"))


  # 自定一个布局
  layout_manual <- ggraph::create_layout(graph, layout = "circle")

  layout_manual_2 <- rbind(layout_manual[1:n_points,] %>%
                             dplyr::mutate(x = circle_df$x,
                                           y = circle_df$y),
                           layout_manual[-c(1:n_points),] %>%
                             dplyr::mutate(x = hub_df$x,
                                           y = hub_df$y)
  )

  ly <- ggraph::create_layout(graph,
                      layout = "manual",
                      x = layout_manual_2$x,
                      y = layout_manual_2$y)

  return(ly)
}
