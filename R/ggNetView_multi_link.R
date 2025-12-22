ggNetView_multi_link <- function(mat,
                                 group_info,
                                 transfrom.method = c("none", "scale", "center", "log2", "log10", "ln", "rrarefy", "rrarefy_relative"),
                                 r.threshold = 0.7,
                                 p.threshold = 0.05,
                                 method = c("WGCNA", "SpiecEasi", "SPARCC", "cor"),
                                 cor.method = c("pearson", "kendall", "spearman"),
                                 proc = c("Bonferroni", "Holm", "Hochberg", "SidakSS", "SidakSD", "BH", "BY", "ABH", "TSBH"),
                                 module.method = c("Fast_greedy", "Walktrap", "Edge_betweenness", "Spinglass"),
                                 SpiecEasi.method = c("mb", "glasso"),
                                 node_annotation = NULL,
                                 top_modules = 15,
                                 select_modules = 8,
                                 layout = NULL,
                                 node_add = 7,
                                 ring_n = NULL,
                                 r = 1,
                                 center = TRUE,
                                 idx = NULL,
                                 shrink = 1,
                                 k_nn = 8,
                                 push_others_delta = 0,
                                 layout.module = c("random", "adjacent", "order"),
                                 shape = 21,
                                 pointalpha = 1,
                                 pointsize = c(1,10),
                                 pointstroke = 0.3,
                                 group.by = "Modularity",
                                 fill.by = "Modularity",
                                 jitter = FALSE,
                                 jitter_sd = 0.1,
                                 mapping_line = FALSE,
                                 curve = F,
                                 curvature = 0.25,
                                 linealpha = 0.25,
                                 linecolor = "grey70",
                                 label = FALSE,
                                 labelsize = 10,
                                 labelsegmentsize = 1,
                                 labelsegmentalpha = 1,
                                 add_outer = FALSE,
                                 outerwidth = 1.25,
                                 outerlinetype = 2,
                                 outeralpha = 0.5,
                                 nodelabsize = 5,
                                 remove = FALSE,
                                 orientation = "up",
                                 angle = 0,
                                 scale = T,
                                 anchor_dist = 6,
                                 seed = 1115,
                                 nrow = NULL
){
  # 参数
  group_info <- otu_sample

  mat = otu_rare_relative

  transfrom.method = "none"
  r.threshold = 0.7
  p.threshold = 0.05
  method = "WGCNA"
  cor.method = "pearson"
  proc = "BH"
  module.method = "Fast_greedy"
  SpiecEasi.method = "mb"
  node_annotation = tax_tab
  top_modules = 15
  layout = "gephi"
  node_add = 7
  ring_n = NULL
  r = 1
  center = TRUE
  idx = NULL
  shrink = 0.5
  k_nn = 8
  push_others_delta = 0
  layout.module = "adjacent"
  shape = 21
  pointalpha = 1
  pointsize = c(1,10)
  pointstroke = 0.3
  group.by = "Modularity"
  fill.by = "Modularity"
  jitter = T
  jitter_sd = 0.3
  mapping_line = FALSE
  curve = F
  curvature = 0.25
  linealpha = 0.25
  linecolor = "grey70"
  label = FALSE
  labelsize = 10
  labelsegmentsize = 1
  labelsegmentalpha = 1
  add_outer = FALSE
  outerwidth = 1.25
  outerlinetype = 2
  outeralpha = 0.5
  nodelabsize = 5
  remove = FALSE
  orientation = "up"
  angle = 0
  scale = T
  anchor_dist = 6
  seed = 1115
  select_modules = 8


  graph_list <- list()

  graph_info <- list()

  for (g in unique(group_info$Group)) {
    print(g)
    group_info_sub <- group_info %>%
      dplyr::filter(Group %in% g)

    mat_sub <- mat %>%
      dplyr::select(all_of(group_info_sub$Sample)) %>%
      tibble::rownames_to_column(var = "ID") %>%
      dplyr::rowwise() %>%
      dplyr::mutate(sum = sum(dplyr::c_across(where(is.numeric)))) %>%
      dplyr::ungroup() %>%
      dplyr::filter(sum != 0) %>%
      dplyr::select(-sum) %>%
      tibble::column_to_rownames(var = "ID")

    graph <- build_graph_from_mat(
      mat = mat_sub,
      transfrom.method = transfrom.method,
      r.threshold = r.threshold,
      p.threshold = p.threshold,
      method = method,
      cor.method = cor.method,
      proc = proc,
      module.method = module.method,
      SpiecEasi.method = SpiecEasi.method,
      node_annotation = node_annotation,
      top_modules = top_modules,
      seed = seed
    )

    # 添加布局
    # find layout function
    func_name <- paste0("create_layout_", layout)

    # find layout functions from ggNetView package
    lay_func <- utils::getFromNamespace(func_name, "ggNetView")

    # get ly1
    ly1 = lay_func(graph_obj = graph,
                   node_add = node_add,
                   r = r,
                   scale = scale,
                   anchor_dist = anchor_dist,
                   orientation = orientation,
                   angle = angle)

    # get ly1_1
    # 圆形布局 添加模块化 获取模块
    if (layout.module == "random") {
      ly1_1 <- module_layout(graph,
                             layout = ly1,
                             center = center,
                             idx = idx,
                             shrink = shrink,
                             jitter,
                             jitter_sd# ,
                             # seed = seed
      )
    }

    if (layout.module == "adjacent") {
      ly1_1 <- module_layout3(graph,
                              layout = ly1,
                              center = center,
                              k_nn = k_nn,
                              push_others_delta = push_others_delta,
                              shrink = shrink,
                              jitter,
                              jitter_sd
                              # seed = seed
      )
    }

    if (layout.module == "order" & func_name != "create_layout_rings") {
      ly1_1 <- module_layout4(graph,
                              layout = ly1,
                              center = center,
                              k_nn = k_nn,
                              push_others_delta = push_others_delta,
                              shrink = shrink,
                              jitter,
                              jitter_sd
                              # seed = seed
      )
    }

    if (layout.module == "order" & func_name == "create_layout_multirings") {
      ly1_1 <- module_layout5(graph,
                              layout = ly1,
                              center = center,
                              k_nn = k_nn,
                              push_others_delta = push_others_delta,
                              shrink = shrink,
                              jitter,
                              jitter_sd
                              # seed = seed
      )
    }

    graph_list[[g]] <- graph

    graph_info[[g]] <- ly1_1$ggplot_data
  }

  graph_list_length <- length(graph_list)
  graph_info_length <- length(graph_info)


  # ggNetView(
  #   graph_obj = graph_list[[1]],
  #   layout = "gephi",
  #   group.by = "Modularity",
  #   fill.by = "Phylum",
  #   layout.module = "random"
  # )


  # # 在这里搜索一下三个组共有的ASV
  # insert_name <- Reduce(
  #   intersect,
  #   lapply(unique(group_info$Group), function(x){
  #     graph_info[[x]]$ggplot_node_df$name
  #   })
  # )
  #
  # # 基于这个我们再探索一下这些共有的ID，隶属于哪个模块
  # insert_module <- Reduce(
  #   intersect,
  #   purrr::map(unique(group_info$Group),
  #              function(x){
  #                top_module_tmp <- graph_list[[x]] %>%
  #                  tidygraph::activate(nodes) %>%
  #                  tidygraph::as_tibble() %>%
  #                  tidygraph::pull(Modularity) %>%
  #                  as.character() %>%
  #                  table() %>%
  #                  sort(., decreasing = T) %>%
  #                  .[names(.) != "Others"] %>%
  #                  .[1:select_modules] %>%
  #                  names()
  #
  #                group_top_module_tmp <- graph_list[[x]] %>%
  #                  tidygraph::activate(nodes) %>%
  #                  tidygraph::as_tibble() %>%
  #                  tidygraph::filter(name %in% insert_name) %>%
  #                  tidygraph::pull(Modularity) %>%
  #                  as.character() %>%
  #                  table() %>%
  #                  sort(., decreasing = T) %>%
  #                  .[names(.) != "Others"] %>%
  #                  .[1:select_modules] %>%
  #                  names()
  #                intersect(top_module_tmp,
  #                          group_top_module_tmp)
  #              })
  # )
  #
  # insert_module
  #
  #
  # tmp_insert_name_df <- dplyr::bind_rows(
  #   graph_info[["KO"]]$ggplot_node_df %>%
  #     dplyr::filter(name %in% insert_name) %>%
  #     dplyr::mutate(Group = "KO") %>%
  #     dplyr::select(name, Modularity, Group) %>%
  #     dplyr::mutate(Modularity = as.character(Modularity)) %>%
  #     dplyr::filter(Modularity != "Others"),
  #   graph_info[["OE"]]$ggplot_node_df %>%
  #     dplyr::filter(name %in% insert_name) %>%
  #     dplyr::mutate(Group = "OE") %>%
  #     dplyr::select(name, Modularity, Group) %>%
  #     dplyr::mutate(Modularity = as.character(Modularity)) %>%
  #     dplyr::filter(Modularity != "Others"),
  #   graph_info[["WT"]]$ggplot_node_df %>%
  #     dplyr::filter(name %in% insert_name) %>%
  #     dplyr::mutate(Group = "WT") %>%
  #     dplyr::select(name, Modularity, Group) %>%
  #     dplyr::mutate(Modularity = as.character(Modularity)) %>%
  #     dplyr::filter(Modularity != "Others")
  # )
  #
  # table(tmp_insert_name_df$name,
  #       tmp_insert_name_df$Group)
  #
  # tmp_insert_name_df %>%
  #   dplyr::mutate(info = str_c(Group, Modularity, sep = "_")) %>%
  #   dplyr::group_by(name) %>%
  #   dplyr::mutate(tmp = str_c(info, collapse = ";")) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::mutate(number = str_count(tmp, pattern = ";")) %>%
  #   dplyr::filter(number != 0) %>%
  #   dplyr::arrange(tmp) %>%
  #   View()


  # graph_info[["KO"]]$ggplot_node_df %>%
  #   dplyr::select(name, Modularity) %>%
  #   dplyr::mutate(Group = "KO") %>%
  #   write.csv(file = "/Users/liuyue/Desktop/tmp/KO_info.csv",
  #             quote = F,
  #             row.names = F)
  #
  # graph_info[["OE"]]$ggplot_node_df %>%
  #   dplyr::select(name, Modularity) %>%
  #   dplyr::mutate(Group = "OE") %>%
  #   write.csv(file = "/Users/liuyue/Desktop/tmp/OE_info.csv",
  #             quote = F,
  #             row.names = F)
  #
  # graph_info[["WT"]]$ggplot_node_df %>%
  #   dplyr::select(name, Modularity) %>%
  #   dplyr::mutate(Group = "WT") %>%
  #   write.csv(file = "/Users/liuyue/Desktop/tmp/WT_info.csv",
  #             quote = F,
  #             row.names = F)

  # graph_info[["WT"]]$ggplot_node_df %>%
  #     dplyr::select(name, Modularity) %>%
  #     dplyr::mutate(Group = "WT") %>%
  #   dplyr::pull(Modularity) %>%
  #   table() %>%
  #   sort(., decreasing = T)
  #
  # graph_info[["OE"]]$ggplot_node_df %>%
  #   dplyr::select(name, Modularity) %>%
  #   dplyr::mutate(Group = "OE") %>%
  #   dplyr::pull(Modularity) %>%
  #   table() %>%
  #   sort(., decreasing = T)

  # WT vs OE
  WT_OE <- compare_modules_by_overlap(graph_info[["WT"]]$ggplot_node_df %>%
                                        dplyr::select(name, Modularity) %>%
                                        dplyr::mutate(Group = "WT"),
                                      graph_info[["OE"]]$ggplot_node_df %>%
                                        dplyr::select(name, Modularity) %>%
                                        dplyr::mutate(Group = "OE"))
  WT_OE %>%
    dplyr::filter(pvalue < 0.05) %>%
    dplyr::mutate(Group = str_c("WT", "to", "OE", sep = "_"))

  # WT vs KO
  WT_KO <- compare_modules_by_overlap(graph_info[["WT"]]$ggplot_node_df %>%
                                        dplyr::select(name, Modularity) %>%
                                        dplyr::mutate(Group = "WT"),
                                      graph_info[["KO"]]$ggplot_node_df %>%
                                        dplyr::select(name, Modularity) %>%
                                        dplyr::mutate(Group = "KO"))

  WT_KO %>%
    dplyr::filter(pvalue < 0.05) %>%
    dplyr::mutate(Group = str_c("WT", "to", "KO", sep = "_"))

  # KO vs OE
  KO_OE <- compare_modules_by_overlap(graph_info[["KO"]]$ggplot_node_df %>%
                                        dplyr::select(name, Modularity) %>%
                                        dplyr::mutate(Group = "KO"),
                                      graph_info[["OE"]]$ggplot_node_df %>%
                                        dplyr::select(name, Modularity) %>%
                                        dplyr::mutate(Group = "OE"))
  KO_OE %>%
    dplyr::filter(pvalue < 0.05) %>%
    dplyr::mutate(Group = str_c("KO", "to", "OE", sep = "_"))

  Module_information <- dplyr::bind_rows(
    WT_OE %>%
      dplyr::filter(pvalue < 0.05) %>%
      dplyr::mutate(Group = str_c("WT", "to", "OE", sep = "_")),
    WT_KO %>%
      dplyr::filter(pvalue < 0.05) %>%
      dplyr::mutate(Group = str_c("WT", "to", "KO", sep = "_")),
    KO_OE %>%
      dplyr::filter(pvalue < 0.05) %>%
      dplyr::mutate(Group = str_c("KO", "to", "OE", sep = "_"))
  )

  Module_information

  # 那么可以直接进行可视化了
  # raw data to plot data

  ggplot() +
    # WT point
    geom_jitter(data = graph_info[["WT"]]$ggplot_node_df,
                mapping = aes(x = x + 0, y = y - 0, fill = Modularity), shape = 21,
                position = position_jitter(width = jitter_sd, height = jitter_sd)) +
    ggnewscale::new_scale_fill() +
    ggforce::geom_mark_circle(data = graph_info[["WT"]]$ggplot_node_df %>%
                                dplyr::filter(name %in% (graph_list[["WT"]] %>%
                                                           tidygraph::activate(nodes) %>%
                                                           tidygraph::as_tibble() %>%
                                                           dplyr::mutate(Modularity = as.character(Modularity)) %>%
                                                           dplyr::filter(Modularity %in% (Module_information %>%
                                                                                            dplyr::filter(str_starts(Group, pattern = "WT")) %>%
                                                                                            dplyr::pull(modA) %>%
                                                                                            unique() %>%
                                                                                            .[.!= "Others"])
                                                           ) %>%
                                                           dplyr::pull(name))),
                              mapping = aes(x = x + 0, y = y + 0, fill = Modularity),
                              n = 100,
                              expand = unit(1, "mm")
    ) +
    # WT to OE segment
    ggplot2::geom_segment(data = Module_information %>%
                            dplyr::filter(Group == "WT_to_OE") %>%
                            dplyr::select(1,2) %>%
                            # WT to OE, WT center point
                            dplyr::left_join(graph_info[["WT"]]$ggplot_node_df %>%
                                               dplyr::mutate(Modularity = as.character(Modularity)) %>%
                                               dplyr::filter(Modularity%in% (Module_information %>%
                                                                               dplyr::filter(Group == "WT_to_OE") %>%
                                                                               dplyr::pull(modA) %>%
                                                                               unique())) %>%
                                               dplyr::group_by(Modularity) %>%
                                               dplyr::summarise(x_center = mean(x),
                                                                y_center = mean(y)) %>%
                                               purrr::set_names(c("WT_Module", "WT_x_center", "WT_y_center")),
                                             by = c("modA" = "WT_Module")) %>%
                            # WT to OE, OE center point
                            dplyr::left_join(graph_info[["OE"]]$ggplot_node_df %>%
                                               dplyr::mutate(Modularity = as.character(Modularity)) %>%
                                               dplyr::filter(Modularity%in% (Module_information %>%
                                                                               dplyr::filter(Group == "WT_to_OE") %>%
                                                                               dplyr::pull(modB) %>%
                                                                               unique())) %>%
                                               dplyr::group_by(Modularity) %>%
                                               dplyr::summarise(x_center = mean(x)-30,
                                                                y_center = mean(y)+30) %>%
                                               purrr::set_names(c("OE_Module", "OE_x_center", "OE_y_center")),
                                             by = c("modB" = "OE_Module")
                            ),
                          mapping = aes(x = WT_x_center, xend = OE_x_center,
                                        y = WT_y_center, yend = OE_y_center),
                          linetype = 2,
                          linewidth = 1,
                          color = "#c51b7d") +
    # WT to KO segment
    ggplot2::geom_segment(data = Module_information %>%
                            dplyr::filter(Group == "WT_to_KO") %>%
                            dplyr::select(1,2) %>%
                            # WT to OE, WT center point
                            dplyr::left_join(graph_info[["WT"]]$ggplot_node_df %>%
                                               dplyr::mutate(Modularity = as.character(Modularity)) %>%
                                               dplyr::filter(Modularity%in% (Module_information %>%
                                                                               dplyr::filter(Group == "WT_to_KO") %>%
                                                                               dplyr::pull(modA) %>%
                                                                               unique())) %>%
                                               dplyr::group_by(Modularity) %>%
                                               dplyr::summarise(x_center = mean(x),
                                                                y_center = mean(y)) %>%
                                               purrr::set_names(c("WT_Module", "WT_x_center", "WT_y_center")),
                                             by = c("modA" = "WT_Module")) %>%
                            # WT to OE, OE center point
                            dplyr::left_join(graph_info[["KO"]]$ggplot_node_df %>%
                                               dplyr::mutate(Modularity = as.character(Modularity)) %>%
                                               dplyr::filter(Modularity%in% (Module_information %>%
                                                                               dplyr::filter(Group == "WT_to_KO") %>%
                                                                               dplyr::pull(modB) %>%
                                                                               unique())) %>%
                                               dplyr::group_by(Modularity) %>%
                                               dplyr::summarise(x_center = mean(x)+30,
                                                                y_center = mean(y)+30) %>%
                                               purrr::set_names(c("KO_Module", "KO_x_center", "KO_y_center")),
                                             by = c("modB" = "KO_Module")
                            ),
                          mapping = aes(x = WT_x_center, xend = KO_x_center,
                                        y = WT_y_center, yend = KO_y_center),
                          linetype = 2,
                          linewidth = 1,
                          color = "#7fbc41") +
    # OE point
    ggnewscale::new_scale_fill() +
    geom_jitter(data = graph_info[["OE"]]$ggplot_node_df,
                mapping = aes(x = x - 30, y = y + 30,  fill = Modularity), shape = 21,
                position = position_jitter(width = jitter_sd, height = jitter_sd)) +
    ggnewscale::new_scale_fill() +
    ggforce::geom_mark_circle(data = graph_info[["OE"]]$ggplot_node_df %>%
                                dplyr::filter(name %in% (graph_list[["OE"]] %>%
                                                           tidygraph::activate(nodes) %>%
                                                           tidygraph::as_tibble() %>%
                                                           dplyr::mutate(Modularity = as.character(Modularity)) %>%
                                                           dplyr::filter(Modularity %in% (Module_information %>%
                                                                                            dplyr::filter(str_ends(Group, pattern = "OE")) %>%
                                                                                            dplyr::pull(modB) %>%
                                                                                            unique() %>%
                                                                                            .[.!= "Others"])
                                                           ) %>%
                                                           dplyr::pull(name))),
                              mapping = aes(x = x - 30, y = y + 30, fill = Modularity),
                              n = 100,
                              expand = unit(1, "mm")
    ) +

    # KO point
    ggnewscale::new_scale_fill() +
    geom_jitter(data = graph_info[["KO"]]$ggplot_node_df,
                mapping = aes(x = x + 30, y = y + 30, fill = Modularity), shape = 21,
                position = position_jitter(width = jitter_sd, height = jitter_sd)) +
    ggnewscale::new_scale_fill() +
    ggforce::geom_mark_circle(data = graph_info[["KO"]]$ggplot_node_df %>%
                                dplyr::filter(name %in% (graph_list[["KO"]] %>%
                                                           tidygraph::activate(nodes) %>%
                                                           tidygraph::as_tibble() %>%
                                                           dplyr::mutate(Modularity = as.character(Modularity)) %>%
                                                           dplyr::filter(Modularity %in% (c(Module_information %>%
                                                                                              dplyr::filter(str_ends(Group, pattern = "KO")) %>%
                                                                                              dplyr::pull(modB),
                                                                                            Module_information %>%
                                                                                              dplyr::filter(str_starts(Group, pattern = "KO")) %>%
                                                                                              dplyr::pull(modA)) %>%
                                                                                            unique() %>%
                                                                                            .[.!= "Others"])
                                                           ) %>%
                                                           dplyr::pull(name))),
                              mapping = aes(x = x + 30, y = y + 30, fill = Modularity),
                              n = 100,
                              expand = unit(1, "mm")
    ) +

    # KO to OE segment
    ggplot2::geom_segment(data = Module_information %>%
                            dplyr::filter(Group == "KO_to_OE") %>%
                            dplyr::select(1,2) %>%
                            dplyr::filter(modA!= "Others" & modB!= "Others") %>%
                            # WT to OE, WT center point
                            dplyr::left_join(graph_info[["KO"]]$ggplot_node_df %>%
                                               dplyr::mutate(Modularity = as.character(Modularity)) %>%
                                               dplyr::filter(Modularity%in% (Module_information %>%
                                                                               dplyr::filter(Group == "KO_to_OE") %>%
                                                                               dplyr::pull(modA) %>%
                                                                               unique())) %>%
                                               dplyr::group_by(Modularity) %>%
                                               dplyr::summarise(x_center = mean(x)+30,
                                                                y_center = mean(y)+30) %>%
                                               purrr::set_names(c("KO_Module", "KO_x_center", "KO_y_center")),
                                             by = c("modA" = "KO_Module")) %>%
                            # WT to OE, OE center point
                            dplyr::left_join(graph_info[["OE"]]$ggplot_node_df %>%
                                               dplyr::mutate(Modularity = as.character(Modularity)) %>%
                                               dplyr::filter(Modularity%in% (Module_information %>%
                                                                               dplyr::filter(Group == "KO_to_OE") %>%
                                                                               dplyr::pull(modB) %>%
                                                                               unique())) %>%
                                               dplyr::group_by(Modularity) %>%
                                               dplyr::summarise(x_center = mean(x)-30,
                                                                y_center = mean(y)+30) %>%
                                               purrr::set_names(c("OE_Module", "OE_x_center", "OE_y_center")),
                                             by = c("modB" = "OE_Module")
                            ),
                          mapping = aes(x = KO_x_center, xend = OE_x_center,
                                        y = KO_y_center, yend = OE_y_center),
                          linetype = 2,
                          linewidth = 1,
                          color = "#74add1") +

    coord_fixed() +
    theme_ggnetview()









  # scaled data to plot




  # 主要是解析一下不同分组之间是否是有相同的变量，然后以后留着进行link的

  # test1 这种情况是仅仅做了平移，但是没有做标准化到统一尺度
  tmp_df <- dplyr::bind_rows(
    graph_info[["KO"]]$ggplot_node_df %>% dplyr::mutate(Group = "KO") %>% dplyr::select(name, x, y, Group) %>% dplyr::mutate(x = x + 30, y = y + 30),
    graph_info[["OE"]]$ggplot_node_df %>% dplyr::mutate(Group = "OE") %>% dplyr::select(name, x, y, Group) %>% dplyr::mutate(x = x - 30, y = y + 30),
    graph_info[["WT"]]$ggplot_node_df %>% dplyr::mutate(Group = "WT") %>% dplyr::select(name, x, y, Group) %>% dplyr::mutate(x = x + 0, y = y - 0),
  )

  # test 1
  ggplot(data = tmp_df) +
    geom_jitter(aes(x = x, y = y, color = Group), alpha = 1, position = position_jitter(width = jitter_sd, height = jitter_sd)) +
    facet_wrap(~Group, scale = "free")

  ggplot(data = tmp_df) +
    geom_jitter(aes(x = x, y = y, color = Group), alpha = 1) +
    coord_fixed() +
    theme_ggnetview()


  ggplot() +
    geom_jitter(data = graph_info[["KO"]]$ggplot_node_df,
               mapping = aes(x = x + 30, y = y + 30, fill = Modularity), shape = 21, position = position_jitter(width = jitter_sd, height = jitter_sd)) +
    ggnewscale::new_scale_fill() +
    geom_jitter(data = graph_info[["OE"]]$ggplot_node_df,
               mapping = aes(x = x - 30, y = y + 30,  fill = Modularity), shape = 21, position = position_jitter(width = jitter_sd, height = jitter_sd)) +
    ggnewscale::new_scale_fill() +
    geom_jitter(data = graph_info[["WT"]]$ggplot_node_df,
               mapping = aes(x = x + 0, y = y - 0, fill = Modularity), shape = 21, position = position_jitter(width = jitter_sd, height = jitter_sd)) +
    coord_fixed() +
    theme_ggnetview()

  # test2 分别对每一个布局，进行标准化到同一尺度，然后再平移位置

  tmp_df2 <- dplyr::bind_rows(
    graph_info[["KO"]]$ggplot_node_df %>%
      dplyr::mutate(Group = "KO") %>%
      dplyr::select(name, x, y, Group) %>%
      dplyr::mutate(
        ymin = min(y),
        ymax = max(y),
        xmin = min(x),
        xmax = max(x),
        xmind = (max(x) + min(x)) / 2,
        ymind = (max(y) + min(y)) / 2,
        scale_v = max(xmax - xmin, ymax - ymin),
        x = (x - xmind)/scale_v,
        y = (y - xmind)/scale_v
      ),
    graph_info[["OE"]]$ggplot_node_df %>%
      dplyr::mutate(Group = "OE") %>%
      dplyr::select(name, x, y, Group) %>%
      dplyr::mutate(
        ymin = min(y),
        ymax = max(y),
        xmin = min(x),
        xmax = max(x),
        xmind = (max(x) + min(x)) / 2,
        ymind = (max(y) + min(y)) / 2,
        scale_v = max(xmax - xmin, ymax - ymin),
        x = (x - xmind)/scale_v,
        y = (y - xmind)/scale_v
      ),
    graph_info[["WT"]]$ggplot_node_df %>%
      dplyr::mutate(Group = "WT") %>%
      dplyr::select(name, x, y, Group) %>%
      dplyr::mutate(
        ymin = min(y),
        ymax = max(y),
        xmin = min(x),
        xmax = max(x),
        xmind = (max(x) + min(x)) / 2,
        ymind = (max(y) + min(y)) / 2,
        scale_v = max(xmax - xmin, ymax - ymin),
        x = (x - xmind)/scale_v,
        y = (y - xmind)/scale_v
      )
  )


  ggplot() +
    geom_point(data = tmp_df2 %>% dplyr::filter(Group == "KO"),
               mapping = aes(x = x + 1, y = y + 1), shape = 21) +
    ggnewscale::new_scale_fill() +
    geom_point(data = tmp_df2 %>% dplyr::filter(Group == "OE"),
               mapping = aes(x = x - 1, y = y + 1), shape = 21) +
    ggnewscale::new_scale_fill() +
    geom_point(data = tmp_df2 %>% dplyr::filter(Group == "WT"),
               mapping = aes(x = x + 0, y = y - 0), shape = 21) +
    coord_fixed() +
    theme_ggnetview()

  tmp_KO <- graph_info[["KO"]]$ggplot_node_df %>%
    dplyr::mutate(
      ymin = min(y),
      ymax = max(y),
      xmin = min(x),
      xmax = max(x),
      xmind = (max(x) + min(x)) / 2,
      ymind = (max(y) + min(y)) / 2,
      scale_v = max(xmax - xmin, ymax - ymin),
      x = (x - xmind)/scale_v,
      y = (y - xmind)/scale_v
    )

  ggplot() +
    geom_point(data = graph_info[["KO"]]$ggplot_node_df %>%
                 dplyr::mutate(
                   ymin = min(y),
                   ymax = max(y),
                   xmin = min(x),
                   xmax = max(x),
                   xmind = (max(x) + min(x)) / 2,
                   ymind = (max(y) + min(y)) / 2,
                   scale_v = max(xmax - xmin, ymax - ymin),
                   x = (x - xmind)/scale_v,
                   y = (y - xmind)/scale_v
                 ),
               mapping = aes(x = x + 1.5, y = y + 1.5, fill = Modularity), shape = 21,
               # position = position_jitter(width = 0.1, height = 0.1)
               ) +
    geom_segment(data = graph_info[["KO"]]$ggplot_edge_df %>%
                   dplyr::mutate(xmid = unique(tmp_KO$xmind),
                                 ymid = unique(tmp_KO$ymind),
                                 scale_v = unique(tmp_KO$scale_v),
                                 from_x = (from_x - xmid)/scale_v,
                                 from_y = (from_y - ymid)/scale_v,
                                 to_x = (to_x - xmid)/scale_v,
                                 to_y = (to_y - ymid)/scale_v),
                 mapping = aes(x = from_x + 1.5,
                               xend = to_x + 1.5,
                               y = from_y + 1.5,
                               yend = to_y + 1.5),
                 linewidth = 0.1) +
    ggnewscale::new_scale_fill() +
    geom_jitter(data = graph_info[["OE"]]$ggplot_node_df %>%
                 dplyr::mutate(
                   ymin = min(y),
                   ymax = max(y),
                   xmin = min(x),
                   xmax = max(x),
                   xmind = (max(x) + min(x)) / 2,
                   ymind = (max(y) + min(y)) / 2,
                   scale_v = max(xmax - xmin, ymax - ymin),
                   x = (x - xmind)/scale_v,
                   y = (y - xmind)/scale_v
                 ),
               mapping = aes(x = x - 1.5, y = y + 1.5,  fill = Modularity), shape = 21,
               position = position_jitter(width = 0.1, height = 0.1)) +
    ggnewscale::new_scale_fill() +
    geom_jitter(data = graph_info[["WT"]]$ggplot_node_df %>%
                 dplyr::mutate(
                   ymin = min(y),
                   ymax = max(y),
                   xmin = min(x),
                   xmax = max(x),
                   xmind = (max(x) + min(x)) / 2,
                   ymind = (max(y) + min(y)) / 2,
                   scale_v = max(xmax - xmin, ymax - ymin),
                   x = (x - xmind)/scale_v,
                   y = (y - xmind)/scale_v
                 ),
               mapping = aes(x = x + 0, y = y - 0, fill = Modularity), shape = 21,
               position = position_jitter(width = 0.1, height = 0.1)) +
    coord_fixed() +
    theme_ggnetview()

  tmp_df2$name[tmp_df2$name %>% table() >= 3]

  tmp_df2 %>%
    dplyr::filter(name %in% tmp_df2$name[tmp_df2$name %>% table() >= 3]) %>%
    dplyr::pull()

}
