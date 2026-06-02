# `ggnetview_zipi()` 使用指南

## 这个功能是做什么的

`ggnetview_zipi()` 计算模块化网络中每个节点的两个指标，并据此把节点分成四类拓扑角色（Guimerà & Amaral, 2005）：

- **Zi（within-module connectivity，模块内连通度）**：节点在自己所属模块内连接的强弱。Zi 高 = 该节点在模块内部是核心。
- **Pi（among-module connectivity / participation coefficient，模块间连通度）**：节点跨模块连接的程度。Pi 高 = 该节点像桥梁，连接多个模块。

按默认阈值 `Zi = 2.5`、`Pi = 0.62` 划分四个象限：

| 角色 | Zi | Pi | 含义 |
|------|----|----|------|
| **Peripherals**（外围节点） | 低 | 低 | 模块内外连接都少，边缘节点 |
| **Module hubs**（模块枢纽） | 高 | 低 | 模块内的核心，但不太跨模块 |
| **Connectors**（连接者） | 低 | 高 | 模块内不突出，但跨模块连接强，充当桥梁 |
| **Network hubs**（网络枢纽） | 高 | 高 | 模块内外都是核心，对整体网络最关键 |

## 函数签名

```r
ggnetview_zipi(
  nodes_bulk,            # 节点表（data.frame / tibble）
  z_bulk_mat,            # 邻接或相关矩阵（matrix）
  modularity_col,        # 模块标签所在的列名
  degree_col,            # 节点度数所在的列名
  zi_threshold = 2.5,    # Zi 阈值
  pi_threshold = 0.62,   # Pi 阈值
  na.rm = FALSE          # 是否丢弃 Zi/Pi 为 NA 的行
)
```

### 输入要求（容易踩坑的地方）

- **`nodes_bulk`**：必须把节点 ID 放在 `rownames` 里，**或者**有一列叫 `name`（`tidygraph::as_tibble()` 的默认输出就符合）。必须包含 `modularity_col` 和 `degree_col` 两列，且这两列**不能有 NA**。
- **`z_bulk_mat`**：必须有 `rownames` 和 `colnames`，且 `nodes_bulk` 的节点 ID 要是矩阵行名的子集。矩阵里非 0 的元素被当作一条边，`NA`/`Inf` 会被自动替换为 0。
- 两个输入的节点会按 ID 自动对齐，不需要你手动排序。

### 返回值

返回一个 **list**，有两个元素：

- **`$data`**：在原 `nodes_bulk` 基础上新增三列 —— `within_module_connectivities`（Zi）、`among_module_connectivities`（Pi）、`type`（角色）。
- **`$plot`**：一个 ggplot 对象，画好的 Zi-Pi 四象限散点图（带背景分区和象限标签）。

## 最简单的完整示例

用包自带的示例数据，从头到尾跑一遍：

```r
library(ggNetView)

# 1. 准备数据并建图
data(otu_rare_relative)
data(tax_tab)

mat <- as.matrix(otu_rare_relative)
mat <- mat[order(rowSums(mat), decreasing = TRUE)[seq_len(50)], ]  # 取前 50 个最丰富的 OTU

obj <- build_graph_from_mat(
  mat           = mat,
  method        = "cor",
  cor.method    = "spearman",
  proc          = "BH",
  r.threshold   = 0.5,
  p.threshold   = 0.05,
  module.method = "Fast_greedy",
  seed          = 1
)
# 建出来的图节点表已经带有 Degree、Strength、Modularity 三列

# 2. 取出 Zi-Pi 需要的两个输入
nodes_bulk <- get_graph_nodes(obj)        # 节点表
adj_mat    <- get_graph_adjacency(obj)    # 邻接矩阵

# 3. 计算 Zi-Pi 并分类
zp <- ggnetview_zipi(
  nodes_bulk     = nodes_bulk,
  z_bulk_mat     = adj_mat,
  modularity_col = "Modularity",
  degree_col     = "Degree"
)

# 4. 看结果
zp$plot                  # 四象限图
head(zp$data)            # 带 Zi、Pi、type 的节点表
```

## 常见用法

### 看每类角色有哪些节点

```r
# 所有 Network hubs（最关键的节点）
subset(zp$data, type == "Network hubs", select = c(name, type,
       within_module_connectivities, among_module_connectivities))

# 各角色数量统计
table(zp$data$type, useNA = "ifany")
```

### 调整阈值

不同研究领域对枢纽的定义不一样，可以改阈值重新分类：

```r
zp2 <- ggnetview_zipi(
  nodes_bulk, adj_mat, "Modularity", "Degree",
  zi_threshold = 2.0,   # 放宽 Zi 阈值
  pi_threshold = 0.60
)
```

### 丢掉无法分类的节点

单成员模块（模块里只有 1 个节点）的 Zi 算不出来，默认其 `type` 为 `NA`。想直接剔除：

```r
zp3 <- ggnetview_zipi(nodes_bulk, adj_mat, "Modularity", "Degree",
                      na.rm = TRUE)
```

### 自定义出图

`$plot` 是标准 ggplot 对象，可以叠加修改：

```r
library(ggplot2)
zp$plot +
  labs(title = "我的网络 Zi-Pi 角色图") +
  theme(legend.position = "bottom")

# 保存
ggsave("zipi_plot.pdf", zp$plot, width = 6, height = 6)
```

## 和 IVI / 中心性结合使用

Zi-Pi 给的是「拓扑角色」（节点扮演什么角色），IVI / 中心性给的是「影响力排名」（节点有多重要），两者互补。推荐先用 `get_node_centrality()` 和 `get_node_ivi()` 算影响力，再用 `ggnetview_zipi()` 看角色——一个 IVI 排名靠前、同时落在 **Network hubs** 象限的节点，是最值得做后续生物学验证的候选。

## 参考文献

Guimerà R, Amaral LAN (2005). "Functional cartography of complex metabolic networks." *Nature* 433(7028):895-900.
