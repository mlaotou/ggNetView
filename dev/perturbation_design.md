# ggNetView 网络虚拟扰动分析 —— 设计方案

> 目标：在不引入新建模框架的前提下，为 ggNetView 增加一套"虚拟扰动（virtual
> perturbation）"分析模块，覆盖用户提出的三类需求，并与现有
> `get_network_topology` / `get_node_ivi` / `get_node_centrality` /
> `ggnetview_zipi` 无缝衔接。所有随机过程可设种子，保持包的"可复现、确定性"定位。

---

## 1. 总体设计

新增三个核心函数 + 配套绘图，全部以 `tbl_graph`（任意 `build_graph_from_*()`
产物）为输入，复用其既有的边属性（`weight = |r|`、`correlation`、
`corr_direction`）和节点属性（`name`、`Modularity`、`Degree`、`Strength`）。

| 函数 | 对应需求 | 一句话功能 |
|------|----------|------------|
| `get_network_perturbation()` | 类型 1 结构性扰动 | 反复"敲掉"节点（随机/定向/指定/模块），重算拓扑，输出扰动曲线与鲁棒性指数 |
| `get_node_influence()` | 类型 2 影响传播 | 给源节点注入扰动，沿加权（带符号）边扩散，输出每个节点受影响程度 |
| `press_perturbation()` | 类型 3 打折版 | 把相关矩阵当群落矩阵代理，求逆得净效应矩阵，回答"持续压制/扶持某物种后全群落如何重排" |
| `ggnetview_perturbation_curve()` | 绘图 | 攻击曲线（指标 vs 移除比例），风格对齐 `ggnetview_zipi` |

---

## 2. 类型 1：结构性扰动 `get_network_perturbation()`

### 思路
"改动网络 → 重算拓扑 → 与原网络比较"。沿移除比例序列（默认
`seq(0.05, 1, 0.05)`）逐步删除节点，记录每一步的拓扑指标，形成曲线。

### 移除策略（`strategy`）
- `"random"`：随机移除，重复 `bootstrap` 次取均值/sd/se（复用现有
  Robustness 思路，可设 `seed`）。
- `"targeted"`：按某个中心性从高到低（或从低到高）定向攻击。
  `centrality` 取 `"degree" | "betweenness" | "closeness" | "eigenvector" |
  "strength" | "ivi"`。这是比随机移除更敏感的脆弱性探针。
- `"module"`：整模块敲除，`target` 指定 `Modularity` 的某个标签。
- `"manual"`：`target` 直接给节点 `name` 向量。

### 每步记录的指标
重点放在"扰动响应"最敏感的连通性指标，而非全套 24 项：

- **最大连通子图占比（LCC fraction）** —— 经典的攻击鲁棒性主曲线
- 连通分量数（n_components）
- 自然连通度 natural connectivity（特征值谱，抗噪稳健）
- 网络效率（复用现有 `.network.efficiency`）
- 平均度、密度、全局传递性、模块度

### 衍生标量
- **R-index（Schneider 等 2011）**：LCC 曲线下面积归一化，单值刻画整体鲁棒性，
  便于跨网络比较。
- 各策略的 AUC，方便"随机 vs 定向攻击"对比。

### 返回
`list(curve = <长表: strategy, fraction, metric, value, (mean/sd/se)>,`
` robustness_index = <data.frame>, plot = <ggplot 或 NULL>)`。

### 文献
Albert, Jeong & Barabási (2000) *Nature* 406:378（error/attack tolerance）；
Schneider et al. (2011) *PNAS* 108:3838（R-index）。

---

## 3. 类型 2：丰度影响传播 `get_node_influence()`

### 思路
把边权（可带 `correlation` 符号）当作"相互作用强度"，给 `source` 节点一个虚拟
扰动，沿边线性扩散，看波及范围。

### 方法
采用**带重启的随机游走 / Katz 型扩散**，保证收敛、定义良好：

```
影响向量  infl = (I - alpha * W_norm)^{-1} %*% s   (减去源自身)
```

- `W_norm`：行/列归一化的（带符号）邻接矩阵，符号来自 `correlation`。
- `alpha`：衰减系数（默认 0.5，需 < 1/谱半径 以收敛）。
- `s`：源节点处置 `delta`、其余 0 的初始向量。
- `signed = TRUE` 时保留正负，可区分"被促进/被抑制"。

### 返回
增广后的 `tbl_graph`，新增节点列 `Influence`（与 `get_node_ivi` 返回风格一致），
便于直接 `ggNetView(..., fill.by = "Influence")` 出图。

### 诚实边界（写进 roxygen `@details`）
相关网络是**关联非因果**，方向不可辨；本函数给出的是"结构可达性加权的影响力
估计"，是定性趋势，不能当真实生态动力学预测。

---

## 4. 类型 3 打折版：`press_perturbation()`

### 思路
经典 press perturbation（持续施压）框架：把相关矩阵当作"群落矩阵
（community matrix）A"的代理，净效应矩阵

```
N = -A^{-1}        N[i, j] = 物种 i 对"持续压制/扶持物种 j"的净响应
```

### 关键设置
- A 的非对角元 = 带符号相关 `correlation`（i 与 j 的相互作用代理）。
- 对角元 = `self_regulation`（自我调节，默认 −1，负值代表密度制约，保证系统稳定）。
- **稳定性检查**：计算 A 的特征值，要求实部全部 < 0（Lyapunov 稳定）；不满足时
  发警告并提示调大 `|self_regulation|`。
- 可选 `source`：只返回"压制某物种 → 其余物种响应"的一列。

### 返回
`list(net_effect = <矩阵>, stable = <logical>, eigen_real_max = <num>,`
` response = <可选 data.frame>, plot = <净效应热图，可选>)`。

### 诚实边界
相关 ≠ 因果、符号与方向近似、要求矩阵稳定且可逆；结果为定性趋势，可发表的
折中方案而非定量预测。

### 文献
Bender, Case & Gilpin (1984) *Ecology* 65:1（press perturbation）；
Novak et al. (2016) *Annu. Rev. Ecol. Evol. Syst.* 47:409（comparison of
press-perturbation predictions）；May (1972) *Nature* 238:413（稳定性）。

---

## 5. 绘图 `ggnetview_perturbation_curve()`
- 输入 `get_network_perturbation()` 的 `curve`。
- x = 移除比例，y = 选定指标（默认 LCC fraction），按 `strategy` 着色，随机策略
  画 mean ± se 带。
- `theme_classic()`、黑色坐标轴、`aspect.ratio = 1`，与 `ggnetview_zipi` 一致。

---

## 6. 与现有代码衔接
- 输入：任何 `build_graph_from_*()` 的 `tbl_graph`；不改动现有构图流程。
- 复用：`tidygraph::as.igraph`、内部效率/连通度计算思路、`get_node_ivi`
  （定向攻击的 ivi 选项）、`get_graph_adjacency`（带符号版自建）。
- 输出风格：节点级返回增广 `tbl_graph`（同 `get_node_ivi`）；网络级/矩阵级返回
  `list(data/curve/matrix, plot)`（同 `ggnetview_zipi`）。
- 可复现：所有随机过程接受 `seed`，内部 `set.seed()`。
- 导出：roxygen `@export` + 更新 `NAMESPACE`；交叉 `@seealso`。

---

## 7. 交付清单
1. `R/get_network_perturbation.R`
2. `R/get_node_influence.R`
3. `R/press_perturbation.R`
4. `R/ggnetview_perturbation_curve.R`
5. `NAMESPACE` 更新
6. `tests/testthat/` 测试
7. 核心数学在 toy 网络上的独立验证
