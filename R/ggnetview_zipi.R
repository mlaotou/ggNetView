#' Compute Zi-Pi (within-module connectivity and participation coefficient)
#'
#' Calculates the within-module degree z-score (Zi) and among-module connectivity
#' (participation coefficient, Pi) for each node in a modular network.
#' These metrics classify nodes into roles such as module hubs, connectors,
#' and peripherals.
#'
#' @param nodes_bulk Data frame or tibble.
#'   Node table with modularity and degree information.
#'   Node IDs must be in \code{rownames} or in a \code{name} column (compatible
#'   with \code{tidygraph::as_tibble} output).
#' @param z_bulk_mat Numeric matrix.
#'   Adjacency or correlation matrix; rows and columns must correspond to nodes.
#'   Non-zero entries are treated as edges. \code{NA}/\code{Inf} are replaced with 0.
#' @param modularity_col Character.
#'   Column name in \code{nodes_bulk} containing module labels.
#' @param degree_col Character.
#'   Column name in \code{nodes_bulk} containing node degree (number of edges).
#'
#' @returns A data frame merging \code{nodes_bulk} with two new columns:
#'   \itemize{
#'     \item \code{within_module_connectivities}: Zi, within-module degree z-score
#'     \item \code{among_module_connectivities}: Pi, participation coefficient (0–1)
#'   }
#'
#' @references
#'   Guimera R, Amaral LAN (2005). "Functional cartography of complex metabolic
#'   networks." \emph{Nature} 433(7028):895–900.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' g <- build_graph_from_mat(matrix(rnorm(200), 20, 10), method = "cor")
#' nodes_bulk <- get_graph_nodes(g)
#' adj_mat <- get_graph_adjacency(g)
#' ggnetview_zipi(nodes_bulk, adj_mat, "Modularity", "Degree")
#' }
ggnetview_zipi <- function(nodes_bulk, z_bulk_mat, modularity_col, degree_col) {
  if (!is.data.frame(nodes_bulk)) {
    stop("`nodes_bulk` must be a data frame or tibble.", call. = FALSE)
  }
  if (!is.matrix(z_bulk_mat) && !is.data.frame(z_bulk_mat)) {
    stop("`z_bulk_mat` must be a matrix or data frame.", call. = FALSE)
  }
  z_bulk_mat <- as.matrix(z_bulk_mat)
  if (is.null(rownames(z_bulk_mat)) || is.null(colnames(z_bulk_mat))) {
    stop("`z_bulk_mat` must have rownames and colnames (node IDs).", call. = FALSE)
  }

  # 支持节点 ID 来自 rownames 或 "name" 列（兼容 tidygraph 输出）
  rn <- rownames(nodes_bulk)
  if (is.null(rn) || length(rn) == 0L ||
      identical(rn, as.character(seq_len(nrow(nodes_bulk))))) {
    if ("name" %in% names(nodes_bulk)) {
      ids <- as.character(nodes_bulk[["name"]])
    } else {
      stop("nodes_bulk 需有 rownames 为节点 ID，或包含 \"name\" 列。")
    }
  } else {
    ids <- rn
  }
  if (length(ids) == 0L || any(is.na(ids)) || any(ids == "")) {
    stop("nodes_bulk 的节点 ID（rownames 或 name 列）不能为空或 NA。")
  }

  # —— 对齐：行名必须覆盖 nodes_bulk 的行名
  if (!all(ids %in% rownames(z_bulk_mat))) {
    stop("rownames(nodes_bulk) 必须是 z_bulk_mat 的子集并对齐。")
  }
  # 重排矩阵顺序以匹配 nodes_bulk
  z_bulk_mat <- z_bulk_mat[ids, ids, drop = FALSE]

  # —— 二值化 & 处理对角线
  if (any(!is.finite(z_bulk_mat))) {
    z_bulk_mat[!is.finite(z_bulk_mat)] <- 0
  }
  A <- (abs(z_bulk_mat) > 0) * 1L
  diag(A) <- 1L  # 确保自连为 1，便于后续 -1

  if (!modularity_col %in% names(nodes_bulk)) {
    stop(sprintf("nodes_bulk 中缺少列 \"%s\"。", modularity_col))
  }
  if (!degree_col %in% names(nodes_bulk)) {
    stop(sprintf("nodes_bulk 中缺少列 \"%s\"。", degree_col))
  }
  mod  <- nodes_bulk[[modularity_col]]
  deg  <- nodes_bulk[[degree_col]]

  # 安全性
  if (any(is.na(mod))) stop("模块列存在 NA。")
  if (any(is.na(deg))) stop("度列存在 NA。")

  # —— 计算 within-module degree z
  # 按模块拆分索引
  split_idx <- split(seq_along(ids), f = factor(mod, levels = unique(mod)))
  z_vec <- numeric(length(ids)); names(z_vec) <- ids

  for (lev in names(split_idx)) {
    idx <- split_idx[[lev]]
    if (length(idx) <= 1) {
      z_vec[idx] <- 0
      next
    }
    Aii <- A[idx, idx, drop = FALSE]
    k_in <- rowSums(Aii) - 1L
    sd_k <- stats::sd(k_in)
    if (sd_k == 0) z_vec[idx] <- 0 else z_vec[idx] <- (k_in - mean(k_in)) / sd_k
  }

  # —— 计算参与系数 P
  # k_is：每个节点对每个模块的边数
  modules <- names(split_idx)
  kis_mat <- sapply(modules, function(lev) {
    idx <- split_idx[[lev]]
    rowSums(A[, idx, drop = FALSE])
  })
  if (!is.matrix(kis_mat)) kis_mat <- as.matrix(kis_mat)
  # 去掉自身对角线：属于该模块的节点，k_is 减 1
  for (j in seq_along(modules)) {
    idx <- split_idx[[modules[j]]]
    kis_mat[idx, j] <- kis_mat[idx, j] - 1L
  }
  kis_mat[kis_mat < 0] <- 0  # 理论上不会小于0，保险

  sum_kis2 <- rowSums(kis_mat^2)
  k_tot    <- as.numeric(deg)
  P <- numeric(length(k_tot))
  P[k_tot == 0] <- 0
  nz <- (k_tot > 0)
  P[nz] <- 1 - (sum_kis2[nz] / (k_tot[nz]^2))
  names(P) <- ids

  # —— 组织输出
  out <- data.frame(
    nodes_id = ids,
    within_module_connectivities = z_vec[ids],
    among_module_connectivities  = P[ids],
    row.names = NULL,
    check.names = FALSE
  )
  nodes_bulk$nodes_id <- ids
  merge(out, nodes_bulk, by = "nodes_id", sort = FALSE)
}
