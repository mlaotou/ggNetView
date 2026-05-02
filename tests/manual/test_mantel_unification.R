# test_mantel_unification.R
# ----------------------------------------------------------------------
# Manual smoke test for the unified Mantel API across:
#   - mantel_utils helpers (mantel_block_vs_col, mantel_pairwise)
#   - gglink_heatmaps()                  with mantel_kind ∈ {block_vs_col, col_vs_col}
#   - ggnetview_modularity_heatmaps()    with mantel_kind ∈ {block_vs_col, col_vs_col}
#
# Run inside the package root with:
#   devtools::load_all(".")
#   source("tests/manual/test_mantel_unification.R")
# ----------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(vegan)
  library(igraph)
  library(tidygraph)
})

set.seed(2026)
N_SAMP <- 30L

cat_sec <- function(x) cat("\n==== ", x, " ", strrep("=", max(0, 50 - nchar(x))), "\n", sep = "")
ok <- function(cond, msg) cat(if (isTRUE(cond)) "[OK]   " else "[FAIL] ", msg, "\n", sep = "")


# ============================================================
# 0) Synthetic data
# ============================================================
cat_sec("0) Building synthetic data")

# env: 30 samples x 24 env vars, organised in 4 blocks of 6 cols each
env <- as.data.frame(matrix(rnorm(N_SAMP * 24), nrow = N_SAMP))
colnames(env) <- paste0("E", sprintf("%02d", seq_len(ncol(env))))
rownames(env) <- paste0("S", sprintf("%02d", seq_len(N_SAMP)))

env_select <- list(
  Env01 = 1:6,
  Env02 = 7:12,
  Env03 = 13:18,
  Env04 = 19:24
)

# spec (for gglink_heatmaps): 30 samples x 30 species, 2 blocks
spec <- as.data.frame(matrix(abs(rnorm(N_SAMP * 30)) * 100, nrow = N_SAMP))
colnames(spec) <- paste0("Sp", sprintf("%02d", seq_len(ncol(spec))))
rownames(spec) <- rownames(env)

spec_select <- list(
  Spec01 = 1:15,
  Spec02 = 16:30
)

# otu_mat (for ggnetview_modularity_heatmaps): 40 OTUs x 30 samples
otu_mat <- matrix(abs(rnorm(40 * N_SAMP)) * 100, nrow = 40)
rownames(otu_mat) <- paste0("OTU", sprintf("%02d", seq_len(40)))
colnames(otu_mat) <- rownames(env)

# graph_obj: 40 nodes, 4 modules of 10 OTUs each, with the column the
# code expects. Edges are arbitrary (only node attributes matter for the
# Mantel branch).
edges <- data.frame(
  from = sample(rownames(otu_mat), 60, replace = TRUE),
  to   = sample(rownames(otu_mat), 60, replace = TRUE),
  weight = runif(60)
) |> dplyr::filter(from != to)

nodes <- data.frame(
  name       = rownames(otu_mat),
  Modularity = rep(c("M1", "M2", "M3", "M4"), each = 10),
  stringsAsFactors = FALSE
)

graph_obj <- tidygraph::tbl_graph(nodes = nodes, edges = edges, directed = FALSE)

ok(nrow(env)  == N_SAMP, "env has 30 rows")
ok(nrow(spec) == N_SAMP, "spec has 30 rows")
ok(ncol(otu_mat) == N_SAMP, "otu_mat has 30 sample columns")


# ============================================================
# 1) Helpers in mantel_utils.R
# ============================================================
cat_sec("1) mantel_block_vs_col() helper")
out_block <- mantel_block_vs_col(
  spec_df          = spec[, spec_select$Spec01],
  env_df           = env[, env_select$Env01],
  block_name       = "Spec01",
  method           = "pearson",
  spec_dist_method = "bray",
  env_dist_method  = "euclidean",
  permutations     = 99L
)
print(out_block)
ok(all(c("ID","Type","Correlation","Pvalue") %in% colnames(out_block)),
   "schema has ID/Type/Correlation/Pvalue")
ok(all(out_block$ID == "Spec01"),                "ID column is the block name")
ok(nrow(out_block) == length(env_select$Env01), "one row per env col in the block")
ok(all(!is.na(out_block$Correlation)),           "no NA correlations on synthetic data")

cat_sec("1') mantel_pairwise() helper (legacy)")
out_pair <- mantel_pairwise(
  spec_df = spec[, spec_select$Spec01],
  env_df  = env[, env_select$Env01],
  method  = "pearson",
  permutations = 99L
)
ok(nrow(out_pair) == length(spec_select$Spec01) * length(env_select$Env01),
   "col_vs_col grid has |spec_cols| x |env_cols| rows")
ok(all(c("ID","Type","Correlation","Pvalue") %in% colnames(out_pair)),
   "schema matches block_vs_col on the four key columns")


# ============================================================
# 2) gglink_heatmaps: mantel_kind = "block_vs_col"
# ============================================================
cat_sec("2) gglink_heatmaps + mantel_kind = block_vs_col")
res_link_block <- tryCatch(
  gglink_heatmaps(
    env             = env,
    spec            = spec,
    env_select      = env_select,
    spec_select     = spec_select,
    relation_method = "mantel",
    mantel_kind     = "block_vs_col",
    spec_dist_method = "bray",
    env_dist_method  = "euclidean",
    permutations     = 99L,
    drop_nonsig      = FALSE
  ),
  error = function(e) { cat("[FAIL] gglink_heatmaps block_vs_col errored:\n"); print(e); NULL }
)
if (!is.null(res_link_block)) {
  stats_df <- res_link_block[[3]]
  cat("rows in stats_df:", nrow(stats_df), "\n")
  print(head(stats_df))
  ok(all(stats_df$method == "mantel"),                       "method column == 'mantel'")
  ok(all(stats_df$ID %in% names(spec_select)),               "ID values are spec_block names (Spec01/Spec02)")
  ok(all(stats_df$Type %in% colnames(env)),                  "Type values are env column names")
  ok(inherits(res_link_block[[1]], "ggplot"),                "p1 is a ggplot")
  ok(inherits(res_link_block[[2]], "ggplot"),                "p2 is a ggplot")
}

# 2b) same call + spec_collapse = TRUE (the natural pairing)
cat_sec("2b) gglink_heatmaps + block_vs_col + spec_collapse = TRUE")
res_link_block_col <- tryCatch(
  gglink_heatmaps(
    env             = env,
    spec            = spec,
    env_select      = env_select,
    spec_select     = spec_select,
    relation_method = "mantel",
    mantel_kind     = "block_vs_col",
    spec_collapse   = TRUE,
    permutations    = 99L
  ),
  error = function(e) { cat("[FAIL] gglink_heatmaps block_vs_col + collapse errored:\n"); print(e); NULL }
)
ok(!is.null(res_link_block_col), "spec_collapse=TRUE call returns a result")


# ============================================================
# 3) gglink_heatmaps: mantel_kind = "col_vs_col"
# ============================================================
cat_sec("3) gglink_heatmaps + mantel_kind = col_vs_col (legacy)")
res_link_col <- tryCatch(
  gglink_heatmaps(
    env             = env,
    spec            = spec,
    env_select      = env_select,
    spec_select     = spec_select,
    relation_method = "mantel",
    mantel_kind     = "col_vs_col",
    permutations    = 99L
  ),
  error = function(e) { cat("[FAIL] gglink_heatmaps col_vs_col errored:\n"); print(e); NULL }
)
if (!is.null(res_link_col)) {
  stats_df <- res_link_col[[3]]
  cat("rows in stats_df:", nrow(stats_df), "\n")
  print(head(stats_df))
  ok(all(stats_df$method == "mantel"),     "method column == 'mantel'")
  ok(all(stats_df$ID %in% colnames(spec)), "ID values are spec column names (legacy schema)")
  # block_vs_col vs col_vs_col should give clearly different numerics
  if (!is.null(res_link_block)) {
    n_unique_block <- length(unique(res_link_block[[3]]$Correlation))
    n_unique_col   <- length(unique(res_link_col$Correlation))
    cat("unique correlations: block_vs_col =", n_unique_block,
        "  col_vs_col =", n_unique_col, "\n")
  }
}


# ============================================================
# 4) ggnetview_modularity_heatmaps: block_vs_col
# ============================================================
cat_sec("4) ggnetview_modularity_heatmaps + block_vs_col")
res_mod_block <- tryCatch(
  ggnetview_modularity_heatmaps(
    graph_obj        = graph_obj,
    env              = env,
    otu_mat          = otu_mat,
    env_select       = env_select,
    relation_method  = "mantel",
    mantel_kind      = "block_vs_col",
    spec_dist_method = "bray",
    env_dist_method  = "euclidean",
    permutations     = 99L
  ),
  error = function(e) { cat("[FAIL] modularity_heatmaps block_vs_col errored:\n"); print(e); NULL }
)
if (!is.null(res_mod_block)) {
  stats_df <- res_mod_block[[3]]
  cat("rows in stats_df:", nrow(stats_df), "\n")
  print(head(stats_df))
  ok(all(stats_df$method == "mantel"),               "method column == 'mantel'")
  ok(all(stats_df$ID %in% c("M1","M2","M3","M4")),    "ID values are module names")
  expected_rows <- length(unique(stats_df$ID)) * sum(lengths(env_select))
  cat("expected rows  :", expected_rows, "\n")
  ok(nrow(stats_df) == expected_rows, "rows = (#modules) x (#env cols across all blocks)")
}


# ============================================================
# 5) ggnetview_modularity_heatmaps: col_vs_col
# ============================================================
cat_sec("5) ggnetview_modularity_heatmaps + col_vs_col (legacy)")
res_mod_col <- tryCatch(
  ggnetview_modularity_heatmaps(
    graph_obj        = graph_obj,
    env              = env,
    otu_mat          = otu_mat,
    env_select       = env_select,
    relation_method  = "mantel",
    mantel_kind      = "col_vs_col",
    permutations     = 99L
  ),
  error = function(e) { cat("[FAIL] modularity_heatmaps col_vs_col errored:\n"); print(e); NULL }
)
if (!is.null(res_mod_col)) {
  stats_df <- res_mod_col[[3]]
  cat("rows in stats_df:", nrow(stats_df), "\n")
  print(head(stats_df))
  ok(all(stats_df$method == "mantel"), "method column == 'mantel'")
  ok(all(stats_df$ID %in% c("M1","M2","M3","M4")), "ID values are module names")
}


cat_sec("DONE")
cat("If every line above starts with [OK], the unified Mantel API works on\n",
    "synthetic data and you can move on to your real datasets.\n", sep = "")
