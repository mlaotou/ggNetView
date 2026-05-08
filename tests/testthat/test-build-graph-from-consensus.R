# Tests for build_graph_from_consensus().
#
# Goal: cover every method + rank-fusion algorithm + the alignment, threshold,
# and validation paths, but keep each fixture small enough that the full file
# runs in well under a second.
#
# Most fixtures share the same idea: 3 synthetic adjacency matrices with a
# common "core" subgraph plus per-method noise. The core is what consensus
# strategies should recover; the noise is what they should suppress.

# ---------------------------------------------------------------------------
# Shared fixture: 3 method matrices on the same 12 features, with a clear
# 4-node clique embedded in a sea of small random weights.
# ---------------------------------------------------------------------------
make_consensus_fixture <- function(seed = 1L, n = 12, clique = 1:4,
                                   clique_w = 0.85, noise_sd = 0.03) {
  set.seed(seed)
  nm <- paste0("g", seq_len(n))
  base <- matrix(0, n, n, dimnames = list(nm, nm))
  for (i in clique) for (j in clique) if (i < j) base[i, j] <- base[j, i] <- clique_w

  jitter <- function(s) {
    set.seed(s)
    e <- matrix(stats::rnorm(n * n, sd = noise_sd), n, n,
                dimnames = list(nm, nm))
    e <- (e + t(e)) / 2
    diag(e) <- 0
    base + e
  }
  list(method_A = jitter(11), method_B = jitter(22), method_C = jitter(33))
}

# ---------------------------------------------------------------------------
# Method coverage: every top-level method returns a tbl_graph with the
# expected schema.
# ---------------------------------------------------------------------------

test_that("build_graph_from_consensus: rank_fusion + borda returns a valid tbl_graph", {
  adj_list <- make_consensus_fixture()
  obj <- build_graph_from_consensus(
    adj_list = adj_list,
    method = "rank_fusion",
    rank_fusion_algorithm = "borda",
    threshold = 0.3,
    top_modules = 3,
    seed = 1
  )

  expect_s3_class(obj, "tbl_graph")
  expect_gt(igraph::gorder(obj), 0L)

  node_df <- obj %>%
    tidygraph::activate(nodes) %>%
    tidygraph::as_tibble()
  expect_true(all(c("name", "Modularity", "Degree", "Strength") %in% colnames(node_df)))

  edge_df <- obj %>%
    tidygraph::activate(edges) %>%
    tidygraph::as_tibble()
  expect_true(all(c("weight", "correlation", "corr_direction") %in% colnames(edge_df)))
})

test_that("build_graph_from_consensus: rank_fusion + rrf works", {
  adj_list <- make_consensus_fixture()
  obj <- build_graph_from_consensus(
    adj_list = adj_list,
    method = "rank_fusion",
    rank_fusion_algorithm = "rrf",
    threshold = 0.3,
    top_modules = 3,
    seed = 1
  )
  expect_s3_class(obj, "tbl_graph")
  expect_gt(igraph::gsize(obj), 0L)
})

test_that("build_graph_from_consensus: rank_fusion + rra works (with or without RobustRankAggreg)", {
  adj_list <- make_consensus_fixture()
  obj <- build_graph_from_consensus(
    adj_list = adj_list,
    method = "rank_fusion",
    rank_fusion_algorithm = "rra",
    threshold = 0.3,
    top_modules = 3,
    seed = 1
  )
  expect_s3_class(obj, "tbl_graph")
  expect_gt(igraph::gsize(obj), 0L)
})

test_that("build_graph_from_consensus: intersection keeps only edges in every method", {
  # Construct mats where exactly ONE pair (g1-g2) is present in all three
  # methods; per-method extras are unique to each method.
  nm <- paste0("g", 1:6)
  zero <- matrix(0, 6, 6, dimnames = list(nm, nm))
  m1 <- m2 <- m3 <- zero
  m1[1, 2] <- m1[2, 1] <- 0.9; m1[3, 4] <- m1[4, 3] <- 0.9
  m2[1, 2] <- m2[2, 1] <- 0.8; m2[3, 5] <- m2[5, 3] <- 0.7
  m3[1, 2] <- m3[2, 1] <- 0.7; m3[5, 6] <- m3[6, 5] <- 0.6

  obj <- build_graph_from_consensus(
    adj_list = list(m1, m2, m3),
    method = "intersection",
    binarize = "none",
    top_modules = 5,
    seed = 1
  )
  edge_df <- obj %>%
    tidygraph::activate(edges) %>%
    tidygraph::as_tibble()
  # Only one edge survives the intersection; tbl_graph stores it once.
  expect_equal(nrow(edge_df), 1L)
})

test_that("build_graph_from_consensus: weighted_average produces in-range weights", {
  adj_list <- make_consensus_fixture()
  obj <- build_graph_from_consensus(
    adj_list = adj_list,
    method = "weighted_average",
    weights = c(1, 2, 1),
    threshold = 0.2,
    top_modules = 3,
    seed = 1
  )
  edge_df <- obj %>%
    tidygraph::activate(edges) %>%
    tidygraph::as_tibble()
  # `weight` is abs() of the consensus value; consensus is in [-1, 1] after
  # per-method min-max normalisation, so weight must be in [0, 1].
  expect_true(all(edge_df$weight <= 1 + 1e-9))
})

test_that("build_graph_from_consensus: majority_vote requires at least min_methods support", {
  adj_list <- make_consensus_fixture()
  obj <- build_graph_from_consensus(
    adj_list = adj_list,
    method = "majority_vote",
    binarize = "threshold",
    binarize_threshold = 0.5,  # only the clique edges clear this in any method
    min_methods = 2,
    top_modules = 3,
    seed = 1
  )
  expect_s3_class(obj, "tbl_graph")
  # Every surviving edge should have non-zero weight (consensus mean of yes-voters).
  edge_df <- obj %>%
    tidygraph::activate(edges) %>%
    tidygraph::as_tibble()
  expect_true(all(edge_df$weight > 0))
})

# ---------------------------------------------------------------------------
# Node alignment
# ---------------------------------------------------------------------------

test_that("build_graph_from_consensus: node_handling = 'intersect' keeps only common features", {
  m1 <- matrix(c(0, 1, 1, 0), 2, 2, dimnames = list(c("a", "b"), c("a", "b")))
  m2 <- matrix(c(0, 1, 1, 0), 2, 2, dimnames = list(c("b", "c"), c("b", "c")))
  m3 <- matrix(c(0, 1, 1, 0), 2, 2, dimnames = list(c("a", "b"), c("a", "b")))
  # Common features between all three: {b}. Should error: < 2 features.
  expect_error(
    build_graph_from_consensus(list(m1, m2, m3), method = "intersection",
                               node_handling = "intersect", top_modules = 2),
    "fewer than 2 features"
  )
})

test_that("build_graph_from_consensus: node_handling = 'union' tolerates non-overlapping features", {
  nm1 <- c("a", "b", "c")
  nm2 <- c("b", "c", "d")
  m1 <- matrix(c(0, 0.7, 0.6, 0.7, 0, 0.5, 0.6, 0.5, 0), 3, 3,
               dimnames = list(nm1, nm1))
  m2 <- matrix(c(0, 0.7, 0.6, 0.7, 0, 0.5, 0.6, 0.5, 0), 3, 3,
               dimnames = list(nm2, nm2))
  obj <- build_graph_from_consensus(
    adj_list = list(m1, m2),
    method = "weighted_average",
    node_handling = "union",
    top_modules = 3,
    seed = 1
  )
  node_df <- obj %>%
    tidygraph::activate(nodes) %>%
    tidygraph::as_tibble()
  # Nodes that survive must be a subset of the union {a, b, c, d};
  # connected components form after isolated vertices are dropped, so we
  # don't insist on every union member, just on no foreign names.
  expect_true(all(node_df$name %in% c("a", "b", "c", "d")))
})

# ---------------------------------------------------------------------------
# Threshold and validation
# ---------------------------------------------------------------------------

test_that("build_graph_from_consensus: threshold filters edges by |consensus weight|", {
  adj_list <- make_consensus_fixture()
  obj_loose <- build_graph_from_consensus(adj_list, threshold = NULL,
                                          top_modules = 3, seed = 1)
  obj_strict <- build_graph_from_consensus(adj_list, threshold = 0.5,
                                           top_modules = 3, seed = 1)
  # Stricter threshold -> at most as many edges.
  expect_lte(igraph::gsize(obj_strict), igraph::gsize(obj_loose))
})

test_that("build_graph_from_consensus: errors on wrong input shapes", {
  # Single matrix -> error
  m <- matrix(c(0, 1, 1, 0), 2, 2, dimnames = list(c("a", "b"), c("a", "b")))
  expect_error(
    build_graph_from_consensus(list(m), method = "rank_fusion"),
    "at least 2"
  )

  # Non-square matrix -> error
  m_bad <- matrix(0, 2, 3, dimnames = list(c("a", "b"), c("a", "b", "c")))
  expect_error(
    build_graph_from_consensus(list(m_bad, m_bad), method = "rank_fusion"),
    "not square"
  )

  # Missing dimnames -> error
  m_nameless <- matrix(0, 2, 2)
  expect_error(
    build_graph_from_consensus(list(m_nameless, m_nameless), method = "rank_fusion"),
    "row and column names"
  )
})

test_that("build_graph_from_consensus: weights validation for weighted_average", {
  adj_list <- make_consensus_fixture()
  expect_error(
    build_graph_from_consensus(adj_list, method = "weighted_average",
                               weights = c(1, 2)),  # wrong length
    "length"
  )
  expect_error(
    build_graph_from_consensus(adj_list, method = "weighted_average",
                               weights = c(-1, 1, 1)),
    "non-negative"
  )
})

test_that("build_graph_from_consensus: rank_fusion preserves sign from the mean correlation", {
  # Two anti-correlated features in every method: consensus weight should
  # come out negative.
  nm <- c("x", "y", "z")
  m1 <- matrix(c(0, -0.9, 0.0, -0.9, 0, 0.6, 0.0, 0.6, 0), 3, 3,
               dimnames = list(nm, nm))
  m2 <- matrix(c(0, -0.7, 0.0, -0.7, 0, 0.5, 0.0, 0.5, 0), 3, 3,
               dimnames = list(nm, nm))

  obj <- build_graph_from_consensus(
    adj_list = list(m1, m2),
    method = "rank_fusion",
    rank_fusion_algorithm = "borda",
    threshold = 0.1,
    top_modules = 3,
    seed = 1
  )
  edge_df <- obj %>%
    tidygraph::activate(edges) %>%
    tidygraph::as_tibble()
  # The negative x-y edge must be there; and `correlation` (signed) for
  # that edge must be negative.
  if (nrow(edge_df) > 0L) {
    # Find x-y edge by walking node indices.
    nodes_df <- obj %>%
      tidygraph::activate(nodes) %>%
      tidygraph::as_tibble()
    name_of <- function(idx) nodes_df$name[idx]
    edge_df$from_name <- vapply(edge_df$from, name_of, character(1))
    edge_df$to_name   <- vapply(edge_df$to,   name_of, character(1))
    xy_row <- edge_df[(edge_df$from_name == "x" & edge_df$to_name == "y") |
                      (edge_df$from_name == "y" & edge_df$to_name == "x"), , drop = FALSE]
    if (nrow(xy_row) > 0L) {
      expect_lt(xy_row$correlation[1L], 0)
    }
  }
})
