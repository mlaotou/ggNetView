test_that("build_graph_from_mat returns a graph with expected topology columns", {
  mat <- rbind(
    A = c(1, 2, 3, 4, 5, 6, 7, 8),
    B = c(1, 2, 3, 4, 5, 6, 7, 8),
    C = c(8, 7, 6, 5, 4, 3, 2, 1),
    D = c(2, 3, 4, 5, 6, 7, 8, 9)
  )

  colnames(mat) <- paste0("S", seq_len(ncol(mat)))

  graph_obj <- build_graph_from_mat(
    mat = mat,
    method = "cor",
    cor.method = "pearson",
    r.threshold = 0.5,
    p.threshold = 1,
    top_modules = 4,
    seed = 1
  )

  expect_s3_class(graph_obj, "tbl_graph")
  expect_gt(igraph::gorder(graph_obj), 0)
  expect_gt(igraph::gsize(graph_obj), 0)

  node_df <- graph_obj %>%
    tidygraph::activate(nodes) %>%
    tidygraph::as_tibble()

  edge_df <- graph_obj %>%
    tidygraph::activate(edges) %>%
    tidygraph::as_tibble()

  expect_true(all(c("name", "Modularity", "Degree", "Strength") %in% colnames(node_df)))
  expect_true(all(c("weight", "correlation", "corr_direction") %in% colnames(edge_df)))
})

test_that("build_graph_from_mat rejects duplicated sample names", {
  mat <- matrix(1:12, nrow = 3)
  colnames(mat) <- c("S1", "S1", "S2", "S3")

  expect_error(
    build_graph_from_mat(mat, method = "cor"),
    "duplicated colname"
  )
})

test_that("build_graph_from_igraph reuses existing module information", {
  g <- igraph::make_ring(6)
  igraph::V(g)$name <- paste0("N", seq_len(igraph::gorder(g)))
  igraph::V(g)$Modularity <- c("M1", "M1", "M1", "M2", "M2", "M2")

  graph_obj <- build_graph_from_igraph(
    igraph = g,
    top_modules = 3,
    seed = 1
  )

  node_df <- graph_obj %>%
    tidygraph::activate(nodes) %>%
    tidygraph::as_tibble()

  expect_true(all(as.character(node_df$Modularity) %in% c("M1", "M2")))
  expect_setequal(unique(as.character(node_df$Modularity)), c("M1", "M2"))
})

test_that("build_graph_from_mat supports Hmisc correlation analysis", {
  mat <- rbind(
    A = c(1, 2, 3, 4, 5, 6, 7, 8),
    B = c(2, 3, 4, 5, 6, 7, 8, 9),
    C = c(8, 7, 6, 5, 4, 3, 2, 1),
    D = c(3, 4, 5, 6, 7, 8, 9, 10)
  )

  colnames(mat) <- paste0("S", seq_len(ncol(mat)))

  graph_obj <- build_graph_from_mat(
    mat = mat,
    method = "Hmisc",
    cor.method = "spearman",
    r.threshold = 0.5,
    p.threshold = 1,
    top_modules = 4,
    seed = 1
  )

  expect_s3_class(graph_obj, "tbl_graph")
  expect_gt(igraph::gorder(graph_obj), 0)
  expect_gt(igraph::gsize(graph_obj), 0)

  edge_df <- graph_obj %>%
    tidygraph::activate(edges) %>%
    tidygraph::as_tibble()

  expect_true(all(edge_df$weight > 0))
})

test_that("build_graph_from_mat rejects kendall for Hmisc", {
  mat <- matrix(seq_len(24), nrow = 4)
  rownames(mat) <- paste0("A", seq_len(nrow(mat)))
  colnames(mat) <- paste0("S", seq_len(ncol(mat)))

  expect_error(
    build_graph_from_mat(mat, method = "Hmisc", cor.method = "kendall"),
    "only supports"
  )
})

test_that("sparcc_matrix_rcpp returns a symmetric matrix with dimnames", {
  set.seed(1)
  data <- matrix(sample(1:10, 24, replace = TRUE), nrow = 6)
  colnames(data) <- paste0("T", seq_len(ncol(data)))

  cor_mat <- sparcc_matrix_rcpp(data, iter = 2, inner_iter = 2, th = 0.1, nthreads = 0)

  expect_equal(dim(cor_mat), c(ncol(data), ncol(data)))
  expect_equal(cor_mat, t(cor_mat), tolerance = 1e-8)
  expect_identical(rownames(cor_mat), colnames(data))
  expect_identical(colnames(cor_mat), colnames(data))
})

test_that("get_network_topology works with default transform argument candidates", {
  mat <- matrix(sample(1:10, 24, replace = TRUE), nrow = 4)
  rownames(mat) <- paste0("A", seq_len(nrow(mat)))
  colnames(mat) <- paste0("S", seq_len(ncol(mat)))

  graph_obj <- build_graph_from_mat(
    mat = mat,
    method = "cor",
    cor.method = "pearson",
    r.threshold = 0,
    p.threshold = 1,
    seed = 1
  )

  out <- get_network_topology(
    graph_obj = graph_obj,
    mat = mat,
    method = "cor",
    cor.method = "pearson",
    r.threshold = 0,
    p.threshold = 1,
    bootstrap = 2
  )

  expect_true(is.list(out))
  expect_true(all(c("topology", "Robustness") %in% names(out)))
})

test_that("get_network_topology_parallel works when parallel is FALSE", {
  mat <- matrix(sample(1:10, 24, replace = TRUE), nrow = 4)
  rownames(mat) <- paste0("A", seq_len(nrow(mat)))
  colnames(mat) <- paste0("S", seq_len(ncol(mat)))

  graph_obj <- build_graph_from_mat(
    mat = mat,
    method = "cor",
    cor.method = "pearson",
    r.threshold = 0,
    p.threshold = 1,
    seed = 1
  )

  out <- get_network_topology_parallel(
    graph_obj = graph_obj,
    mat = mat,
    method = "cor",
    cor.method = "pearson",
    r.threshold = 0,
    p.threshold = 1,
    parallel = FALSE,
    bootstrap = 2
  )

  expect_true(is.list(out))
  expect_true(all(c("topology", "Robustness") %in% names(out)))
})

# ---------------------------------------------------------------------------
# build_graph_from_node_edge()
# ---------------------------------------------------------------------------

test_that("build_graph_from_node_edge retains isolated nodes", {
  node <- data.frame(
    name = c("A", "B", "C", "D", "E"),
    grp  = c("g1", "g1", "g2", "g2", "g3"),
    stringsAsFactors = FALSE
  )
  edge <- data.frame(
    from   = c("A", "B"),
    to     = c("B", "C"),
    weight = c(0.8, 0.6),
    stringsAsFactors = FALSE
  )

  obj <- build_graph_from_node_edge(node = node, edge = edge, top_modules = 5)

  expect_s3_class(obj, "tbl_graph")

  # Critical contract: D and E are isolated; build_graph_from_df() would
  # silently drop them. build_graph_from_node_edge() must keep them.
  node_df <- obj %>%
    tidygraph::activate(nodes) %>%
    tidygraph::as_tibble()
  expect_setequal(node_df$name, c("A", "B", "C", "D", "E"))

  expect_true(all(c("name", "grp", "Modularity", "Degree", "Strength") %in% colnames(node_df)))

  edge_df <- obj %>%
    tidygraph::activate(edges) %>%
    tidygraph::as_tibble()
  expect_true(all(c("weight", "correlation") %in% colnames(edge_df)))
})

test_that("build_graph_from_node_edge errors on missing arguments", {
  expect_error(
    build_graph_from_node_edge(edge = data.frame(from = "A", to = "B")),
    "node"
  )
  expect_error(
    build_graph_from_node_edge(node = data.frame(name = "A")),
    "edge"
  )
})

test_that("build_graph_from_node_edge errors on duplicated node IDs", {
  node <- data.frame(name = c("A", "A", "B"), stringsAsFactors = FALSE)
  edge <- data.frame(from = "A", to = "B", weight = 1, stringsAsFactors = FALSE)
  expect_error(
    build_graph_from_node_edge(node = node, edge = edge),
    "unique IDs"
  )
})

test_that("build_graph_from_node_edge errors when edge IDs are unknown", {
  node <- data.frame(name = c("A", "B"), stringsAsFactors = FALSE)
  edge <- data.frame(from = "A", to = "Z", weight = 1, stringsAsFactors = FALSE)
  expect_error(
    build_graph_from_node_edge(node = node, edge = edge),
    "appear in `edge` but not in"
  )
})

# ---------------------------------------------------------------------------
# build_graph_from_stringdb()
# ---------------------------------------------------------------------------

test_that("build_graph_from_stringdb parses STRING-style data and preserves evidence columns", {
  # Mimics the STRING TSV header `#node1` + extra evidence channels.
  stringdb_df <- data.frame(
    `#node1`                              = c("AANAT", "AANAT", "ABCA1", "ABCA1"),
    node2                                 = c("CRY1",  "TPH1",  "SIRT1", "APOA1"),
    coexpression                          = c(0,       0.109,   0.079,   0.055),
    experimentally_determined_interaction = c(0,       0,       0.046,   0.697),
    combined_score                        = c(0.608,   0.675,   0.520,   0.999),
    check.names      = FALSE,
    stringsAsFactors = FALSE
  )

  obj <- build_graph_from_stringdb(stringdb = stringdb_df, top_modules = 5)

  expect_s3_class(obj, "tbl_graph")
  expect_gt(igraph::gorder(obj), 0L)
  expect_gt(igraph::gsize(obj), 0L)

  edge_df <- obj %>%
    tidygraph::activate(edges) %>%
    tidygraph::as_tibble()

  # weight + correlation come from the standard pipeline.
  expect_true(all(c("weight", "correlation") %in% colnames(edge_df)))
  # Evidence channels survive as edge attributes (this is the key contract).
  expect_true(all(c("coexpression",
                    "experimentally_determined_interaction",
                    "combined_score") %in% colnames(edge_df)))
})

test_that("build_graph_from_stringdb applies score_threshold", {
  stringdb_df <- data.frame(
    node1          = c("A", "B", "C", "D"),
    node2          = c("B", "C", "D", "E"),
    combined_score = c(0.30, 0.50, 0.90, 0.95),
    stringsAsFactors = FALSE
  )

  obj <- build_graph_from_stringdb(
    stringdb        = stringdb_df,
    score_threshold = 0.6,
    top_modules     = 5
  )
  edge_df <- obj %>%
    tidygraph::activate(edges) %>%
    tidygraph::as_tibble()

  # Only the two edges with score >= 0.6 should remain.
  expect_equal(nrow(edge_df), 2L)
  expect_true(all(edge_df$weight >= 0.6))
})

test_that("build_graph_from_stringdb errors on missing required columns", {
  bad <- data.frame(foo = "X", bar = "Y", stringsAsFactors = FALSE)
  expect_error(
    build_graph_from_stringdb(stringdb = bad),
    "not found in"
  )
})

test_that("build_graph_from_stringdb supports a custom score_col", {
  stringdb_df <- data.frame(
    node1          = c("A", "B", "C"),
    node2          = c("B", "C", "D"),
    coexpression   = c(0.10, 0.50, 0.80),
    combined_score = c(0.40, 0.55, 0.85),
    stringsAsFactors = FALSE
  )

  obj <- build_graph_from_stringdb(
    stringdb        = stringdb_df,
    score_col       = "coexpression",
    score_threshold = 0.4,
    top_modules     = 5
  )
  edge_df <- obj %>%
    tidygraph::activate(edges) %>%
    tidygraph::as_tibble()

  # coexpression >= 0.4 keeps two of the three edges.
  expect_equal(nrow(edge_df), 2L)
  expect_true(all(edge_df$weight >= 0.4))
})
