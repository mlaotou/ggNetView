# Tests for the node-importance pair: get_node_centrality() and
# get_node_ivi(). Both should attach per-node columns to a tbl_graph
# without disturbing the existing schema.

# ---------------------------------------------------------------------------
# Shared fixture: a small correlation network the existing build_graph_*()
# pipeline produces. Same shape as the README/correlation-vignette examples,
# kept tiny for fast tests.
# ---------------------------------------------------------------------------
make_node_importance_fixture <- function(seed = 1L) {
  set.seed(seed)
  mat <- matrix(stats::rnorm(15 * 20), nrow = 15, ncol = 20)
  rownames(mat) <- paste0("feature", seq_len(15))
  colnames(mat) <- paste0("sample",  seq_len(20))
  build_graph_from_mat(
    mat           = mat,
    method        = "cor",
    cor.method    = "pearson",
    proc          = "none",
    r.threshold   = 0.2,
    p.threshold   = 1,
    module.method = "Fast_greedy",
    top_modules   = 5,
    seed          = seed
  )
}

# ---------------------------------------------------------------------------
# get_node_centrality()
# ---------------------------------------------------------------------------

test_that("get_node_centrality: default measures all become per-node columns", {
  g  <- make_node_importance_fixture()
  N  <- igraph::gorder(g)
  g2 <- get_node_centrality(g)

  expect_s3_class(g2, "tbl_graph")

  node_df <- g2 %>% tidygraph::activate(nodes) %>% tidygraph::as_tibble()
  expected <- c("Betweenness", "Closeness", "Eigenvector",
                "PageRank", "Hub_score", "Authority_score",
                "Coreness", "Harmonic")
  expect_true(all(expected %in% colnames(node_df)))

  # Each centrality column must have one value per node (no scalars).
  for (m in expected) {
    expect_equal(length(node_df[[m]]), N,
                 info = sprintf("measure %s has wrong length", m))
    expect_true(is.numeric(node_df[[m]]),
                info = sprintf("measure %s is not numeric", m))
  }
})

test_that("get_node_centrality: subsetting measures works", {
  g  <- make_node_importance_fixture()
  g2 <- get_node_centrality(g, measures = c("Betweenness", "PageRank"))

  node_df <- g2 %>% tidygraph::activate(nodes) %>% tidygraph::as_tibble()
  expect_true(all(c("Betweenness", "PageRank") %in% colnames(node_df)))
  expect_false("Closeness" %in% colnames(node_df))
  expect_false("Eigenvector" %in% colnames(node_df))
})

test_that("get_node_centrality: 'all' shorthand expands to every measure", {
  g  <- make_node_importance_fixture()
  g_all  <- get_node_centrality(g, measures = "all")
  g_def  <- get_node_centrality(g)

  cols_all <- colnames(g_all  %>% tidygraph::activate(nodes) %>% tidygraph::as_tibble())
  cols_def <- colnames(g_def  %>% tidygraph::activate(nodes) %>% tidygraph::as_tibble())

  # `all` and the default both currently expand to the same 8 measures;
  # this test guards against the two sets diverging silently.
  expect_setequal(cols_all, cols_def)
})

test_that("get_node_centrality: weighted = TRUE inverts edge weights to distances", {
  g <- make_node_importance_fixture()
  g_un <- get_node_centrality(g, measures = "Betweenness", weighted = FALSE)
  g_w  <- get_node_centrality(g, measures = "Betweenness", weighted = TRUE)

  node_un <- g_un %>% tidygraph::activate(nodes) %>% tidygraph::as_tibble()
  node_w  <- g_w  %>% tidygraph::activate(nodes) %>% tidygraph::as_tibble()

  # Weighted and unweighted versions can produce different rankings; we
  # don't assert anything specific about the ordering, only that the
  # weighted call succeeds and returns one numeric value per node.
  expect_equal(length(node_w$Betweenness), nrow(node_un))
  expect_true(all(is.finite(node_w$Betweenness) | is.na(node_w$Betweenness)))
})

test_that("get_node_centrality: rejects unknown measures with a helpful error", {
  g <- make_node_importance_fixture()
  expect_error(
    get_node_centrality(g, measures = c("Betweenness", "FooBar")),
    "Unknown measure"
  )
})

test_that("get_node_centrality: rejects non-tbl_graph input", {
  expect_error(
    get_node_centrality(matrix(0, 3, 3)),
    "tbl_graph"
  )
})

test_that("get_node_centrality: overwrite = FALSE skips clashing columns", {
  g <- make_node_importance_fixture()
  # Pre-populate a Betweenness column with a sentinel value.
  g_pre <- g %>%
    tidygraph::activate(nodes) %>%
    tidygraph::mutate(Betweenness = -1)

  # Capture both the warning text AND the function's return value in one
  # call. testthat 3's `expect_warning()` doesn't reliably forward the
  # return value when the warning matches, so we use a calling-handler:
  # this lets us inspect the warning and keep the augmented graph.
  warning_text <- NULL
  g_post <- withCallingHandlers(
    get_node_centrality(
      g_pre,
      measures  = c("Betweenness", "Coreness"),
      overwrite = FALSE
    ),
    warning = function(w) {
      warning_text <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )

  expect_true(!is.null(warning_text),
              info = "Expected a warning from get_node_centrality(overwrite = FALSE).")
  expect_match(warning_text, "already exist")

  node_df <- g_post %>%
    tidygraph::activate(nodes) %>%
    tidygraph::as_tibble()
  expect_true(all(node_df$Betweenness == -1))     # untouched
  expect_true("Coreness" %in% colnames(node_df))  # added
})

# ---------------------------------------------------------------------------
# get_node_ivi()
# ---------------------------------------------------------------------------

test_that("get_node_ivi: attaches an IVI column with one value per node", {
  testthat::skip_if_not_installed("influential")
  g  <- make_node_importance_fixture()
  g2 <- get_node_ivi(g)

  expect_s3_class(g2, "tbl_graph")
  node_df <- g2 %>% tidygraph::activate(nodes) %>% tidygraph::as_tibble()
  expect_true("IVI" %in% colnames(node_df))
  expect_equal(length(node_df$IVI), igraph::gorder(g))
  expect_true(is.numeric(node_df$IVI))
  # IVI is non-negative and finite by construction. Some versions of
  # `influential` auto-normalise the result to [0, 100]; others return
  # un-scaled values. We only assert the universal invariants here so
  # the test stays valid across `influential` releases.
  expect_true(all(is.finite(node_df$IVI)))
  expect_true(all(node_df$IVI >= 0))
})

test_that("get_node_ivi: rejects non-tbl_graph input", {
  expect_error(
    get_node_ivi(igraph::make_ring(5)),
    "tbl_graph"
  )
})

test_that("get_node_ivi: weights = 'weight' resolves the edge-weight attribute", {
  testthat::skip_if_not_installed("influential")
  g  <- make_node_importance_fixture()
  # `weight` is the standard edge attribute set by build_graph_from_*().
  g2 <- get_node_ivi(g, weights = "weight")
  node_df <- g2 %>% tidygraph::activate(nodes) %>% tidygraph::as_tibble()
  expect_true("IVI" %in% colnames(node_df))
})

test_that("get_node_ivi: errors clearly when the named edge attribute is missing", {
  testthat::skip_if_not_installed("influential")
  g  <- make_node_importance_fixture()
  expect_error(
    get_node_ivi(g, weights = "no_such_attr"),
    "not found on the graph"
  )
})

test_that("get_node_ivi: scale = 'range' / 'z-scale' / 'none' all run", {
  testthat::skip_if_not_installed("influential")
  g <- make_node_importance_fixture()
  N <- igraph::gorder(g)

  for (s in c("range", "z-scale", "none")) {
    g_s <- get_node_ivi(g, scale = s)
    iv  <- (g_s %>% tidygraph::activate(nodes) %>% tidygraph::as_tibble())$IVI
    expect_equal(length(iv), N, info = sprintf("scale='%s'", s))
    expect_true(all(is.finite(iv)), info = sprintf("scale='%s'", s))
  }
})

test_that("get_node_ivi: scale = 'range' yields values in [1, 100] on modern influential", {
  testthat::skip_if_not_installed("influential")
  # Skip on legacy `influential` versions that didn't expose `scale`.
  # On those versions the legacy boolean `scaled` produces a different
  # numerical range that is not necessarily [1, 100], so we don't
  # constrain it here.
  if (!"scale" %in% names(formals(influential::ivi))) {
    testthat::skip("Installed `influential` does not expose the `scale` argument.")
  }
  g  <- make_node_importance_fixture()
  g2 <- get_node_ivi(g, scale = "range")
  iv <- (g2 %>% tidygraph::activate(nodes) %>% tidygraph::as_tibble())$IVI
  iv_fin <- iv[is.finite(iv)]
  expect_gte(min(iv_fin), 1 - 1e-9)
  expect_lte(max(iv_fin), 100 + 1e-9)
})
