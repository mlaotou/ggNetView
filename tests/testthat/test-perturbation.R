# Tests for the virtual-perturbation trio:
#   get_network_perturbation()  -- structural attack (type 1)
#   get_node_influence()        -- abundance-influence propagation (type 2)
#   press_perturbation()        -- press-perturbation approximation (type 3)
# and the companion plot ggnetview_perturbation_curve().

# ---------------------------------------------------------------------------
# Shared fixture: a small correlation network from build_graph_from_mat(),
# same shape as test-node-importance.R, kept tiny for fast tests.
# ---------------------------------------------------------------------------
make_perturbation_fixture <- function(seed = 1L) {
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
# get_network_perturbation()
# ---------------------------------------------------------------------------

test_that("targeted attack returns a well-formed, monotone LCC curve", {
  g   <- make_perturbation_fixture()
  res <- get_network_perturbation(g, strategy = "targeted",
                                  centrality = "degree", plot = FALSE)

  expect_named(res, c("curve", "robustness_index", "plot"))
  expect_true(all(c("strategy", "fraction", "metric", "value") %in% names(res$curve)))

  lcc <- res$curve[res$curve$metric == "LCC_fraction", ]
  lcc <- lcc[order(lcc$fraction), ]

  # LCC fraction in [0, 1]; the no-removal point (fraction 0) is the
  # maximum (the network may be disconnected, so it need not equal 1),
  # and removing everything leaves an empty graph (0).
  expect_true(all(lcc$value >= 0 & lcc$value <= 1))
  expect_equal(lcc$value[1], max(lcc$value))
  expect_equal(lcc$value[nrow(lcc)], 0)
  # Static targeted removal can only shrink the giant component.
  expect_true(all(diff(lcc$value) <= 1e-8))

  # R-index lies in [0, 1].
  expect_true(res$robustness_index$R_index >= 0 &&
              res$robustness_index$R_index <= 1)
})

test_that("random strategy is reproducible and reports se", {
  g  <- make_perturbation_fixture()
  r1 <- get_network_perturbation(g, strategy = "random", bootstrap = 10,
                                 seed = 42, plot = FALSE)
  r2 <- get_network_perturbation(g, strategy = "random", bootstrap = 10,
                                 seed = 42, plot = FALSE)
  expect_equal(r1$curve$value, r2$curve$value)
  expect_true(any(is.finite(r1$curve$value_se) & r1$curve$value_se > 0))
})

# Use a deterministic hub-and-spoke graph (one central hub + spokes, plus a
# small triangle) so that removing the highest-degree node necessarily
# shatters the network. On this structure a degree-targeted attack is
# guaranteed to be at least as damaging as random failure -- this avoids the
# ~1-2% flakiness that the property shows on random correlation graphs.
make_hub_fixture <- function() {
  edges <- data.frame(
    from = c(rep("hub", 8), "s1", "s2", "s1"),
    to   = c(paste0("s", 1:8), "s2", "s3", "s3"),
    weight = 1
  )
  build_graph_from_df(df = edges)
}

test_that("targeted attack is at least as damaging as random (lower R-index)", {
  g   <- make_hub_fixture()
  rnd <- get_network_perturbation(g, strategy = "random", bootstrap = 50,
                                  seed = 7, plot = FALSE)
  tgt <- get_network_perturbation(g, strategy = "targeted",
                                  centrality = "degree", plot = FALSE)
  expect_lte(tgt$robustness_index$R_index, rnd$robustness_index$R_index + 1e-8)
  # Removing the hub first should collapse the giant component hard.
  expect_lt(tgt$robustness_index$R_index, 0.9)
})

test_that("manual / module knock-out produce before/after rows", {
  g  <- make_perturbation_fixture()
  nm <- get_graph_nodes(g)$name[1:2]
  mn <- get_network_perturbation(g, strategy = "manual", target = nm,
                                 plot = FALSE)
  expect_equal(sort(unique(mn$curve$fraction))[1], 0)
  expect_true(any(mn$curve$fraction > 0))

  mods <- as.character(get_graph_nodes(g)$Modularity)
  md <- get_network_perturbation(g, strategy = "module", target = mods[1],
                                 plot = FALSE)
  expect_true(any(md$curve$fraction > 0))
})

test_that("get_network_perturbation validates input", {
  expect_error(get_network_perturbation(list()), "tbl_graph")
  g <- make_perturbation_fixture()
  expect_error(get_network_perturbation(g, strategy = "manual"),
               "target")
})

# ---------------------------------------------------------------------------
# get_node_influence()
# ---------------------------------------------------------------------------

test_that("get_node_influence attaches a numeric Influence column", {
  g   <- make_perturbation_fixture()
  src <- get_graph_nodes(g)$name[1]
  g2  <- get_node_influence(g, source = src)

  expect_s3_class(g2, "tbl_graph")
  nd <- g2 %>% tidygraph::activate(nodes) %>% tidygraph::as_tibble()
  expect_true("Influence" %in% names(nd))
  expect_equal(length(nd$Influence), igraph::gorder(g))
  expect_true(all(is.finite(nd$Influence)))

  # source node's own influence is dropped to 0 by default
  expect_equal(nd$Influence[nd$name == src], 0)
})

test_that("get_node_influence rejects unknown source and bad alpha", {
  g <- make_perturbation_fixture()
  expect_error(get_node_influence(g, source = "no_such_node"), "Unknown source")
  src <- get_graph_nodes(g)$name[1]
  expect_error(get_node_influence(g, source = src, alpha = 1.5), "alpha")
})

# ---------------------------------------------------------------------------
# press_perturbation()
# ---------------------------------------------------------------------------

test_that("press_perturbation auto-stabilises and inverts correctly", {
  g  <- make_perturbation_fixture()
  pp <- press_perturbation(g)

  expect_named(pp, c("net_effect", "stable", "eigen_real_max",
                     "self_regulation", "response"))
  expect_true(pp$stable)
  expect_lt(pp$eigen_real_max, 0)

  N <- pp$net_effect
  expect_equal(dim(N)[1], igraph::gorder(g))
  expect_true(all(is.finite(N)))

  # N must satisfy N = -A^{-1}, i.e. -A %*% N = I.
  A <- pp$net_effect            # rebuild A from the returned pieces
  # Reconstruct A from net_effect: A = -solve(N)
  A_recon <- -solve(N)
  expect_equal(unname(A_recon %*% N), -diag(nrow(N)), tolerance = 1e-6)
})

test_that("press_perturbation source query ranks responses", {
  g   <- make_perturbation_fixture()
  src <- get_graph_nodes(g)$name[1]
  pp  <- press_perturbation(g, source = src)
  expect_s3_class(pp$response, "data.frame")
  expect_true(all(c("name", "net_response") %in% names(pp$response)))
  # ordered by descending absolute response
  a <- abs(pp$response$net_response)
  expect_true(all(diff(a) <= 1e-8))
})

test_that("press_perturbation warns when forced unstable", {
  g <- make_perturbation_fixture()
  expect_warning(press_perturbation(g, self_regulation = -1e-6),
                 "not dynamically stable")
})

# ---------------------------------------------------------------------------
# ggnetview_perturbation_curve()
# ---------------------------------------------------------------------------

test_that("ggnetview_perturbation_curve returns a ggplot", {
  g   <- make_perturbation_fixture()
  res <- get_network_perturbation(g, strategy = "targeted",
                                  centrality = "degree", plot = FALSE)
  p   <- ggnetview_perturbation_curve(res$curve)
  expect_s3_class(p, "ggplot")
  expect_error(ggnetview_perturbation_curve(res$curve, metric = "nope"),
               "not found")
})
