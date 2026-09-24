test_that("spline mask gives one closed polygon per cluster", {
  set.seed(1)
  d <- data.frame(x = rnorm(60), y = rnorm(60))
  cl <- rep(c("a", "b", "c"), each = 20)
  m <- generateMask_ggnetview(d, cl)
  expect_setequal(as.character(unique(m$cluster)), c("a", "b", "c"))
  expect_true(all(m$polygon_id == 1L))
  for (k in c("a", "b", "c")) {
    mk <- m[m$cluster == k, ]
    expect_equal(mk$x[1], mk$x[nrow(mk)])
    expect_equal(mk$y[1], mk$y[nrow(mk)])
  }
})

test_that("mask method switches between spline and kde", {
  set.seed(2)
  d <- data.frame(x = rnorm(30), y = rnorm(30))
  cl <- rep("a", 30)
  sp <- generateMask_ggnetview(d, cl, method = "spline")
  kd <- generateMask_ggnetview(d, cl, method = "kde", bandwidth_scale = 2)
  expect_gt(nrow(sp), 100L)
  expect_false(isTRUE(all.equal(sp$x, kd$x)))
  expect_error(generateMask_ggnetview(d, cl, method = "hull"))
})

test_that("ggNetView accepts module_outline_method", {
  expect_true("module_outline_method" %in% names(formals(ggNetView)))
  expect_identical(eval(formals(ggNetView)$module_outline_method), c("spline", "kde"))
})
