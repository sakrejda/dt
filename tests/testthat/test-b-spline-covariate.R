test_that("b-spline covariate is correctly encoded.", {
  x = seq(from = -10, to = 10, by = 0.01)
  K = 13
  knots = dt:::knot_placer_basic_interval(lb = -5, ub = 5, K = K)
  o = dt::b_spline_covariate(x, knots, label = 'zozo')
  expect_equal(o$format, "matrix")
  expect_equal(o$type, "b-spline-covariate")
  om = o$matrix
  cn = paste('zozo', 'knot', 1:K, sep = '::')
  expect_equal(colnames(om), cn)
  idx = which(x > min(knots[2]) & x < max(knots[K-1]))
  max_error = max(abs(rowSums(om)[idx] - 1))
  expect_lt(max_error, 1e-15)
})
