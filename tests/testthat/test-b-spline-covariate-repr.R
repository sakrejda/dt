test_that("b-spline covariate representation is correctly encoded.", {
  x = seq(from = -10, to = 10, by = 0.01)
  K = 13
  knots = dt:::knot_placer_basic_interval(lb = -5, ub = 5, K = K)
  o = dt::b_spline_covariate_repr(x, knots, label = 'zozo')
  expect_equal(o$format, "representation")
  expect_equal(o$type, "b-spline-covariate")
  cn = paste('zozo', 'knot', 1:K, sep = '::')
  expect_equal(o$col_names, cn)
  expect_equal(o$row_names, as.character(1:o$N))
  expect_equal(o$N, length(x))
  expect_equal(o$K, length(knots))
})
