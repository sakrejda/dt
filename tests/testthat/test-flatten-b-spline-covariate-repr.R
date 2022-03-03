test_that("b-spline covariate representation can be flattened to the correct (flat) matrix.", {
  x = seq(from = -10, to = 10, by = 1)
  #x = seq(from = -10, to = 10, by = 0.01)
  K = 10
  knots = dt:::knot_placer_basic_interval(lb = -5, ub = 5, K = K)
  o = dt::b_spline_covariate(x, knots, label = 'zozo')
  om = o$matrix
  f_om = row_flatten_matrix(om)

  or = b_spline_covariate_repr(x, knots, label = 'zozo')
  f_or = flatten_b_spline_covariate_repr(or)
  expect_equal(f_om$format, f_or$format)
  expect_equal(f_om$N, f_or$N)
  expect_equal(f_om$K, f_or$K)
  expect_equal(f_om$nze_value, f_or$nze_value)
  expect_equal(f_om$n_nze, f_or$n_nze)
  expect_equal(f_om$nze_idx, f_or$nze_idx)
  expect_equal(f_om$row_nze_idx, f_or$row_nze_idx)
  expect_equal(f_om$col_nze_idx, f_or$col_nze_idx)
  expect_equal(flat_to_matrix(f_om),
               flat_to_matrix(f_or))
  expect_equal(flat_to_matrix(f_om), om)
})
