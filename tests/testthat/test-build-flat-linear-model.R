test_that("a two-component linear model can be build.", {
  x = seq(from = -10, to = 10, by = 0.01)
  K = 10
  knots = dt:::knot_placer_basic_interval(lb = -5, ub = 5, K = K)

  or_bs = dt::b_spline_covariate_repr(x, knots, label = 'zozo')
  f_or_bs = dt::flatten_b_spline_covariate_repr(or_bs)

  groups = sample(letters, length(x), replace = TRUE)
  or_di = dt::discrete_intercept_repr(groups, label = 'ququ')
  f_or_di = dt::flatten_discrete_intercept_repr(or_di)

  model = dt::build_flat_linear_model(f_or_di, f_or_bs)

  expect_equal(model$component_count, 2)
  expect_equal(model$component_formats, c(f_or_di$format, f_or_bs$format))
  expect_equal(model$component_types, c(f_or_di$type, f_or_bs$type))
  expect_equal(model$component_row_count, c(f_or_di$N, f_or_bs$N))
  expect_equal(model$component_col_count, c(f_or_di$K, f_or_bs$K))
  expect_equal(model$component_col_names, c(f_or_di$col_names, f_or_bs$col_names))
  expect_equal(model$component_col_end_offset, c(f_or_di$K, f_or_di$K + f_or_bs$K))
  expect_equal(model$component_col_start_offset, c(1, f_or_di$K + 1))
  expect_equal(model$component_col_ranges, 
    list(ququ = 1:f_or_di$K, zozo = f_or_di$K + (1:f_or_bs$K)))
  expect_equal(model$N, unique(c(f_or_di$N, f_or_bs$N)))
  expect_equal(model$K, f_or_di$K + f_or_bs$K)
  expect_equal(model$flat_matrix, cbind_flat_matrices(f_or_di, f_or_bs))
})
