
test_that("a two-component linear model can be build.", {
  x = seq(from = -10, to = 10, by = 0.01)
  K = 10
  knots = dt:::knot_placer_basic_interval(lb = -5, ub = 5, K = K)
  rm(K)
  
  or_bs = dt::b_spline_covariate_repr(x, knots, label = 'zozo')
  f_or_bs = dt::flatten_b_spline_covariate_repr(or_bs)

  groups = sample(letters, length(x), replace = TRUE)
  or_di = dt::discrete_intercept_repr(groups, label = 'ququ')
  f_or_di = dt::flatten_discrete_intercept_repr(or_di)

  model = dt::build_flat_linear_model(f_or_di, f_or_bs) %>%
    dt::set_hierarchical_flat_linear_model_scaling('ququ') %>%
    dt::set_hierarchical_flat_linear_model_scaling('zozo')

  expect_equal(model$n_hierarchical_variances, 2)
  expect_equal(model$hierarchical_sd_start, c(1, 27))
  expect_equal(model$hierarchical_sd_n, c(26, 10))
})
