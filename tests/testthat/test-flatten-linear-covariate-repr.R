
test_that("linear covariate representation is correctly flattened.", {
  x = rnorm(32)
  or = dt::linear_covariate_repr(x, label = 'zozo')
  f_or = dt:::flatten_linear_covariate_repr(or)
  expect_equal(or$col_names, f_or$col_names)
  expect_equal(c(or$N, or$K), c(f_or$N, f_or$K))
  expect_equal(or$row_names, f_or$row_names)
  expect_equal(f_or$nze_value, x[x != 0])
  
})

test_that("linear covariate representation is correctly flattened (include zero elements)", {
  x = rnorm(32)
  x[c(1, 7, 20)] = 0
  or = dt::linear_covariate_repr(x, label = 'zozo')
  f_or = dt:::flatten_linear_covariate_repr(or)
  expect_equal(c(or$N, or$K), c(f_or$N, f_or$K))
  expect_equal(f_or$n_nze, length(x[x != 0]))
  expect_equal(f_or$nze_idx, which(x != 0))
  expect_equal(f_or$yze_idx, which(x == 0))
  expect_equal(f_or$nze_value, x[x != 0])
  expect_equal(f_or$row_start_idx, 1:29)
  expect_equal(f_or$row_n_nze, rep(1, 29))
  expect_equal(f_or$row_nze_idx, which(x != 0))
  expect_equal(f_or$col_nze_idx, rep(1, f_or$n_nze))
  expect_equal(or$row_names, f_or$row_names)
  expect_equal(or$col_names, f_or$col_names)
  
})
