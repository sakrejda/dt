
test_that("linear covariate representation is correctly encoded", {
  x = rnorm(32)
  or = dt::linear_covariate_repr(x, label = 'zozo')
  expect_equal(or$type, 'linear-covariate')
  expect_equal(or$format, 'representation')
  expect_equal(or$col_names, 'zozo')
  expect_equal(c(or$N, or$K), c(32, 1))
  expect_equal(or$row_names, as.character(1:32))
  expect_equal(or$x, x)
  
})
