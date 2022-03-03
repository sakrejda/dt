
test_that("linear covariate is tagged correctly", {
  o = dt::linear_covariate(rnorm(33), label = 'zozo')
  expect_equal(o$format, "matrix")
  expect_equal(o$type, "linear-covariate")
})

test_that("linear covariate is correctly encoded.", {
  x = rnorm(32)
  om = dt::linear_covariate(x, label = 'zozo')$matrix
  expect_equal(colnames(om), 'zozo')
  expect_equal(dim(om), c(32, 1))
  expect_equal(rownames(om), as.character(1:32))
  expect_equal(as.vector(om), x)
})


