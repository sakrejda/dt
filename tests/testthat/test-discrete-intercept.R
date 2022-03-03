

test_that("discrete intercept is tagged correctly", {
  o = dt::discrete_intercept(c(letters, letters), label = 'zozo')
  expect_equal(o$format, "matrix")
  expect_equal(o$type, "discrete-intercept")
})

test_that("discrete intercept has the right levels and dimensions as matrix", {
  o = dt::discrete_intercept(c(letters, letters), label = 'zozo')$matrix
  cn = paste('zozo', 'level', letters, sep = '::')
  expect_equal(colnames(o), cn)
  expect_equal(rownames(o), as.character(1:52))
  expect_equal(dim(o), c(52, 26)) 
  for (i in 1:26) {
    expect_equal(o[i, i], 1)
    expect_equal(o[i + 26, i], 1)
  }
})

