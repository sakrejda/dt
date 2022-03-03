

test_that("discrete intercept repr has the same levels and dimensions as matrix", {
  om = dt::discrete_intercept(c(letters, letters), label = 'zozo')$matrix
  or = dt::discrete_intercept_repr(c(letters, letters), label = 'zozo')
  expect_equal(colnames(om), or$col_names)
  expect_equal(rownames(om), or$row_names)
  expect_equal(dim(om), c(or$N, or$K))
  for (i in 1:or$K) {
    expect_equivalent(which(om[,i] == 1), or$nze_per_col[[i]])
  }
})

test_that("discrete intercept repr has the right levels and dimensions.", {
  or = dt::discrete_intercept_repr(c(letters, letters), label = 'zozo')
  expect_equal(or$type, "discrete-intercept")
  expect_equal(or$format, 'representation')
  cn = paste('zozo', 'level', letters, sep = '::')
  expect_equal(or$col_names, cn)
  expect_equal(or$row_names, as.character(1:52))
  expect_equivalent(c(or$N, or$K), c(52, 26)) 
  for (i in 1:26) {
    expect_equal(or$nze_per_col[[i]], c(i, i + 26))
  }
})


