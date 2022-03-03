
test_that("discrete offset repr has the same levels and dimensions as matrix", {
  om = dt::discrete_offset(c(letters, letters), 'q', label = 'zozo')$matrix
  or = dt::discrete_offset_repr(c(letters, letters), 'q', label = 'zozo')
  expect_equal(or$type, "discrete-offset")
  expect_equal(or$format, 'representation')
  expect_equal(colnames(om), or$col_names)
  expect_equal(rownames(om), or$row_names)
  expect_equal(dim(om), c(or$N, or$K))
  for (i in 1:or$K) {
    expect_equivalent(which(om[,i] == 1), or$nze_per_col[[i]])
  }
})

test_that("discrete offset repr has the right levels and dimensions.", {
  or = dt::discrete_offset_repr(c(letters, letters), 'q', label = 'zozo')
  cn = paste('zozo', 'level', letters[letters != 'q'], sep = '::')
  expect_equal(or$col_names, cn)
  expect_equal(or$row_names, as.character(1:52))
  expect_equivalent(c(or$N, or$K), c(52, 25)) 
  q_spot = 0
  for (i in 1:25) {
    q_spot = q_spot + (i == which(letters == 'q'))
    expect_equal(or$nze_per_col[[i]], c(i + q_spot, i + 26 + q_spot))
  }
})


