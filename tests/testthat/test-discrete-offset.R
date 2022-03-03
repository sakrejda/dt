test_that("discrete offset is tagged correctly", {
  o = dt::discrete_offset(c(letters, letters), 'q', label = 'zozo')
  expect_equal(o$format, "matrix")
  expect_equal(o$type, "discrete-offset")
})

test_that("discrete offset has the right levels and dimensions as matrix", {
  o = dt::discrete_offset(c(letters, letters), 'q', label = 'zozo')$matrix
  cn = paste('zozo', 'level', letters[letters != 'q'], sep = '::')
  expect_equal(colnames(o), cn)
  expect_equal(rownames(o), as.character(1:52))
  expect_equal(dim(o), c(52, 25)) 
  q_spot = 0
  for (i in 1:25) {
    q_spot = q_spot + (i == which(letters == 'q'))
    expect_equal(o[i + q_spot, i], 1)
    expect_equal(o[i + 26 + q_spot, i], 1)
  }
})
