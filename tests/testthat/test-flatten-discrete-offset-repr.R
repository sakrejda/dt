

test_that("discrete offset repr can be flattened correctly.", {
  om = dt::discrete_offset(c(letters, letters), reference = 'd', label = 'zozo')$matrix
  f_om = dt::row_flatten_matrix(om)
  or = dt::discrete_offset_repr(c(letters, letters), reference = 'd', label = 'zozo')
  f_or = dt::flatten_discrete_offset_repr(or)
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

