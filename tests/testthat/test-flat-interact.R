test_that("type of an interaction is the concatenation of component types.", {

  dir_1 = dt::discrete_intercept_repr(c(letters, letters), label = 'zozo')
  f_dir_1 = dt::flatten_discrete_intercept_repr(dir_1)
  dir_2 = dt::discrete_intercept_repr(c(letters, letters), label = 'zozo')
  f_dir_2 = dt::flatten_discrete_intercept_repr(dir_2)
  itr = dt::flat_interact(f_dir_1, f_dir_2)
  expect_equal(itr$type, paste(f_dir_1$type, f_dir_2$type, sep = ':::'))
  expect_equal(itr$format, 'row')


  dir_1 = dt::discrete_offset_repr(c(letters, letters), reference = 'q', label = 'zozo')
  f_dir_1 = dt::flatten_discrete_offset_repr(dir_1)
  dir_2 = dt::discrete_offset_repr(c(letters, letters), reference = 'z', label = 'zozo')
  f_dir_2 = dt::flatten_discrete_offset_repr(dir_2)
  itr = dt::flat_interact(f_dir_1, f_dir_2)
  expect_equal(itr$type, paste(f_dir_1$type, f_dir_2$type, sep = ':::'))
  expect_equal(itr$format, 'row')




})
