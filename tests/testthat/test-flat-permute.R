
test_that("we can permute and column-wise multiple two matrices in flat form.", {
  n_tests = 20
  for (i in 1:n_tests) {
    N = sample.int(n = 100, size = 1)
    K1 = sample.int(n = 50, size = 1)
    m1 = generate_random_matrix_with_zeros(N, K1, 99, 0.6)
    K2 = sample.int(n = 5, size = 1) + 5
    m2 = generate_random_matrix_with_zeros(N, K2, 99, 0.6)
    m1_p_m2 = matrix(data = 0, nrow = N, ncol = K1 * K2)
    j = 0
    for (k2 in 1:K2) {
      for (k1 in 1:K1) {
        j = j + 1
        m1_p_m2[,j] = m2[,k2] * m1[,k1]
      }
    }
    f_m1_p_m2 = row_flatten_matrix(m1_p_m2)
    f_fm1_p_fm2 = flat_permute(row_flatten_matrix(m1), row_flatten_matrix(m2))
    expect_equal(f_m1_p_m2$N, f_fm1_p_fm2$N)
    expect_equal(f_m1_p_m2$K, f_fm1_p_fm2$K)
    expect_equal(f_m1_p_m2$n_nze, f_fm1_p_fm2$n_nze)
    expect_equal(f_m1_p_m2$nze_idx, f_fm1_p_fm2$nze_idx)
    expect_equal(f_m1_p_m2$nze_value, f_fm1_p_fm2$nze_value)
    expect_equal(f_m1_p_m2$row_start_idx, f_fm1_p_fm2$row_start_idx)
    expect_equal(f_m1_p_m2$row_n_nze, f_fm1_p_fm2$row_n_nze)
    expect_equal(f_m1_p_m2$col_nze_idx, f_fm1_p_fm2$col_nze_idx)
    expect_equal(f_m1_p_m2$row_nze_idx, f_fm1_p_fm2$row_nze_idx)
  }
})


