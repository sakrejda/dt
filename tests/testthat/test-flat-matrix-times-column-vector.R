test_that("can pick out a flat matrix column.", {
  n_tests = 20
  for (i in 1:n_tests) {
    N = sample.int(n = 1000, size = 1)
    K = sample.int(n = 50, size = 1) + 5
    m1 = generate_random_matrix_with_zeros(N, K, 99, 0.6)
    j = sample.int(n = K, size = 1)
    m2 = m1
    c1 = m1[,j]
    for (k in 1:K) {
      m2[,k] = m2[,k] * c1
    }
    fm1 = row_flatten_matrix(m1)
    fm2 = row_flatten_matrix(m2)
    fc1 = flat_pick_matrix_column(fm1, j)
    fm1 = flat_matrix_times_column_vector(fm1, fc1)
    expect_equivalent(fm1, fm2)
  }
})
