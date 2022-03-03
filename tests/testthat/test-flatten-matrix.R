
test_that("a matrix can round-trip through flat form.", {
  n_tests = 20
  for (i in 1:n_tests) {
    N = sample.int(n = 1000, size = 1)
    K = sample.int(n = 50, size = 1) + 5
    m1 = generate_random_matrix_with_zeros(N, K, 99, 0.6)
    m2 = generate_random_matrix_with_zeros(N, K, 99, 0.9)
    fm1 = row_flatten_matrix(m1)
    fm2 = row_flatten_matrix(m2)
    rfm = cbind_flat_matrices(fm1, fm2)
    rm = flat_to_matrix(rfm)
    expect_equivalent(cbind(m1, m2), rm)
  }
})
