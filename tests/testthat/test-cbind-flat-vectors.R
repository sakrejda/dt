test_that("a two-column matrix can round-trip through flat form and cbind.", {
  n_tests = 5
  for (i in 1:n_tests) {
    N = sample.int(n = 1000, size = 1)
    K = 2
    n_zero = rbinom(n = 1, size = N * K, prob = 0.6)
    zero_row = sample.int(n = N, size = n_zero, replace = TRUE)
    zero_col = sample.int(n = K, size = n_zero, replace = TRUE)
    m = matrix(data = rnorm(N * K), nrow = N, ncol = K)
    for (j in 1:n_zero) {
      m[zero_row[j], zero_col[j]] = 0
    }
    a = flatten_vector(m[,1])
    b = flatten_vector(m[,2])
    fab = cbind_flat_vectors(a, b)
    rm = flat_to_matrix(fab)
    expect_equivalent(m, rm)
  }
})
