test_that("a vector can round-trip through flat form.", {
  N = 45
  K = 1
  n_tests = 20
  for (i in 1:n_tests) {
    n_zero = rbinom(n = 1, size = N * K, prob = 0.6)
    zero_row = sample.int(n = N, size = n_zero, replace = TRUE)
    zero_col = sample.int(n = K, size = n_zero, replace = TRUE)
    m = matrix(data = rnorm(N * K), nrow = N, ncol = K)
    for (j in 1:n_zero) {
      m[zero_row[j], zero_col[j]] = 0
    }
    fm = flatten_vector(m[,1])
    rm = flat_to_matrix(fm)
    expect_equivalent(m, rm)
  }
})

test_that("a matrix can round-trip through flat form.", {
  N = 45
  K = 20
  n_tests = 20
  for (i in 1:n_tests) {
    n_zero = rbinom(n = 1, size = N * K, prob = 0.6)
    zero_row = sample.int(n = N, size = n_zero, replace = TRUE)
    zero_col = sample.int(n = K, size = n_zero, replace = TRUE)
    m = matrix(data = rnorm(N * K), nrow = N, ncol = K)
    for (j in 1:n_zero) {
      m[zero_row[j], zero_col[j]] = 0
    }
    fm = row_flatten_matrix(m)
    rm = flat_to_matrix(fm)
    expect_equivalent(m, rm)
  }
})
