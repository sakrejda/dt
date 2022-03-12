
test_that("multiple matrices can be col-bound together.", {
  n_tests = 20
  for (i in 1:n_tests) {
    n_matrices = i
    ms = list()
    N = sample.int(n = 100, size = 1)
    for (j in 1:n_matrices) {
      K = sample.int(n = 50, size = 1) + 5
      ms[[j]] = generate_random_matrix_with_zeros(N, K, 99, 0.6)
    }
    fms = purrr::map(ms, dt::row_flatten_matrix)
    bfms = purrr::lift_dl(dt::cbind_flat_matrices)(fms)
    bms = dt::flat_to_matrix(bfms)
    expect_equivalent(purrr::lift_dl(cbind)(ms), bms)
  }
})
