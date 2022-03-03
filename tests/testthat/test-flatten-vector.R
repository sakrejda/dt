test_that("flatten_vector yields a sparse vector representation", {
  N = 52
  n_tests = 20
  for (i in 1:n_tests) {
    ov = rnorm(N)
    yze = sample.int(n = N - 1, size = round(0.5 * N))
    ov[yze] = 0
    fv = flatten_vector(ov)
    expect_equal(fv$n_nze, N - length(yze))
    expect_equal(fv$nze_value, ov[!(1:N %in% yze)])
    zev = vector(mode = 'numeric', length = N)
    zev[fv$nze_idx] = fv$nze_value
    nze = (1:N)[!(1:N %in% yze)]
    expect_equal(zev, ov)
    expect_equal(length(ov), fv$N)
    expect_equal(fv$K, 1)
    expect_equal(fv$row_start_idx, 1:length(nze))
    expect_equal(fv$row_n_nze, rep(1, length(nze)))
    expect_equal(fv$col_nze_idx, rep(1, length(nze)))
    expect_equal(fv$row_nze_idx, nze)
  }
})
