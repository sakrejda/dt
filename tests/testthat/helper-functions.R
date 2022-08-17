
  generate_random_matrix_with_zeros = function(N, K, max_value = 99, p = 0.6) {
    n_zero = rbinom(n = 1, size = N * K, prob = p)
    zero_row = sample.int(n = N, size = n_zero, replace = TRUE)
    zero_col = sample.int(n = K, size = n_zero, replace = TRUE)
    x = rbinom(n = N * K, size = max_value, prob = p)
    m = matrix(data = x, nrow = N, ncol = K)
    for (j in 1:n_zero) {
      m[zero_row[j], zero_col[j]] = 0
    }
    rownames(m) = rep('', nrow(m))
    colnames(m) = rep('', ncol(m))
    return(m)
  }

