
#' Combine two matrices into a single (flat) matix
#'
#' @param a first flat matrix
#' @param b second flat matrix
#' @param ... additional flat matrices
#' @return flat (row-wise) matrix, cbind(a,b)
#'
#' @export
cbind_flat_matrices = function(a, ...) {
  args = list(...)
  if (length(args) == 0) {
    return(a)
  } else {
    b = args[[1]]
  }
  if (a$N != b$N) {
    msg = glue::glue("Function can only process",
                     "matrices with matching row counts.")
    rlang::abort(msg, a = a, b = b)
  }

  b$col_nze_idx = b$col_nze_idx + a$K
  o = list(
    N = a$N,
    K = a$K + b$K,
    n_nze = a$n_nze + b$n_nze,
    nze_idx = c(a$nze_idx, b$nze_idx),
    nze_value = c(a$nze_value, b$nze_value),
    row_nze_idx = c(a$row_nze_idx, b$row_nze_idx),
    col_nze_idx = c(a$col_nze_idx, b$col_nze_idx))
  nze_row_order = order(o$row_nze_idx)
  new_nze_value = vector(mode = 'numeric', length = o$n_nze)
  new_row_nze_idx = vector(mode = 'numeric', length = o$n_nze)
  new_col_nze_idx = vector(mode = 'numeric', length = o$n_nze)
  for (i in seq_along(nze_row_order)) {
    j = nze_row_order[i]
    new_nze_value[i] = o$nze_value[j]
    new_row_nze_idx[i] = o$row_nze_idx[j]
    new_col_nze_idx[i] = o$col_nze_idx[j]
  }
  o$nze_idx = (new_row_nze_idx - 1) * o$K + new_col_nze_idx
  o$nze_value = new_nze_value
  o$col_nze_idx = new_col_nze_idx
  o$row_nze_idx = new_row_nze_idx
  o$row_start_idx = which(!duplicated(o$row_nze_idx))
  o$row_n_nze = tabulate(o$row_nze_idx)
  o$row_n_nze = o$row_n_nze[o$row_n_nze != 0]
  if (length(args) == 1) {
    return(o)
  } else {
    args[[1]] = o
    for (i in 2:length(args)) {
      args[[1]] = cbind_flat_matrices(args[[1]], args[[i]])
    }
    return(args[[1]])
  }
}
