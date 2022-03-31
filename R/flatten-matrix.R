#' Flat representation of a matrix, primarily for testing
#'
#' @param x matrix to flatten
#' @return row-wise flat matrix representation
#'
#' @export
row_flatten_matrix = function(x) {
  N = nrow(x)
  K = ncol(x)
  nze_idx = vector(mode = 'numeric', length = 0)
  yze_idx = vector(mode = 'numeric', length = 0)
  nze_value = vector(mode = 'numeric', length = 0)
  row_nze_idx = vector(mode = 'numeric', length = 0)
  col_nze_idx = vector(mode = 'numeric', length = 0)
  idx = 0:(N * K - 1)
  row = (idx %/% K) + 1
  col = (idx %% K) + 1
  nze_idx = which(t(x) != 0)
  row_nze_idx = row[nze_idx]
  col_nze_idx = col[nze_idx]
  nze_value = t(x)[nze_idx]
  o = list(
    type = "matrix",
    format = "row",
    N = nrow(x), K = ncol(x),
    n_nze = length(nze_idx),
    nze_idx = nze_idx,
    yze_idx = yze_idx,
    nze_value = nze_value,
    row_start_idx = which(!duplicated(row_nze_idx)),
    row_n_nze = tabulate(row_nze_idx),
    col_nze_idx = col_nze_idx,
    row_nze_idx = row_nze_idx,
    row_names = rownames(x),
    col_names = colnames(x)
  )
  return(o)
}
