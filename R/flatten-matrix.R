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
  row_n_nze = tabulate(row_nze_idx)
  row_n_nze = row_n_nze[row_n_nze != 0]
  if (is.null(rownames(x))) {
    row_names = rep('', N)
  } else {
    row_names = rownames(x)
  }
  if (is.null(colnames(x))) {
    col_names = rep('', K)
  } else {
    col_names = colnames(x)
  }
  o = build_flat_model_component(
    type = "matrix",
    format = "row",
    N = N, K = K,
    n_nze = length(nze_idx),
    nze_idx = nze_idx,
    nze_value = nze_value,
    row_start_idx = which(!duplicated(row_nze_idx)),
    row_n_nze = row_n_nze,
    row_nze_idx = row_nze_idx,
    col_nze_idx = col_nze_idx,
    row_names = row_names,
    col_names = col_names,
    label = ''
  )
  return(o)
}
