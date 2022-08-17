#' Flat representation of a vector
#'
#' @param x vector to flatten
#' @return row-wise flat vector representation
#'
#' @export
flatten_vector = function(x) {
  nze_idx = which(x != 0)
  N = length(x)
  o = build_flat_model_component(
    type = 'simple-vector',
    N = N, K = 1,
    n_nze = length(nze_idx),
    nze_idx = nze_idx,
    nze_value = x[nze_idx],
    row_start_idx = 1:length(nze_idx),
    row_n_nze = rep(1, length(nze_idx)),
    col_nze_idx = rep(1, length(nze_idx)),
    row_nze_idx = nze_idx,
    row_names = rep('', N),
    col_names = '',
    label = '')
  return(o)
}
