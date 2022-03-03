#' Flat representation of a vector
#'
#' @param x vector to flatten
#' @return row-wise flat vector representation
#'
#' @export
flatten_vector = function(x) {
  nze_idx = which(x != 0)
  o = list(
    N = length(x), K = 1,
    n_nze = length(nze_idx),
    nze_idx = nze_idx,
    yze_idx = setdiff(1:length(x), nze_idx),
    nze_value = x[nze_idx],
    row_start_idx = 1:length(nze_idx),
    row_n_nze = rep(1, length(nze_idx)),
    col_nze_idx = rep(1, length(nze_idx)),
    row_nze_idx = nze_idx)
  return(o)
}
