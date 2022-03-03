#' Pick out a column from a flat matrix
#'
#' @param m matrix
#' @param j index of column to pick
#' @return m
#' 
#' @export
flat_pick_matrix_column = function(m, j) {
  if (is.character(j)) {
    j = which(m$col_names == j)
    if (length(j) > 1) {
      rlang::abort("Picking multiple columns is not implemented.")
    }
  }
  if (m$K == 0) {
    return(m)
  }
  keep = which(m$col_nze_idx == j)
  o = list(
    N = m$N,
    K = 1,
    type = m$type,
    format = m$format,
    n_nze = length(keep),
    nze_idx = m$nze_idx[keep],
    nze_value = m$nze_value[keep],
    row_start_idx = 1:length(keep),
    row_n_nze = rep(1, length(keep)),
    row_nze_idx = m$row_nze_idx[keep],
    col_nze_idx = rep(1, length(keep)),
    row_names = m$row_names,
    col_names = m$col_names[j])
  return(o)
}
