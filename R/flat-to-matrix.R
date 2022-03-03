#' Turn flat matrix representation into a (dense) matrix
#'
#' @param x flat matrix representation from (e.g.) flatten_vector
#' @return dense matrix matching x
#'
#' @export
flat_to_matrix = function(x) {
  o = matrix(data = 0, nrow = x$N, ncol = x$K,
    dimnames = list(
      rows = x$row_names,
      parameters = x$col_names))
  for (i in 1:x$n_nze) {
    o[x$row_nze_idx[i], x$col_nze_idx[i]] = x$nze_value[i]
  }
  return(o)
}
