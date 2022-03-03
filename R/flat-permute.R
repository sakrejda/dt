
#' Combine two flat linear model components by column-wise permutation
#' / product
#'
#' e.g.-an interaction
#'
#' @param a, a first model components
#' @param b, a second model component
#' @return permutation of submodel columns
#'
#' @export
flat_permute = function(a, b) {
  if (a$N != b$N) {
    msg = glue::glue(
      "Attempted to combine components with different row counts:\n",
      "Component 'a': {a$N}\n", "Component 'b': {b$N}\n")
    rlang::abort(msg, class = 'error-mismatched-component-row-counts',
      a = a, b = b)
  }
  if (b$K == 0) {
    return(list(type = "empty", format = NA, N = a$N, K = 0))
  }
  if (a$K == 0) {
    return(list(type = "empty", format = NA, N = b$N, K = 0))
  }
  o = purrr::map(1:b$K, ~ flat_matrix_times_column_vector(a, flat_pick_matrix_column(b, .x)))
  if (b$K > 1) {
    for (i in 2:b$K) {
      o[[1]] = cbind_flat_matrices(o[[1]], o[[i]])
    }
  }
  return(o[[1]])
}
