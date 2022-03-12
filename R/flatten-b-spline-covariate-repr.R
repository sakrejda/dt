#' Generate a flat matrix from the spline representation
#'
#' @param x spline representation
#' @return flat matrix representation
#'
#' @export
flatten_b_spline_covariate_repr = function(x) {
  if (x$type != 'b-spline-covariate') {
    msg = glue::glue("Function can only process a *spline* covariate.")
    rlang::abort(msg, component = x)
  }
  if (x$format != 'representation') {
    msg = glue::glue("Function can only process a *flat* spline covariate.")
    rlang::abort(msg, component = x)
  }
  o = x$basis(x$x, x$knots[1]) %>%
    matrix(ncol = 1) %>% row_flatten_matrix()
  if (x$K > 1) {
    for (k in 2:length(x$knots)) {
      col = x$basis(x$x, x$knots[k]) %>%
        matrix(ncol = 1) %>% row_flatten_matrix()
      o = cbind_flat_matrices(o, col)
    }
  }
  return(list(
    type = "b-spline-covariate",
    format = "row",
    N = x$N,
    K = x$K,
    n_nze = o$n_nze,
    nze_idx = o$nze_idx,
    yze_idx = o$yze_idx,
    nze_value = o$nze_value,
    row_start_idx = o$row_start_idx,
    row_n_nze = o$row_n_nze,
    col_nze_idx = o$col_nze_idx,
    row_nze_idx = o$row_nze_idx,
    row_names = x$row_names,
    col_names = x$col_names,
    label = x$label
  ))
}
