
#' List representation of linear covariate
#'
#' @param x covariate representation
#' @return flat version
#'
#' @export
flatten_linear_covariate_repr = function(x) {
  if (x$type != 'linear-covariate') {
    msg = glue::glue("Function can only process a *linear* covariate.")
    rlang::abort(msg, component = x)
  }
  if (x$format != 'representation') {
    msg = glue::glue("Function can only process a *flat* linear covariate.")
    rlang::abort(msg, component = x)
  }
  if (x$K != 1) {
    msg = glue::glue("A linear covariate must generate a single column.")
    rlang::abort(msg, component = x)
  }
  fv = flatten_vector(x$x)
  fc = list(
    type = 'linear-covariate',
    format = 'row',
    N = x$N,
    K = 1,
    n_nze = fv$n_nze,
    nze_idx = fv$nze_idx,
    yze_idx = fv$yze_idx,
    nze_value = fv$nze_value,
    row_start_idx = fv$row_start_idx,
    row_n_nze = fv$row_n_nze,
    row_nze_idx = fv$row_nze_idx,
    col_nze_idx = fv$col_nze_idx,
    row_names = x$row_names,
    col_names = x$col_names,
    label = x$label
  )
  return(fc)
}
