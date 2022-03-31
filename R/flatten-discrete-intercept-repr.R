#' Generate a flat matrix from the discrete intercept representation
#'
#' @param x discrete intercept representation
#' @return flat matrix representation
#'
#' @export
flatten_discrete_intercept_repr = function(x) {
  if (x$type != 'discrete-intercept') {
    msg = glue::glue("Function can only process a *discrete intercept* covariate.")
    rlang::abort(msg, component = x)
  }
  if (x$format != 'representation') {
    msg = glue::glue("Function can only process a *flat* spline covariate.")
    rlang::abort(msg, component = x)
  }
  n_nze = purrr::map_int(x$nze_per_col, length) %>% sum()
  nze_idx = vector(mode = 'numeric', length = n_nze)
  n_nze_per_col = purrr::map_int(x$nze_per_col, length)
  col_nze_idx = purrr::imap(x$nze_per_col, ~ rep(.y, length(.x))) %>%
    purrr::flatten_int()
  row_nze_idx = purrr::flatten_int(x$nze_per_col)
  row_order = order(row_nze_idx)
  row_nze_idx = row_nze_idx[row_order]
  col_nze_idx = col_nze_idx[row_order]
  nze_idx = x$K * (row_nze_idx - 1) + col_nze_idx
  nze_value = rep(1, length(nze_idx))
  row_start_idx = which(!duplicated(row_nze_idx))
  row_n_nze = tabulate(row_nze_idx)
  
  o = list(
    type = "discrete-intercept",
    format = "row",
    N = x$N,
    K = x$K,
    n_nze = n_nze,
    nze_idx = nze_idx,
    nze_value = nze_value,
    row_start_idx = row_start_idx,
    row_n_nze = row_n_nze,
    row_nze_idx = row_nze_idx,
    col_nze_idx = col_nze_idx,
    row_names = x$row_names,
    col_names = x$col_names,
    label = x$label
  )
  return(o)
}
