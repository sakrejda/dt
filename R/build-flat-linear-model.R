#' (Lazily) build a model from its components
#'
#' @param ... any components for a single model
#' @return (lazy) model representation
#'
#' @export
build_flat_linear_model = function(
  ...
) {
  args = list(...)
  flat_model = list(
    component_count = length(args),
    component_types = purrr::map_chr(args, `[[`, 'type'),
    component_formats = purrr::map_chr(args, `[[`, 'format'),
    component_is_permutation = purrr::map_chr(args, `[[`, 'type') %>%
      stringr::str_detect(.x, ':::'),
    component_row_count = purrr::map_int(`[[`, 'N'),
    component_col_count = purrr::map_int(`[[`, 'K'),
    component_col_end_offset = purrr::map_int(`[[`, 'K') %>% cumsum()
  )
  flat_model$component_col_start_offset = flat_model$component_col_end_offset -
    flat_model$component_col_count + 1
  flat_model$N = unique(flat_model$component_row_count)
  if (length(flat_model$N) != 1) {
    msg = glue::glue(
      "Attempted to combine components with different row counts:\n")
    rlang::abort(msg, class = 'error-mismatched-component-row-counts',
      flat_model = flat_model)
  }
  flat_model$K = sum(flat_model$component_col_count)
  return(flat_model)
}
