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
    component_labels = purrr::map(args, `[[`, 'label'),
    component_count = length(args),
    component_types = purrr::map_chr(args, `[[`, 'type'),
    component_formats = purrr::map_chr(args, `[[`, 'format'),
    component_is_permutation = purrr::map_chr(args, `[[`, 'type') %>%
      stringr::str_detect(':::'),
    component_row_count = purrr::map(args, `[[`, 'N') %>% purrr::map_int(as.integer),
    component_col_count = purrr::map(args, `[[`, 'K') %>% purrr::map_int(as.integer),
    component_col_end_offset = purrr::map(args, `[[`, 'K') %>% 
      purrr::map_int(as.integer) %>% cumsum()
  )
  flat_model$component_col_names = purrr::map(args, `[[`, 'col_names') %>%
    purrr::flatten_chr()
  flat_model$component_col_start_offset = flat_model$component_col_end_offset -
    flat_model$component_col_count + 1
  flat_model$component_col_ranges = purrr::map2(
    flat_model$component_col_start_offset, 
    flat_model$component_col_end_offset, ~ .x:.y)
  names(flat_model$component_col_ranges) = flat_model$component_labels
  flat_model$N = unique(flat_model$component_row_count)
  if (length(flat_model$N) != 1) {
    msg = glue::glue(
      "Attempted to combine components with different row counts:\n")
    rlang::abort(msg, class = 'error-mismatched-component-row-counts',
      flat_model = flat_model)
  }
  flat_model$K = sum(flat_model$component_col_count)
  flat_model$flat_matrix = purrr::lift_dl(cbind_flat_matrices)(args)
  flat_model$component_col_scaling = rep(1, flat_model$K)
  flat_model$n_hierarchical_variances = 0
  flat_model$hierarchical_sd_start = numeric()
  flat_model$hierarchical_sd_n = numeric()
  flat_model$component_col_constant = numeric()
  flat_model$component_col_estimated = 1:flat_model$K
  return(flat_model)
}
