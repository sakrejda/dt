#' Construct a complete flat model component
#'
#' Enforces some of the list structure that's implicit currently
#'
#' @param type character string listing the component type
#' @param format character string listing the format ('row', always)
#' @param N single integer, specify the number of observations in the
#'        component.
#' @param K single integer, specify the number of parameters in the model
#'        component
#' @param n_nze, single integer, specify the number of non-zero elements in
#'        the flat representation of the component model matrix
#' @param nze_idx, vector of integers, specify the indexes of non-zero entries
#'        in the vector of non-zero values
#' @param nze_value, vector of non-zero values (floating point numbers)
#' @param row_start_idx, vector of integers, each indicates the start of a row
#'        in the vector of non-zero values
#' @param row_n_nze, number of non-zero entries in each row
#' @param row_nze_idx, row of each entry in the vector of non-zero values
#' @param col_nze_idx, col of each entry in the vector of non-zero values
#' @param row_names, row names for the model component
#' @param col_names, column names for the model component
#' @param label, model component label
#' @param reference, reference category relevant for 'discrete-offset' type
#'        only.
#' @return flat model component representation 
#'
#' @export
build_flat_model_component = function(
  type, 
  format = 'row', 
  N, K, 
  n_nze, nze_idx, nze_value, row_start_idx,
  row_n_nze, row_nze_idx, col_nze_idx, 
  row_names, col_names, 
  label, ...
) {
  components = c('type', 'format', 'N', 'K', 
    'n_nze', 'nze_idx', 'nze_value', 
    'row_start_idx', 
    'row_n_nze', 'row_nze_idx', 'col_nze_idx', 
    'row_names', 'col_names', 'label')
  o = list(type = type, format = format,
    N = N, K = K, n_nze = n_nze, 
    nze_idx = nze_idx, 
    nze_value = nze_value, 
    row_start_idx = row_start_idx,
    row_n_nze = row_n_nze, 
    row_nze_idx = row_nze_idx, 
    col_nze_idx = col_nze_idx,
    row_names = row_names, col_names = col_names,
    label = label)
  extra_args = list(...)
  if ('reference' %in% names(extra_args)) {
    o[['reference']] = extra_args$reference
  }
  if (!isTRUE(all(components %in% names(o)))) {
    missed = components[!(components %in% names(o))]
    msg = paste0("\nMissing args of model component.",
      "\nExpected:\n", paste0("\t", components, "\n", collapse = ''),
      "\nMissing:\n", paste0("\t", missed, "\n", collapse = ''))
    rlang::abort(message = msg, missed = missed)
  }
  if (isTRUE(any(purrr::map_lgl(o, ~ any(is.null(.x)))))) {
    null_valued = names(o)[purrr::map_lgl(o, is.null)]
    msg = paste0("\nNULL args in model component.",
      "\nNULL valued args:\n", paste0("\t", null_valued, "\n", collapse = ''))
    rlang::abort(message = msg, null_valued = null_valued)
  }
  if (isTRUE(any(purrr::map_lgl(o, ~ any(is.na(.x)))))) {
    na_valued = names(o)[purrr::map_lgl(o, is.na)]
    msg = paste0("\nNA args in model component.",
      "\nNA valued args:\n", paste0("\t", na_valued, "\n", collapse = ''))
    rlang::abort(message = msg, na_valued = na_valued)
  }
  return(o)
}


