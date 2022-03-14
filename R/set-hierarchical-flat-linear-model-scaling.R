#' Label certain parameter (by name or index) with an estimated scale
#'
#' @param m model to update
#' @param name parameter name to update
#' @param idx index(es) of entries to update
#'
#' @export
set_hierarchical_flat_linear_model_scaling = function(m, name, idx) {
  if (missing(idx)) {
    if (missing(name)) {
      stop("Either 'name' or 'idx' is required.")
    } else if (name %in% m$component_labels) {
      idx = m$component_col_ranges[[name]]
    } else {
      stop(glue::glue("'name' parameter ('{name}') not found in model.",
           name = name))
    }
  }
  if (name %in% m$component_labels && !is.null(name)) {
    idx = sort(idx)
  } else {
    stop(glue::glue("'name' parameter ('{name}') not found in model.",
                    name = name))
  }
  if (!all(diff(idx) == 1)) {
    stop("Hierarchical variances must be contiguous.")
  }
  m$n_hierarchical_variances = m$n_hierarchical_variances + 1
  m$hierarchical_sd_start = c(m$hierarchical_sd_start, min(idx))
  m$hierarchical_sd_n = c(m$hierarchical_sd_n, length(idx))
  return(m)
}
