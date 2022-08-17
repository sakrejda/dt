
#' Label certain parameter (by name or index) with a constant scale
#'
#' @param m model to update
#' @param name parameter name to update
#' @param idx index(es) of entries to update
#' @param scale scaling constant
#'
#' @export
set_flat_linear_model_scaling = function(m, name, idx, scale) {
  if (missing(idx)) {
    if (missing(name)) {
      stop("Either 'name' or 'idx' is required.")
    } else if (name %in% m$component_labels) {
      idx = m$component_col_ranges[[name]]
    } else {
      stop(paste0("'name' parameter, '", name, "'is not present in the model."))
    }
  }
  if (length(scale) != 1 && length(scale) != length(idx)) {
    stop("'scale' parameter must be length 1 or contain one entry per column.")
  }
  m$component_col_scaling[idx] = scale
  return(m)
}
