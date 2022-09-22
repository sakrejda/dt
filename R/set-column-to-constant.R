
#' Label certain parameter (by name or index) as one that is *not* 
#' estimated. It's parameter will be fixed to 1.0
#'
#' @param m model to update
#' @param name parameter name to update
#' @param idx index(es) of entries to update
#'
#' @export
set_column_to_constant = function(m, name, idx) {
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
  m$component_col_constant = c(m$component_col_constant, idx) |> sort()
  m$component_col_estimated = setdiff(1:m$K, m$component_col_constant)

  return(m)
}
