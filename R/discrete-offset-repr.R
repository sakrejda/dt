#' Represent a simple discrete offset representation
#'
#' @param x value to use for offset
#' @param reference value defined as non-effect
#' @param label 1-length character vector used as prefix for column names
#' @return a representation of the covariate 
#' @export
discrete_offset_repr = function(
  x, 
  reference,
  label = "x"
) {
  if (missing(reference)) {
    msg = glue::glue("Argument 'reference' must name a value present in argument 'x'.",
      label)
    rlang::abort(msg)
  }
  unique_values = sort(unique(x))
  if (reference %in% unique_values) {
    offset_values = unique_values[unique_values != reference]
  } else {
    msg = glue::glue("Argument 'reference' must name a value present in argument 'x'.",
      "Offset labelled as '{label}' does not contain the value '{reference}'.", 
      label, reference)
    rlang::abort(msg)
  }
  nze_per_col = list()
  for (k in seq_along(offset_values)) {
    nze_per_col[[k]] = which(x == offset_values[k])
  }
  return(list(
    type = "discrete-offset",
    format = "representation",
    nze_per_col = nze_per_col,
    N = length(x),
    K = length(offset_values),
    row_names = as.character(1:length(x)),
    col_names = paste(label, "level", offset_values, sep = "::")))
}
