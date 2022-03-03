#' Represent a simple discrete offset 
#'
#' @param x value to use for offset
#' @param reference value defined as non-effect
#' @param label 1-length character vector used as prefix for column names
#' @return a representation of the covariate 
#' @export
discrete_offset = function(
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
  N = length(x)
  K = length(offset_values)
  o = matrix(data = 0, nrow = N, ncol = K,
    dimnames = list(
      rows = 1:N,
      parameters = paste(label, "level", offset_values, sep = "::")))
  for (k in seq_along(offset_values)) {
    o[x == offset_values[k],k] = 1
  }
  return(list(
    type = "discrete-offset",
    format = "matrix",
    N = nrow(o),
    K = ncol(o),
    matrix = o))
}
