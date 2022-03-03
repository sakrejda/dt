#' Simple discrete intercept
#'
#' @param x value to use for covariate
#' @param label 1-length character vector used as prefix for column names
#' @return a matrix representing the covariate 
#' @export
discrete_intercept = function(
  x,
  label = "x"
) {
  N = length(x)
  unique_values = sort(unique(x))
  K = length(unique_values)
  o = matrix(data = 0, nrow = N, ncol = K,
    dimnames = list(
      rows = 1:N,
      parameters = paste(label, "level", unique_values, sep = "::")))
  for (k in seq_along(unique_values)) {
    o[x == unique_values[k],k] = 1
  }
  return(list(
    type = "discrete-intercept",
    format = "matrix",
    N = nrow(o),
    K = ncol(o),
    matrix = o))
}
