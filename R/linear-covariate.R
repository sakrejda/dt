#' Simple continuous covariate
#'
#' @param x value to use for covariate
#' @param label 1-length character vector used as prefix for column name
#' @return a matrix representing the covariate 
#' @export
linear_covariate = function(
  x, 
  label = "x"
) {
  N = length(x) 
  K = 1
  o = matrix(data = x, nrow = N, ncol = K,
    dimnames = list(
      rows = 1:N,
      parameters = paste(label)))
  return(list(
    type = "linear-covariate",
    format = "matrix",
    N = nrow(o),
    K = ncol(o),
    matrix = o))
}
