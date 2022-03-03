
#' Simple continuous covariate representation
#'
#' @param x value to use for covariate
#' @param label 1-length character vector used as prefix for column name
#' @return a matrix representing the covariate 
#' @export
linear_covariate_repr = function(
  x, 
  label = "x"
) {
  N = length(x) 
  return(list(
    type = "linear-covariate",
    format = "representation",
    x = x, N = length(x), K = 1,
    row_names = as.character(1:length(x)),
    col_names = label))
}
