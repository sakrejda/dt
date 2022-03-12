#' Represent a simple discrete intercept
#'
#' @param x value to use for covariate
#' @param label 1-length character vector used as prefix for column names
#' @return a representation of the covariate 
#' @export
discrete_intercept_repr = function(
  x, 
  label = "x"
) {
  unique_values = sort(unique(x))
  nze_per_col = list()
  for (k in seq_along(unique_values)) {
    nze_per_col[[k]] = which(x == unique_values[k])
  }
  return(list(
    label = label,
    type = "discrete-intercept",
    format = "representation",
    nze_per_col = nze_per_col,
    N = length(x),
    K = length(unique_values),
    row_names = as.character(1:length(x)),
    col_names = paste(label, "level", unique_values, sep = "::")))
}
