#' Calculate the matrix of covariates for the b-spline
#'
#' @param x values to calculate spline values for
#' @param knots knot locations
#' @param basis basis function to use for calculating the spline values
#' @param label 1-length character vector used to prefix the resulting
#'        set of column names
#' @param ... further arguments to the basis function
#' @return a matrix representing spline value per x value 
#' @export
b_spline_covariate = function(
  x, 
  knots, 
  basis = dt:::b_spline_basic_radial_basis,
  label = "spline",
  ...
) {
  N = length(x)
  K = length(knots)
  o = matrix(data = NA, nrow = N, ncol = K, 
    dimnames = list(
      rows = 1:N, 
      parameters = paste(label, "knot", 1:K, sep = "::")))
  for (k in 1:K) {
    o[,k] = basis(x, knots[k], ...)
  }
  return(list(
    type = "b-spline-covariate",
    format = "matrix",
    N = nrow(o),
    K = ncol(o),
    matrix = o))
}

