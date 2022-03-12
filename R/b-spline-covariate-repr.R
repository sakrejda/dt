

#' Represent the matrix of covariates for the b-spline
#'
#' @param x values to calculate spline values for
#' @param knots knot locations
#' @param basis basis function to use for calculating the spline values
#' @param label 1-length character vector used to prefix the resulting
#'        set of column names
#' @param ... further arguments to the basis function
#' @return a representation of the spline value per x value 
#' @export
b_spline_covariate_repr = function(
  x, 
  knots, 
  basis = dt:::b_spline_basic_radial_basis,
  label = "spline",
  ...
) {
  dots = rlang::enquos(...)
  N = length(x)
  K = length(knots)
  return(list(
    label = label,
    type = "b-spline-covariate",
    format = "representation",
    N = length(x), K = length(knots),
    x = x, knots = knots, 
    row_names = as.character(1:length(x)),
    col_names = paste(label, "knot", 1:length(knots), sep = "::"),
    basis = basis,
    dots = dots))
}

