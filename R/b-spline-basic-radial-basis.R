#' A basic radial b-spline basis
#'
#' @param x location to evaluate at
#' @param k location of basis knot
#' @param h height parameter
#' @param s knot width/scaling parameter
#' @return value at x for knot at k with parameter h
b_spline_basic_radial_basis = function(x, k, h = 1, s = 1) {
  if (length(x) > 1)
    return(purrr::map_dbl(x, ~ b_spline_basic_radial_basis(., k, h, s)))
  r = abs(x - k)/s
  if (r == 0) {
    o = h
  } else if (r <= h) {
    r2 = r * r
    o = h - (1.5 * r2) / h + (0.75 * r2 * r) / (h * h)
  } else if (r <= (2 * h)) {
    r2 = r * r
    o = (2 * h) - (3 * r) + (1.5 * r2) / h - (0.25 * r2 * r) / (h * h)
  } else {
    o = 0
  }
  o = o/1.5
  return(o)
}

