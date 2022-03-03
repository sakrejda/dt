#' Integral of a basic radial b-spline basis
#'
#' @param lb lower bound of integration
#' @param ub upper bound of integration
#' @param k location of basis knot
#' @param h height parameter
#' @return integral between lb and ub for knot at k with parameter h
#'
#' @export
b_spline_integrated_basic_radial_basis = function(lb, ub, k, h = 1) {
  pts = c(k - 2 * h, k - h, k, k + h, k + 2 * h)
  sub_parts = partition_bounded_reals(pts, lb, ub)
  total = 0
  for (i in seq_along(sub_parts)) {
    part = sub_parts[[i]]
    if (part['lb'] >= pts[3] && !(part['ub'] > pts[4])) {
      a = part['lb'] - k
      b = part['ub'] - k
      total = total + h * (b - a) - 0.5 * (b^3 - a^3) / h + 
        (3 * (b^4 - a^4)) / (16 * h * h)
    } else if (!(part['lb'] < pts[2]) && part['ub'] <= pts[3]) {
      a = k - part['ub']
      b = k - part['lb']
      total = total + h * (b - a) - 0.5 * (b^3 - a^3) / h + 
        (3 * (b^4 - a^4)) / (16 * h * h)
    } else if (part['lb'] >= pts[4] && !(part['ub'] > pts[5])) {
      a = part['lb'] - k
      b = part['ub'] - k
      total = total + 2 * h * (b - a) - 1.5 * (b^2 - a^2) + 
        0.5 * (b^3 - a^3) / h - (b^4 - a^4) / (16 * h^2)
    } else if (!(part['lb'] < pts[1]) && part['ub'] <= pts[2]) {
      a = k - part['ub']
      b = k - part['lb']
      total = total + 2 * h * (b - a) - 1.5 * (b^2 - a^2) + 
        0.5 * (b^3 - a^3) / h - (b^4 - a^4) / (16 * h^2)
    } 
  }
  return(total/1.5)
}

