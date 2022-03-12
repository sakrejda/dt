
#' Basic interval-spanning knot placement (not smart)
#'
#' If K is:
#'   Greater than 3: place knots at lower bound, 
#'     upper bound, outside knots one additional division 
#'     out. 
#'   Exactly 3: knots at lower bound, upper bound, and in 
#'     middle of interval
#'   Exactly 2: knots at lower and upper bound
#'   Exactly 1: knot at middle of interval
#'
#' @param lb lower bound of interval
#' @param ub upper bound of interval
#' @param K number of knots
#' @return locations of all knots
#'
#' @export
knot_placer_basic_interval = function(lb, ub, K = 4) {
  width = ub - lb
  if (K > 3) {
    h = width / (K - 3)
    knots = seq(from = lb - h, to = ub + h, length.out = K)
    attr(knots, 'h') = h
  } else if (K == 3) {
    knots = c(lb, (lb + ub)/2, ub)
  } else if (K == 2) {
    knots = c(lb, ub)
  } else if (K == 1) {
    knots = (lb + ub)/2
  }
  return(knots)
}
