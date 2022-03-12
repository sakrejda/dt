#' Create an interaction term between two flat represented model components
#'
#' @param a first model component
#' @param b second model component
#' @return interaction resolved component
#'
#' @export
flat_interact = function(a, b) {
  if (a$format != "row") {
    rlang::abort("LHS object is not in 'row' format.")
  } else if (b$format != "row") {
    rlang::abort("RHS object is not in 'row' format.")
  }
  o = flat_permute(a,b)
  o$label = paste(a$label, b$label, sep = ':::')
  o$type = paste(a$type, b$type, sep = ':::')
  o$format = "row"
  return(o) 
}
