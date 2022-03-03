#' Partition the real numbers into intervals
#'
#' @param breaks vector of breakpoints to create partitions with
#' @return list of vectors, each vector contains the lower and upper
#'         bound in order
#' @export
partition_all_reals = function(breaks = vector(mode = 'numeric')) {
  N = length(breaks)
  if (N == 0) {
    return(list(c(lb = -Inf, ub = Inf)))
  } else if (N == 1) {
    return(list(c(lb = -Inf, ub = breaks), c(lb = breaks, ub = Inf)))
  } else {
    breaks = sort(breaks)
    lb = c(-Inf, breaks[1:N])
    ub = c(breaks[1:N], Inf)
    partitions = purrr::map2(lb, ub, ~ c(lb = .x, ub = .y))
    return(partitions)
  }
}

#' Partition a subset of the reals into intervals 
#'
#' @param breaks vector of breakpoints to create partitions with
#' @param lb lowest number to include
#' @param ub highest number to include
#' @return list of vectors, each vector contains the lower and upper
#'         bound in order
#' @export
partition_bounded_reals = function(breaks, lb, ub) {
  partitions = partition_all_reals(breaks) %>% 
    purrr::discard(~ .x[1] >= ub || .x[2] <= lb) %>%
    purrr::map_if(~ .x[1] < lb && .x[2] > lb, ~ c(lb, .x[2])) %>%
    purrr::map_if(~ .x[1] < ub && .x[2] > ub, ~ c(.x[1], ub)) %>%
    purrr::map(~ rlang::set_names(., c('lb', 'ub')))
  return(partitions)
}
