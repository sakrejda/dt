test_that("max of b-spline radial basis function is 'h' parameter / 1.5", {
  max_height = dt:::b_spline_basic_radial_basis(0.6, 0.6, 0.9)
  expect_equal(0.6, max_height)
})

test_that("b-spline radial basis function is truncated beyond 2 * h", {
  k = rnorm(1)
  h = rexp(1)
  x = k + 2 * h + rexp(10, rate = 1/10)
  for (i in seq_along(x)) {
    expect_equal(0, dt:::b_spline_basic_radial_basis(x[i] , k, h))
  }
})

test_that("numerical and analytical integrals match.", {
  integration_error = function(z, width, k, h) {
    f = function(x) dt:::b_spline_basic_radial_basis(x, k, h)
    o = purrr::map_dbl(z, 
    ~ dt:::b_spline_integrated_basic_radial_basis(.x, .x + width, k, h) - 
      integrate(f, lower = .x, upper = .x + width)$value)
    return(o)
  }
  k_vec = rnorm(4, sd = 10)
  h_vec = rexp(4, rate = 0.1)
  for (k in k_vec) {
    for (h in h_vec) {
      pts = seq(from = k - 5 * h, to = k + 5 * h, length.out = 50)
      o = purrr::map_dbl(pts, ~ integration_error(.x, .01, k, h))
      expect_lt(max(abs(o)), 1e-6)
      expect_lt(mean(abs(o)), 1e-10)
    }
  }
})

#  diff_integral_error = function(x, width = 1e-3, k, h) (
#      b_integrated_basic_radial_basis(x, x + 1e-3 + width, k, h) - 
#      b_integrated_basic_radial_basis(x, x + 1e-3, k, h)
#    ) / 1e-3
