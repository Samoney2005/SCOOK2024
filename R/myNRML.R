#' Newton Raphson Method
#'
#' Estimates the likelihood of a parameter.
#'
#' @param x0 Initial estimate.
#' @param delta Timestep for the derivative.
#' @param llik Log-likelihood function.
#' @param xrange X range of the graph produced by this function.
#' @param parameter Main title of the graph.
#'
#' @return A list containing the estimated parameters and their derivatives.
#' @export
#'
#' @examples
#' llik_example <- function(x) {
#'   log(dpois(4, x) * dpois(6, x) * dpois(7, x) * dpois(6, x) * dpois(5, x))
#' }
#' myNRML(x0 = 2, delta = 0.000001, llik = llik_example, xrange = c(0, 20), parameter = "lambda")

myNRML = function(x0, delta = 0.001, llik, xrange, parameter = "param") {
  f = function(x) (llik(x + delta) - llik(x)) / delta
  fdash = function(x) (f(x + delta) - f(x)) / delta
  d = 1000
  i = 0
  x = numeric(100)  # Pre-allocate for efficiency
  y = numeric(100)
  x[1] = x0
  y[1] = f(x[1])

  while (d > delta & i < 99) {  # Adjusted for pre-allocation
    i = i + 1
    x[i + 1] = x[i] - f(x[i]) / fdash(x[i])
    y[i + 1] = f(x[i + 1])
    d = abs(x[i + 1] - x[i])  # Updated convergence condition
  }

  layout(matrix(1:2, nrow = 1, ncol = 2, byrow = TRUE), widths = c(1, 2))
  curve(llik(x), xlim = xrange, xlab = parameter, ylab = "log Lik", main = "Log Lik")
  curve(f(x), xlim = xrange, xaxt = "n", xlab = parameter, ylab = "derivative",
        main = "Newton-Raphson Algorithm \n on the Derivative")
  points(x[1:i], y[1:i], col = "Red", pch = 19, cex = 1.5)
  axis(1, x[1:i], round(x[1:i], 2), las = 2)
  abline(h = 0, col = "Red")

  segments(x[1:(i - 1)], y[1:(i - 1)], x[2:i], rep(0, i - 1), col = "Blue", lwd = 2)
  segments(x[2:i], rep(0, i - 1), x[2:i], y[2:i], lwd = 0.5, col = "Green")

  list(x = x[1:i], y = y[1:i])
}

# Example call
llik_example <- function(x) {
  log(dpois(4, x) * dpois(6, x) * dpois(7, x) * dpois(6, x) * dpois(5, x))
}
myNRML(x0 = 2, delta = 0.000001, llik = llik_example, xrange = c(0, 20), parameter = "lambda")
