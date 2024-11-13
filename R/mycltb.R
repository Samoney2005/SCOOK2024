#' CLT Binomial
#'
#' The Central Limit Theorem (CLT) applied to a Binomial distribution. This function generates a histogram of sample means obtained by sampling from a Binomial distribution, along with the corresponding theoretical normal distribution curve.
#'
#' @param n Sample size for each iteration.
#' @param iter Number of iterations.
#' @param p The probability of success for each trial in the Binomial distribution (default is 0.5).
#' @param ... Additional arguments to be passed to plotting functions.
#'
#' @return A histogram of sample means and a theoretical normal distribution curve.
#'
#' @examples
#' # Example 1: CLT with default parameters
#' mycltb(n = 10, iter = 10000)
#'
#' # Example 2: CLT with custom parameters
#' mycltb(n = 20, iter = 5000, p = 0.3, col = "blue")
#'
#' @importFrom graphics hist curve
#' @importFrom stats dnorm rbinom
#' @export
mycltb <- function(n, iter, p = 0.5, ...) {

  # Generate random samples from the Binomial distribution
  y <- rbinom(n * iter, size = n, prob = p)

  # Reshape the data into a matrix
  data <- matrix(y, nrow = n, ncol = iter, byrow = TRUE)

  # Calculate sample means
  w <- apply(data, 2, mean)

  # Make a histogram of sample means
  hist(w, freq = FALSE, ylim = c(0, max(dnorm(w, mean = n * p, sd = sqrt(p * (1 - p)))) * 1.1),
       main = paste("Histogram of Sample Mean", "\n", "Sample Size =", n), xlab = "Sample Mean", ...)

  # Define x
  x <- seq(min(w), max(w), length.out = 1000)

  # Add a theoretical normal curve
  curve(dnorm(x, mean = n * p, sd = sqrt(p * (1 - p))), add = TRUE, col = "red", lty = 2, lwd = 3, from = min(w), to = max(w))
}
