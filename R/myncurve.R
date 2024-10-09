#' Area of Curve
#'
#' @param mu The mean of the function
#' @param sigma The standard deviation of the function
#' @param a The max x-coordinate of the area
#'
#' @return Area to terminal and graph of area under the curve
#' @export
#'
#' @import stats
#' @import graphics
#'
#' @examples
#' myncurve(mu=10, sigma=5, a=6)
myncurve <- function(mu, sigma,a){
  x<- NULL
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu +
                                              3*sigma))

  xcurve = seq(mu-3*sigma,a, length = 10000)
  ycurve = dnorm(xcurve, mu, sigma)

  polygon(x = c(mu-3*sigma,xcurve, a), y = c(0,ycurve, 0), col = "red")

  prob = pnorm(a, mu, sigma)
  prob = round(prob,4)

  print(prob)
}
