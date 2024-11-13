#' Number of Tickets
#'
#' @param N N number of seats available
#' @param gamma probability of overbooking
#' @param p probability that a passenger will show
#'
#' @return 2 plots (discrete and continuous) representing the number of tickets
#' and a list of variable values
#'
#' nd: The calculated number of tickets to sell using the discrete probability distribution.
#' nc: The calculated number of tickets to sell using the normal approximation.
#' N: The total number of seats available on the flight.
#' gamma: The maximum acceptable probability that the flight is overbooked.
#' p: The likelihood that a ticket holder will attend the flight.
#'
#'
#' The function also produces two plots showing the Objective function versus n. The Objective function
#' is derived by setting the defining equation to zero (e.g., 1 - gamma - pnorm(...) = 0). One plot represents
#' the discrete case, while the other illustrates the continuous case using the normal approximation.
#'
#' @export
#'
#' @importFrom stats dnorm
#' @importFrom stats pbinom
#' @importFrom stats pnorm
#' @importFrom graphics abline
#' @importFrom graphics barplot
#' @importFrom graphics curve
#' @importFrom graphics layout
#'
#' @examples ntickets(400,.02,.95)
ntickets = function(N, gamma, p){

  objectived = function(gamma, N, p, n){
    1 - gamma - pbinom(N, n, p)
  }
  minD = which.min(abs(objectived(gamma = gamma,
                                  N = N, p = p,
                                  n = (N + 1):(N*1.1))))
  nd = N + minD

  plot(x = N:(N*1.1), y = objectived(gamma = gamma,
                                     N = N, p = p,
                                     n = N:(N*1.1)),
       ylab = "Objective Function",
       xlab = "n",
       pch = 21,
       bg = "blue",
       main = paste("Objective vs n to find optimal tickets sold \n(" , nd , ") gamma= " , gamma , " N = " , N , " continous", sep = ""))

  abline(h = 0, col = "red")
  abline(v = nd, col = "red")
  objectivec = function(gamma, N, p, n){

    1 - gamma - pnorm(N + 0.5, mean = n*p,
                      sd = sqrt(n*p*(1-p)))
  }
  x = NULL
  opt = optimize(function(x) abs(objectivec(
    gamma = gamma, N = N, p = p, n = x)),
    lower = N, upper = N*1.1)
  nc = opt$minimum

  curve(objectivec(gamma = gamma, N = N, p = p,
                   n = x),
        ylab = "Objective Function", xlab = "n",
        xlim = c(N,N*1.1), lwd = 2,
        main = paste("Objective vs n to find optimal tickets sold \n(" , nc , ") gamma= " , gamma , " N = " , N , " continous", sep = ""))

  abline(v = nc, col = "red")
  abline(h = 0, col = "red")
  list("nd" = nd, "nc" = nc, "gamma" = gamma, "N" = N, "p" = p)
}
