#' CLT Uniform
#'
#' The Central Limit Theorem (CLT) applied to a Uniform distribution. This function generates a histogram of sample means obtained by sampling from a Uniform distribution, along with the corresponding theoretical normal distribution curve and the probability density function of the Uniform distribution.
#'
#' @param n Sample size for each iteration.
#' @param iter Number of iterations.
#' @param a Lower limit of the uniform distribution (default is 0).
#' @param b Upper limit of the uniform distribution (default is 10).
#'
#' @return A histogram of sample means, a theoretical normal distribution curve, and the probability density function of the Uniform distribution.
#'
#' @examples
#' # Example 1: CLT with default parameters
#' mycltu(n = 10, iter = 10000)
#'
#' # Example 2: CLT with custom parameters
#' mycltu(n = 20, iter = 5000, a = 5, b = 15)
#'
#' @importFrom stats dnorm dunif runif density
#' @importFrom graphics lines
#' @export
mycltu=function(n,iter,a=0,b=10){
  ## r-random sample from the uniform
  y=runif(n*iter,a,b)
  ## Place these numbers into a matrix
  ## The columns will correspond to the iteration and the rows will equal the sample size n
  data=matrix(y,nrow=n,ncol=iter,byrow=TRUE)
  ## apply the function mean to the columns (2) of the matrix
  ## these are placed in a vector w
  w=apply(data,2,mean)
  ## We will make a histogram of the values in w
  ## How high should we make y axis?
  ## All the values used to make a histogram are placed in param (nothing is plotted yet)
  param=hist(w,plot=FALSE)
  ## Since the histogram will be a density plot we will find the max density

  ymax=max(param$density)
  ## To be on the safe side we will add 10% more to this
  ymax=1.1*ymax
  ## Now we can make the histogram
  hist(w,freq=FALSE,  ylim=c(0,ymax), main=paste("Histogram of sample mean",
                                                 "\n", "sample size= ",n,sep=""),xlab="Sample mean")
  ## add a density curve made from the sample distribution
  lines(density(w),col="Blue",lwd=3) # add a density plot
  ## Add a theoretical normal curve
  x <- seq(min(w), max(w), length.out = 1000)
  curve(dnorm(x,mean=(a+b)/2,sd=(b-a)/(sqrt(12*n))),add=TRUE,col="Red",lty=2,lwd=3) # add a theoretical curve
  ## Add the density from which the samples were taken
  curve(dunif(x,a,b),add=TRUE,lwd=4)

}
