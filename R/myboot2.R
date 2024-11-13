#' Bootstrapping Function
#'
#' This function uses bootstrapping to approximate properties of a population
#' @param x the sample to be used
#' @param iter the number of iterations to use for calculation
#' @param fun the function to be applied onto our sample
#' @param alpha used to determine the (1-alpha)*100 percent confidence interval
#' @param cx the relative scaling of the text/symbols for plots
#' @param ... more parameters for plotting histogram
#'
#' @return a list, containing the data for the function, the confidence
#' interval and the provided sample
#'
#' @export

myboot2<-function(iter=10000,x,fun="mean",alpha=0.05,cx=1.5,...){

  # size of sample
  n=length(x)

  # generates sample of size (n*iter) from items of "x"
  # then converts it to a (n x iter) matrix
  # each column represents one iteration of the simulation
  y=sample(x,n*iter,replace=TRUE)
  rs.mat=matrix(y,nrow=n,ncol=iter,byrow=TRUE)

  # Applies a function ("fun") to each column (thus, each simulation)
  # After applying the function, new array is (1 x iter)
  xstat=apply(rs.mat,2,fun)

  # calculates quantile of x for [alpha/2] and [1 - (alpha/2)]
  ci=quantile(xstat,c(alpha/2,1-alpha/2))

  # Graphs a histogram of our post-function data
  # "para" contains the histogram data
  para=hist(xstat,freq=FALSE,las=1,
            main=paste("Histogram of Bootstrap sample statistics","\n","alpha=",alpha," iter=",iter,sep=""),
            ...)

  # Transposes "x" from 1-row matrix to 1-column matrix
  # Used for the "apply" function
  mat=matrix(x,nrow=length(x),ncol=1,byrow=TRUE)

  # pte is the point estimate
  # applies function ("fun") to x for pte
  pte=apply(mat,2,fun)

  # draws vertical line on histogram to show pte
  abline(v=pte,lwd=3,col="Black")

  # Make the segment for the ci
  segments(ci[1],0,ci[2],0,lwd=4)

  # Graphs lower and upper bound on ci onto histogram
  text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Red",cex=cx)
  text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Red",cex=cx)

  # plot the point estimate 1/2 way up the density
  text(pte,max(para$density)/2,round(pte,2),cex=cx)

  # outputs our data
  return(list(ci=ci,fun=fun,x=x))
}
