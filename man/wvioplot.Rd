\name{wvioplot}
\alias{wvioplot}
\title{weighted violin plot}
\description{
  Produce violin plot(s) of the given values, taking weights into account. This
  package builds on Daniel Adler's excellent vioplot package.
}
\usage{

wvioplot(x, ..., range = 1.5, h = NULL, ylim = NULL, names = NULL, 
		horizontal = FALSE, col = "grey", border = "black", lty = 1, 
		lwd = 1, rectCol = "black", colMed = "white", pchMed = 19, 
		at, add = FALSE, wex = 1, drawRect = TRUE, centpt = "median", 
		weights = NULL, adjust = 3, clip = TRUE) 
}
\arguments{
  \item{x}{data vector}
  \item{...}{additional data vectors} 
  \item{range}{a factor to calculate the upper/lower adjacent values}
  \item{h}{the height for the density estimator, if omit as explained in sm.density, h will be set to an optimum}
  \item{ylim}{y limits}
  \item{names}{one label, or a vector of labels for the datas must match the number of datas given}
  \item{col, border, lty, lwd}{Graphical parameters for the violin passed to lines and polygon}
  \item{rectCol, colMed, pchMed}{Graphical parameters to control the look of the box}
  \item{drawRect}{logical. the box is drawn if \code{TRUE}.}
  \item{at}{position of each violin. Default to \code{1:n}}
  \item{add}{logical. if FALSE (default) a new plot is created}
  \item{wex}{relative expansion of the violin. }
  \item{horizontal}{logical. horizontal or vertical violins}
  \item{centpt}{one of "mean" or "median", indicating whether to use the median or mean for the center point}
  \item{weights}{either a vector of character weights or a list containing one vector of weights per data vector. Each weight vector must be the same length as each data vector}
  \item{adjust}{smoothing parameter. This will govern the smoothness of the sides of the body of each violin. This argument is passed to the kernel density function.}
  \item{clip}{logical. Should the extremes of the violin be clipped to the limits of the data?}
}
\details{
 A violin plot is a combination of a box plot and a kernel density plot. 
 Specifically, it starts with a box plot. It then adds a rotated kernel density plot to each side of the box plot.
 Each NA value in the data and each corresponding weight value are automatically removed.
 
 
}
\examples{
  # box- vs violin-plot 
  par(mfrow=c(2,1))
  mu<-2
  si<-0.6
  bimodal<-c(rnorm(1000,-mu,si),rnorm(1000,mu,si)) 
  uniform<-runif(2000,-4,4)
  normal<-rnorm(2000,0,3)
  wvioplot(bimodal,uniform,normal)
  boxplot(bimodal,uniform,normal)
  
  # weighted versus unweighted:
  # make up some weights:
  weights = rpois(2000, 1)
  par(mfrow=c(2,1))
  wvioplot(bimodal, uniform, normal)
  wvioplot(bimodal, uniform, normal, weights = weights )
  
  # use seperate set of weights for each variable:
  weightsbi = rpois(2000, 1)
  weightsun = rpois(2000, 1)
  weightsno = rpois(2000, 1)
  wvioplot(bimodal, uniform, normal, 
  			weights = list(weightsbi, weightsun, weightsno) )
  # be sure to pass the variables via list()
  
  # use different levels of smoothing:
  par(mfrow=c(3,1))
  wvioplot(bimodal, uniform, normal, adjust = 3) #default
  wvioplot(bimodal, uniform, normal, adjust = 2)
  wvioplot(bimodal, uniform, normal, adjust = 1)
  
  # do not clip at the values of the data:
  par(mfrow=c(2,1))
  wvioplot(bimodal, uniform, normal, clip=TRUE) #default
  wvioplot(bimodal, uniform, normal, clip=FALSE)
  
  # add to an existing plot
  x <- rnorm(100)
  y <- rnorm(100)
  plot(x, y, xlim=c(-5,5), ylim=c(-5,5))
  wvioplot(x, col="tomato", horizontal=TRUE, at=-4, add=TRUE,lty=2, rectCol="gray")
  wvioplot(y, col="cyan", horizontal=FALSE, at=-4, add=TRUE,lty=2)
  

  
}
\seealso{
    \code{\link[vioplot]{vioplot}}
    \code{\link{boxplot}}
  	\code{\link{density}}
}
\references{
  Hintze, J. L. and R. D. Nelson (1998).  \emph{Violin plots: a box plot-density trace
synergism.}  The American Statistician, 52(2):181-4.
}
\author{Solomon Messing \email{messing~at~stanford...}

}
\keyword{hplot, vioplot, wvioplot}

