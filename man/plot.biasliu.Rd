\name{plot.biasliu}
\alias{plot.biasliu}
\title{Bias Variance and MSE Trade-off Plot}
\description{
Trade-off between bias, variance and MSE of the Liu regression against vector or scalar value of biasing parameter \eqn{d}{d}.}

\usage{\method{plot}{biasliu}(x, abline = TRUE, \dots)}
\arguments{
  \item{x}{An object of "liu".}
  \item{abline}{Horizontal and vertical lines show the minimum value of the Liu MSE at certain value of biasing parameter \eqn{d}{d}.}
  \item{\dots}{No presently used in this implementation.}
}
\details{The effect of multicollinearity on the coefficients can be identified using different graphical display. One of them is plot of bias, variance and MSE. Addition of biasing parameter \eqn{d}{d} lead to a substantial impact on variance and MSE of Liu regression estimates. Therefore, a trade-off is made between bias and variance to have an acceptable MSE. The \code{plot.biasliu} can be helpful for selection of optimal value of biasing parameter \eqn{d}{d}.}
\value{Nothing returned}
\author{Muhammad Imdadullah, Dr. Muhammad Aslam}

\references{
Kalivas, J.H., and Palmer, J. (2014). Characterizing multivariate calibration tradeoff (bias, variance, selectivity, and sensitivity) to select model tuning parameters. \emph{Journal of Chemometrics}, \strong{28}(5), 347--357.
}
\seealso{Liu model fitting \code{\link{liu}}, Liu residuals \code{\link{residuals.liu}}, Liu PRESS \code{\link{press.liu}}, Testing of Liu Coefficients \code{\link{summary.liu}} }

\examples{
mod<-liu(y~.,as.data.frame(Hald), d=seq(-5,5,.1))

## for indication of vertical line (biasing parameter d) and
## horizontal line (minimum Liu MSE value corresponding to vertical line)

plot.biasliu(mod)

## without horizontal and vertical line
plot.biasliu(mod, abline = FALSE)
}
\keyword{ Liu Bias }
\keyword{ Liu Variance }
\keyword{ Liu MSE }
