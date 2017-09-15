\name{plot.liu}
\alias{plot}
\alias{plot.liu}
\title{Plot of the Liu Coefficients}
\description{Plot of the Liu coefficient for scalar or vector values of biasing parameter \eqn{d}{d}.}
\usage{\method{plot}{liu}(x, abline = TRUE, \dots)}
\arguments{
  \item{x}{An object of class "liu".}
  \item{abline}{Vertical line to minimum value of MSE at certain value of biasing parameter \eqn{d}{d} used as argument in \code{liu} function.}
  \item{\dots}{Not presently used in this implementation.}
}
\details{
Graphical way of judging optimal value of biasing parameter \eqn{d}{d}. The  biasing parameter may be selected for acceptable (or minimum) value of MSE.}
\value{Nothing returned}

\references{
Liu, K. (1993). A new Class of Biased Estimate in Linear Regression. \emph{Journal of Statistical Planning and Inference}, \strong{141}, 189--196.
}
\author{Muhammad Imdadullah, Dr. Muhammad Aslam}

\seealso{Liu model fitting \code{\link{liu}}, Liu residuals \code{\link{residuals.liu}}, Liu PRESS \code{\link{press.liu}}, Testing of Liu Coefficients \code{\link{summary.liu}}, Plot of bias variance trade-off \code{\link{plot.biasliu}}}

\examples{
mod<-liu(y~.,as.data.frame(Hald), d=seq(-5,5,.4))
plot(mod)
plot(mod, abline = TRUE)
}
\keyword{ Liu coefficient plot }
