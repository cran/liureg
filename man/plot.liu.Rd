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
Imdad, M. U. (2017). \emph{Addressing Linear Regression Models with Correlated Regressors: Some Package Development in R} (Doctoral Thesis, Department of Statistics, Bahauddin Zakariya University, Multan, Pakistan).

Imdadullah, M., Aslam, M., and Altaf, S. (2017). liureg: A comprehensive R Package for the Liu Estimation of Linear Regression Model with Collinear Regressors. \emph{The  R Journal}, \strong{9} (2), 232--247.

Liu, K. (1993). A new Class of Biased Estimate in Linear Regression. \emph{Journal of Statistical Planning and Inference}, \strong{141}, 189--196. \url{http://doi.org/10.1080/03610929308831027}.
}

\author{Muhammad Imdad Ullah, Muhammad Aslam}

\seealso{Liu model fitting \code{\link{liu}}, Liu residuals \code{\link{residuals.liu}}, Liu PRESS \code{\link{press.liu}}, Testing of Liu Coefficients \code{\link{summary.liu}}, Plot of bias variance trade-off \code{\link{plot.biasliu}}}

\examples{
mod<-liu(y~., data = as.data.frame(Hald), d = seq(-5, 5, 0.4))
plot(mod)
plot(mod, abline = TRUE)
}
\keyword{ Liu coefficient plot }
