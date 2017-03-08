\name{plot.infoliu}
\alias{plot.infoliu}
\title{Model Selection Criteria Plots}
\description{
Plot of the Liu AIC and BIC model selection criteria against Liu degrees of freedom.}
\usage{\method{plot}{infoliu}(x, abline = TRUE, \dots)}
\arguments{
  \item{x}{An object of class "liu".}
  \item{abline}{Vertical line to show minimum value of Liu MSE at certain of Liu degrees of freedom.}
  \item{\dots}{Not presently used in this implementation.}
}
\details{
Plot of the Liu AIC and BIC against the Liu degree of freedom (sum of diagonal elements of the Liu Hat matrix). A vertical line represents the minimum Liu MSE at certain value of the Liu degree of freedom.
}
\value{Nothing returned}
\references{
Akaike, H. (1974). A new look at the Statistical Model Identification. \emph{IEEE Transaction on Automatic Control}, \strong{9}(6), 716--723.

Schwarz, G. (1978). Estimating the Dimension of a Model. \emph{Annals of Statistics}, \strong{6}(2), 461--464.
}
\author{Muhammad Imdadullah, Dr. Muhammad Aslam}

\seealso{Liu model fitting \code{\link{liu}}, Liu residuals \code{\link{residuals.liu}}, Liu PRESS \code{\link{press.liu}}, Testing of Liu Coefficients \code{\link{summary.liu}}, bias variance trade-off \code{\link{plot.biasliu}} }

\examples{
mod<- liu(y~., as.data.frame(Hald), d=seq(-5,5,.1))
## for indication of minimum MSE at Liu df (as vertical line)
plot.infoliu(mod)

## without vertical line
plot.infoliu(mod, abline = FALSE)
}

\keyword{ Model Selection Criteria }
\keyword{ AIC }
\keyword{ BIC }
