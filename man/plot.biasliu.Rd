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

\author{Muhammad Imdad Ullah, Muhammad Aslam}

\references{
Imdad, M. U. (2017). \emph{Addressing Linear Regression Models with Correlated Regressors: Some Package Development in R} (Doctoral Thesis, Department of Statistics, Bahauddin Zakariya University, Multan, Pakistan).

Imdadullah, M., Aslam, M., and Altaf, S. (2017). liureg: A comprehensive R Package for the Liu Estimation of Linear Regression Model with Collinear Regressors. \emph{The  R Journal}, \strong{9} (2), 232--247.

Kalivas, J. H., and Palmer, J. (2014). Characterizing multivariate calibration tradeoff (bias, variance, selectivity, and sensitivity) to select model tuning parameters. \emph{Journal of Chemometrics}, \strong{28}(5), 347--357. \url{https://doi.org/10.1002/cem.2555}.
}

\seealso{Liu model fitting \code{\link{liu}}, Liu residuals \code{\link{residuals.liu}}, Liu PRESS \code{\link{press.liu}}, Testing of Liu Coefficients \code{\link{summary.liu}} }

\examples{
mod<-liu(y~., as.data.frame(Hald), d = seq(-5, 5, 0.1))

## for indication of vertical line (biasing parameter d) and
## horizontal line (minimum Liu MSE value corresponding to vertical line)

plot.biasliu(mod)

## without horizontal and vertical line
plot.biasliu(mod, abline = FALSE)
}
\keyword{ Liu Bias }
\keyword{ Liu Variance }
\keyword{ Liu MSE }
