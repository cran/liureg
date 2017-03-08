\name{lstats.liu}
\alias{lstats}
\alias{lstats.liu}
\alias{print.lstats}

\title{Liu Regression Statistics}
\description{
The \code{lstats} function computes the Liu regression related statistics such as variance, estimated squared bias, MSE, R-squared and adjusted R-squared etc.}

\usage{
lstats(object, \dots)
\method{lstats}{liu}(object, \dots)
\method{print}{lstats}(x, \dots)
}
\arguments{
  \item{object}{An object of class "liu".}
  \item{x}{An object of class "liu" for \code{print.lstats.liu}.}
  \item{\dots}{Not presently used in this implementation.}
}
\details{
The \code{lstats} function computes the Liu regression related statistics which may help in selecting appropriate optimal value of biasing parameter \eqn{d}{d}. If value of \eqn{d}{d} is one then these statistics are equivalent to the relevant OLS statistics.}
\value{
\item{lEDF}{Residual effective degrees of freedom for given biasing parameter \eqn{d}{d} from Hastie and Tibshirani (1990), i.e., \eqn{n-trace(2H_d)-H_d t(H_d)}{n-trace(2H_d)-H_d t(H_d)}.}
\item{lsigma2}{Computation of \eqn{\hat{\sigma}^2}{\hat{\sigma}^2} from Liu regression.}
\item{Cl}{Mallows \eqn{C_p}{C_p} like statistics for given biasing parameter \eqn{d}{d.} }
\item{var}{Variance of Liu regression for given biasing parameter \eqn{d}{d}.}
\item{bias2}{Estimated squared bias of Liu regression for given biasing parameter \eqn{d}{d}.}
\item{mse}{Total MSE value for given biasing parameter \eqn{d}{d}.}
\item{Fv}{F-statistics value for testing of the significance of the Liu regression estimator computed for given biasing parameter \eqn{d}{d}.}
\item{R2}{R-squared for given biasing parameter \eqn{d}{d}.}
\item{adjR2}{Adjusted R-squared for given biasing parameter \eqn{d}{d}.}
\item{minmse}{Minimum MSE value for a certain value of biasing parameter \eqn{d}{d}.}
\item{SSER}{Sum of squares of error from Liu regression for each biasing parameter \eqn{d}{d}.}
}

\references{
Akdeniz, F. and Kaciranlar, S. (1995). On the Almost Unbiased Generalized Liu Estimators and Unbiased Estimation of the Bias and MSE. \emph{Communications in Statistics-Theory and Methods}, \strong{24}, 1789--1897.

Cule, E. and De lorioa, M. (2012). A semi-Automated method to guide the choice of ridge parameter in ridge regression. \emph{arXiv:1205.0686v1[stat.AP].}

Hastie, T. and Tibshirani, R. (1990). \emph{Generalized Additive Models}. Chapman \& Hall.

Imdadullah, M.  \emph{Addressing Linear Regression Models with Correlated Regressors: Some Package Development in R}, Unpublished Ph.D thesis, Department of Statistics, Bahauddin Zakriya University, Multan, Pakistan.

Liu, K. (1993). A new Class of Biased Estimate in Linear Regression. \emph{Journal of Statistical Planning and Inference}, \strong{141}, 189--196.}

\author{Muhammad Imdadullah, Dr. Muhammad Aslam}

\seealso{Liu model fitting \code{\link{liu}}, Liu residuals \code{\link{residuals.liu}}, Liu PRESS \code{\link{press.liu}}, Testing of Liu Coefficients \code{\link{summary.liu}} }

\examples{
mod<-liu(y~X1+X2+X3+X4, data=as.data.frame(Hald), d=seq(-5, 5, .1), scaling="centered")

lstats(mod)
}
\keyword{ Liu Regression Statistics }