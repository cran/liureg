\name{lstats.liu}
\alias{lstats}
\alias{lstats.liu}
\alias{print.lstats}

\title{Liu Regression Statistics}
\description{
The \code{lstats} function computes the Liu regression related statistics such as variance, estimated squared bias, MSE, R-squared and adjusted R-squared etc. These statistics are computed by following Liu (1993) <doi:10.1080/03610929308831027>; Akdeniz, F. and Kaciranlar, S. (1995) <doi:10.1080/03610929508831585>; Cule, E. and De Iorioa, M. (2012); Hastie, T. and Tibshirani, R. (1990); and Mallows (1973) <doi:10.2307/1267380>.}

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
Akdeniz, F. and Kaciranlar, S. (1995). On the Almost Unbiased Generalized Liu Estimators and Unbiased Estimation of the Bias and MSE. \emph{Communications in Statistics-Theory and Methods}, \strong{24}, 1789--1897. \url{http://doi.org/10.1080/03610929508831585}.

Cule, E. and De lorioa, M. (2012). A semi-Automated method to guide the choice of ridge parameter in ridge regression. \emph{arXiv:1205.0686v1[stat.AP]}. \url{https://arxiv.org/abs/1205.0686v1}.

Hastie, T. and Tibshirani, R. (1990). \emph{Generalized Additive Models}. Chapman \& Hall.

Imdad, M. U. (2017). \emph{Addressing Linear Regression Models with Correlated Regressors: Some Package Development in R} (Doctoral Thesis, Department of Statistics, Bahauddin Zakariya University, Multan, Pakistan).

Imdadullah, M., Aslam, M., and Altaf, S. (2017). liureg: A comprehensive R Package for the Liu Estimation of Linear Regression Model with Collinear Regressors. \emph{The  R Journal}, \strong{9} (2), 232--247.

Liu, K. (1993). A new Class of Biased Estimate in Linear Regression. \emph{Journal of Statistical Planning and Inference}, \strong{141}, 189--196. \url{http://doi.org/10.1080/03610929308831027}.

Mallows, C. L. (1973). Some Comments on Cp. \emph{Technometrics}, 15: 661--675. \url{http://doi.org/10.2307/1267380}.
}

\author{Muhammad Imdad Ullah, Muhammad Aslam}

\seealso{Liu model fitting \code{\link{liu}}, Liu residuals \code{\link{residuals.liu}}, Liu PRESS \code{\link{press.liu}}, Testing of Liu Coefficients \code{\link{summary.liu}} }

\examples{
mod<-liu(y~., data = as.data.frame(Hald), d = seq(-5, 5, 0.1), scaling = "centered")

lstats(mod)
}
\keyword{ Liu Regression Statistics }
