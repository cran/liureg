\name{vcov.liu}
\alias{vcov.liu}
\alias{vcov}

\title{Variance-Covariance Matrix for Fitted Liu Model}

\description{The \code{vcov} function computes the variance-covariance matrix for the estimates of linear Liu regression model.}

\usage{\method{vcov}{liu}(object, \dots)}

\arguments{
\item{object}{For \code{VCOV} method, an object of class "liu", i.e., a fitted model.}
\item{\dots}{Not presently used in this implementation.}
}

\details{
The \code{vcov} function computes variance-covariance matrix for scalar or vector value of biasing parameter \eqn{d}{d} provided as argument to \code{liu} function.}

\value{
A list of matrix of estimated covariances in the linear Liu regression model for scalar or vector biasing parameter \code{d}{d} is produced. Each list element has row and column names corresponding to the parameter names given by the \code{coef(mod)}. List items are named correspond to values of biasing parameter \eqn{d}{d}.}

\references{
Kaciranlar, S., Sakalhoglu, S., Akdeniz, F., Styan, G., and Werner, H. (1999). A new Biased Estimator in Linear Regression and a Detailed Analysis of the Widely Analysed Dataset on Portland Cement. \emph{Sankhya: The Indian Journal of Statistics, Series B}, \strong{61} (B3), 443--459.
}

\author{Muhammad Imdadullah, Dr. Muhammad Aslam}

\note{Covariance will be without intercept term, as intercept term is not penalized in the Liu regression.}

\seealso{The Liu model fitting \code{\link{liu}}, Liu summary statistics \code{\link{summary.liu}}, Liu prediction \code{\link{predict}}, Liu PRESS values \code{\link{press}}, Liu residual \code{\link{residuals}}}

\examples{
data(Hald)
mod<- liu(y~., data=as.data.frame(Hald), scaling="centered", d=seq(-2,2,.1) )

vcov(mod)
}

\keyword{ Liu Variance Covariance Matrix}
