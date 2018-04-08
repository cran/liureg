\name{residuals.liu}
\alias{residuals.liu}
\alias{residuals}

\title{Liu Regression Residuals}

\description{The \code{residuals} function computes the Liu residuals for scalar or vector value of biasing parameter \eqn{d}{d}.}

\usage{\method{residuals}{liu}(object, \dots)}

\arguments{
  \item{object}{An object of class "liu".}
  \item{\dots}{Not presently used in this implementation.}
}

\details{
The generic functions \code{residuals} can be used to compute residuals of object of class "liu" from function \code{liu}.
}

\value{
Returns a vector or a matrix of the Liu residuals for scalar or vector value biasing parameter \eqn{d}{d} provided as argument to \code{liu} function.
}

\references{
Akdeniz, F. and Kaciranlar, S. (1995). On the Almost Unbiased Generalized Liu Estimators and Unbiased Estimation of the Bias and MSE. \emph{Communications in Statistics-Theory and Methods}, \strong{24}, 1789--1897. \url{http://doi.org/10.1080/03610929508831585}.

Liu, K. (1993). A new Class of Biased Estimate in Linear Regression. \emph{Journal of Statistical Planning and Inference}, \strong{141}, 189--196. \url{http://doi.org/10.1080/03610929308831027}.
}

\author{Muhammad Imdad Ullah, Muhammad Aslam}

\seealso{
The Liu model fitting \code{\link{liu}}, Liu prediction \code{\link{predict}}, Liu PRESS values \code{\link{press}}}

\examples{
mod<-liu(y~., data = as.data.frame(Hald), d = seq(-2, 2, 0.1))

residuals(mod)
}

\keyword{ Liu Residuals }
