\name{hatl.liu}
\alias{hatl}
\alias{hatl.liu}

\title{Liu Regression: Hat Matrix}
\description{The \code{hatl} function computes hat matrix of Liu regression (Liu, 1993).}

\usage{hatl(object, \dots)
\method{hatl}{liu}(object, \dots)
}

\arguments{
  \item{object}{An object of class "liu".}
  \item{\dots}{Not presently used in this implementation.}
}

\details{
Hat matrix for scalar or vector values of biasing parameter \eqn{d}{d} provided as argument to \code{liu} function. It is used to compute degrees of freedom for given \eqn{d}{d}, and error degree of freedom etc. The hat matrix can be computed using formula \eqn{X(X'X+I_p)^{-1}(X'X+dI_p)(X'X)^{-1}X'}{X(X'X+I_p)^{-1}(X'X+dI_p)(X'X)^{-1}X'}.}

\value{
Returns a list of matrix for each biasing parameter \eqn{d}{d}:
\item{hatl}{A list of hat matrix for each biasing parameter \eqn{d}{d}.}
}

\note{The hat matrix is not idempotent because it is not projection matrix, therefore it is called quasi-projection matrix.}


\references{Liu, K. (1993). A new Class of Biased Estimate in Linear Regression. \emph{Journal of Statistical Planning and Inference}, \strong{141}, 189--196.}
\author{Muhammad Imdadullah, Dr. Muhammad Aslam}

\seealso{Liu model fitting \code{\link{liu}}, Liu residuals \code{\link{residuals.liu}}, Liu PRESS \code{\link{press.liu}}, Testing of Liu Coefficients \code{\link{summary.liu}} }

\examples{
mod<-liu(y~., as.data.frame(Hald), d=c(-5, -1, 0.2, 0.3))
## Hat matrix for each biasing parameter
hatl(mod)

## Hat matrix for second biasing parameter i.e. d=-1
hatl(mod)[[2]]

## Diagonal element of hat matrix for second biasing parameter
diag(hatl(mod)[[2]])
}

\keyword{ Liu regression }
\keyword{ Liu hat matrix }
