\name{infoliu.liu}
\alias{infoliu}
\alias{infoliu.liu}
\title{Model Selection Criteria for Liu Regression}
\description{The \code{infoliu.liu} computes model selection criteria (AIC and BIC).}
\usage{infoliu(object, \dots)
\method{infoliu}{liu}(object, \dots)
}
\arguments{
  \item{object}{An object of class "liu".}
  \item{\dots}{Not presently used in this implementation.}
}
\details{
Model information selection criteria are common way of selecting among model while balancing the competing goals of fit and parsimony. The model selection criteria AIC and BIC are computed by quantifying degree of freedom \code{df} in the Liu regression model, using formula \eqn{trace(H_d)}{trace(H_d)}, where \eqn{H_d}{H_d} is hat matrix from Liu regression. Model selection criteria may be helpful for selecting optimal value of biasing parameter \eqn{d}{d}.
}

\value{
It returns a matrix of information criteria, AIC and BIC for each biasing parameter \eqn{d}{d}. Column of matrix indicates model selection criteria AIC and BIC, respectively, while rows indicate value of biasing parameter \eqn{d}{d} for which model selection criteria are computed.
}
\references{
Akaike, H. (1974). A new look at the Statistical Model Identification. \emph{IEEE Transaction on Automatic Control}, \strong{9}(6), 716--723.

Akdeniz, F. and Kaciranlar, S. (1995). On the Almost Unbiased Generalized Liu Estimators and Unbiased Estimation of the Bias and MSE. \emph{Communications in Statistics-Theory and Methods}, \strong{24}, 1789--1897.

Liu, K. (1993). A new Class of Biased Estimate in Linear Regression. \emph{Journal of Statistical Planning and Inference}, \strong{141}, 189--196.

Schwarz, G. (1978). Estimating the Dimension of a Model. \emph{Annals of Statistics}, \strong{6}(2), 461--464.
}
\author{Muhammad Imdadullah, Dr. Muhammad Aslam}

\seealso{Testing of Liu coefficient \code{\link{summary.liu}} }

\examples{
mod<-liu(y~X1+X2+X3+X4, data=as.data.frame(Hald), d=seq(-5, 5, 0.1), scaling="centered")

infoliu(mod)

## AIC values
infoliu(mod)[,1]

## BIC values
infoliu(mod)[,2]
}
\keyword{ AIC }
\keyword{ BIC }
