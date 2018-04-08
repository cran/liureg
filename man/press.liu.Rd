\name{press.liu}
\alias{press.liu}
\alias{press}

\title{Predicted Residual Sum of Squares}

\description{
The \code{press.liu} function computes predicted residual sum of squares (PRESS), computed from by following Ozkale and Kaciranlar (2007) <doi:10.1080/03610920601126522>.}

\usage{press(object, predr = FALSE, \dots)
\method{press}{liu}(object, predr = FALSE, \dots)
}

\arguments{
  \item{object}{An object of class "liu".}
  \item{predr}{If \code{TRUE} then predicted residuals are returned.}
  \item{\dots}{Not presently used in this implementation.}
}

\details{
For all of the \code{n} leave-one-out predicted residual sum of squares is calculated by fitting full regression model. PRESS is computed by using, \eqn{\sum (\hat{e}_{d(i)})^2}{\sum (\hat{e}_{d(i)})^2} or \eqn{\sum \left[\frac{\hat{e}_{di}}{1-h_{1-ii}}-\frac{e_i}{(1-h_{1-ii})(1-h_{ii})}(h_{1-ii}-\widetilde{H}_{d-ii})\right]^2}{\sum [(\hat{e}_{di})/(1-h_{1-ii})-(e_i)/((1-h_{1-ii})(1-h_{ii}))*(h_{1-ii}-\widetilde{H}_{d-ii})]^2} , where \eqn{h_{ii}=X(X'X)^{-1} X'}{H_{ii}=X(X'X)^{-1}X'}'s ith diagonal element, \eqn{h_{1-ii}=X(X'X+I)^{-1}X'}{h_{1-ii}=X(X'X+I)^{-1}X'}'s ith diagonal element and \eqn{\hat{e}_{di}} is the ith residual at specific value of \eqn{d}{d}.
}

\value{
The \code{press.liu} produces a vector of PRESS for scalar or vector values of biasing parameter \eqn{d}{d}. If argument \code{predr} is \code{TRUE} then predicted residuals are returned instead of predicted residual sum of squares.
}

\references{
Akdeniz, F. and Kaciranlar, S. (1995). On the Almost Unbiased Generalized Liu Estimators and Unbiased Estimation of the Bias and MSE. \emph{Communications in Statistics-Theory and Methods}, \strong{24}, 1789--1897. \url{http://doi.org/10.1080/03610929508831585}.

Allen, D. M. (1971). Mean Square Error of Prediction as a Criterion for Selecting Variables. \emph{Technometrics}, \strong{13}, 469-475. \url{http://www.jstor.org/stable/1267161}.

Allen, D. M. (1974). The Relationship between Variable Selection and Data Augmentation and Method for Prediction. \emph{Technometrics}, \strong{16}, 125-127. \url{http://www.jstor.org/stable/1267500}.

Imdad, M. U. (2017). \emph{Addressing Linear Regression Models with Correlated Regressors: Some Package Development in R} (Doctoral Thesis, Department of Statistics, Bahauddin Zakariya University, Multan, Pakistan).

Imdadullah, M., Aslam, M., and Altaf, S. (2017). liureg: A comprehensive R Package for the Liu Estimation of Linear Regression Model with Collinear Regressors. \emph{The  R Journal}, \strong{9} (2), 232--247.

Liu, K. (1993). A new Class of Biased Estimate in Linear Regression. \emph{Journal of Statistical Planning and Inference}, \strong{141}, 189--196. \url{http://doi.org/10.1080/03610929308831027}.

Ozkale, R. M. and Kaciranlar, S. (2007). A Prediction-Oriented Criterion for Choosing the Biasing Parameter in Liu Estimation. \emph{Commincations in Statistics-Theory and Methods}, \strong{36}(10): 1889--1903. \url{http://doi.org/10.1080/03610920601126522}.
}

\author{Muhammad Imdad Ullah, Muhammad Aslam}

\seealso{
The ridge model fitting \code{\link{liu}}, Liu residual \code{\link{residuals}}, Liu predicted value \code{\link{predict}}
}

\examples{
mod<-liu(y~., data = as.data.frame(Hald), d = seq(-5, 5, 0.1))
## PRESS
press(mod)

## Predicted residual
press(mod, predr = TRUE)
}

\keyword{ PRESS }
