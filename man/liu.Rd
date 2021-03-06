\name{liu}
\alias{liu}
\alias{liu.default}
\alias{coef.liu}
\alias{print.liu}
\alias{fitted.liu}
\alias{liuest}
\title{Liu Regression and Estimator}
\description{
Fits a linear Liu regression model after scaling regressors and returns an object of class "liu" (by calling \code{liuest} function), designed to be used in plotting method, testing of Liu coefficients and for computation of different Liu related statistics. The Liu biasing parameter \eqn{d}{d} can be a scalar or a vector. This new biased estimator was first proposed by Liu (1993) <doi:10.1080/03610929308831027>.
}
\usage{liu(formula, data, d = 1, scaling=c("centered", "sc", "scaled"), \dots)
liuest(formula, data, d=1, scaling=c("centered", "sc", "scaled"), \dots)
\method{liu}{default}(formula, data, d = 1, scaling=c("centered", "sc", "scaled"), \dots)
\method{coef}{liu}(object, \dots)
\method{print}{liu}(x, digits = max(5,getOption("digits") - 5), \dots)
\method{fitted}{liu}(object, \dots)
}
\arguments{
\item{formula}{Standard R formula expression, that is, a symbolic representation of the model to be fitted and has form \code{response~predictors}. For further details, see \code{\link{formula}}. }
\item{data}{An optional data frame containing the variables in the model. If not found in data, the variables are taken from \code{environment(formula)}, typically the environment from which \code{liu} or \code{liuest} is called.}
\item{d}{Liu biasing parameter (may be a vector).}
\item{scaling}{The method to be used to scale the predictors. The scaling option \code{"sc"} scales the predictors to correlation form, such that the correlation matrix has unit diagonal elements. \code{"scaled"} option standardizes the predictors to have zero mean and unit variance. \code{"centered"} option centers the predictors.}
\item{object}{A liu object, typically generated by a call to \code{liu} for \code{fitted.liu}, \code{predict.liu}, \code{vcov.liu}, \code{residuals.liu}, \code{infocr.liu}, \code{coef.liu}, \code{summary.liu} and \code{press.liu} functions.}
\item{x}{An object of class \code{liu} (for the \code{hatl.liu}, \code{lstats.liu},  \code{dest.liu}, \code{summary.liu}, \code{print.liu}, \code{print.summary.liu}, \code{print.dest}, \code{print.lstats}, and \code{plot.liu}, \code{plot.biasliu} and \code{plot.infoliu}.}
\item{digits}{Minimum number of significant digits to be used.}
\item{\dots}{Additional arguments to be passed to or from other methods.}
}
\details{
\code{liu} or \code{liuest} function fits in Liu regression after scaling the regressors and centering the response. The \code{liu} is default a function that calls \code{liuest} for computation of Liu coefficients and returns an object of class "liu" designed to be used in plotting method, testing of Liu coefficients and for computation of different Liu related statistics. If intercept is present in the model, its coefficient is not penalized. However, intercept is estimated from the relation \eqn{y=\overline{y}-\beta \overline{X}}. \code{print.liu} tries to be smart about formatting of Liu coefficients.
}
\value{
\code{liu} function returns an object of class "liu" after calling list of named objects from \code{liuest} function:
\item{coef}{A named vector of fitted coefficients.}
\item{call}{The matched call.}
\item{Inter}{Was an intercept included?}
\item{scaling}{The scaling method used.}
\item{mf}{Actual data used.}
\item{y}{The centered response variable.}
\item{xs}{The scaled matrix of predictors.}
\item{xm}{The vector of means of the predictors.}
\item{terms}{The \code{\link{terms}} object used.}
\item{xscale}{Square root of sum of squared deviation from mean regarding the scaling option used in \code{liu} or \code{liuest} function as argument.}
\item{lfit}{The fitted value of Liu regression for given biasing parameter \eqn{d}{d}.}
\item{d}{The Liu regression biasing parameter \eqn{d}{d} which can be scalar or a vector.}
}

\note{The function at the current form cannot handle missing values. The user has to take prior action with missing values before using this function.}

\references{
Akdeniz, F. and Kaciranlar, S. (1995). On the Almost Unbiased Generalized Liu Estimators and Unbiased Estimation of the Bias and MSE. \emph{Communications in Statistics-Theory and Methods}, \strong{24}, 1789--1897. \url{http://doi.org/10.1080/03610929508831585}.

Imdad, M. U. (2017). \emph{Addressing Linear Regression Models with Correlated Regressors: Some Package Development in R} (Doctoral Thesis, Department of Statistics, Bahauddin Zakariya University, Multan, Pakistan).

Imdadullah, M., Aslam, M., and Altaf, S. (2017). liureg: A comprehensive R Package for the Liu Estimation of Linear Regression Model with Collinear Regressors. \emph{The  R Journal}, \strong{9} (2), 232--247.

Liu, K. (1993). A new Class of Biased Estimate in Linear Regression. \emph{Journal of Statistical Planning and Inference}, \strong{141}, 189--196. \url{http://doi.org/10.1080/03610929308831027}.
}

\author{Muhammad Imdad Ullah, Muhammad Aslam}

\seealso{Liu model fitting \code{\link{liu}}, Liu residuals \code{\link{residuals.liu}}, Liu PRESS \code{\link{press.liu}}, Testing of Liu Coefficients \code{\link{summary.liu}} }

\examples{
data(Hald)
mod<-liu(y~., data = as.data.frame(Hald), d = seq(0, 0.1, 0.01), scaling = "centered")
## Scaled Coefficients
mod$coef

## Re-Scaled Coefficients
coef(mod)

## Liu fitted values
fitted(mod)

## Liu predited values
predict(mod)

## Liu Residuals
residuals(mod)

## Liu trace
plot(mod)

## Liu Var-Cov matrix
vcov(mod)

## Liu biasing parameters by researchers
dest(mod)

## Liu related statistics
lstats(mod)

## list of objects from liuest function
liuest(y~., data = as.data.frame(Hald), d = seq(0, 0.1, 0.01), scaling = "centered")
}

\keyword{ Liu Regression }
\keyword{ Liu Estimator }
\keyword{ Bias Regression Methods}
