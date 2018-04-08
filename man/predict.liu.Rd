\name{predict.liu}
\alias{predict}
\alias{predict.liu}
\title{Predict Method for Liu Regression Model Fits}
\description{Predicted values based on the Liu regression model for scalar or vector values of biasing parameter \eqn{d}{d}.}

\usage{\method{predict}{liu}(object, newdata, na.action = na.pass, terms = NULL, \dots)}
\arguments{
  \item{object}{An object of class "liu".}
  \item{newdata}{An optional data frame in which to look for variables with which to predict.}
  \item{na.action}{Function determine what should be done with missing values in \code{newdata}. The default is to predict \code{NA}.}
  \item{terms}{The \code{\link{terms}} object used.}
  \item{\dots}{Not presently used in this implementation.}
}
\details{
The \code{predict.liu} function predicted values, obtained by evaluating the regression function in the frame \code{newdata} which defaults to model.frame (\code{object}). If \code{newdata} is omitted the predictions are based on the data used for the fit. In that case how cases with missing values in the original fit are handled is determined by the \code{na.action} argument of that fit. If \code{na.action=na.omit} omitted cases will not appear in the predictions, whereas if \code{na.action=na.exclude} they will appear (in predictions), with value NA.}
\value{\code{predict.liu} produces a vector of prediction or a matrix of predictions for scalar or vector values of biasing parameter \eqn{d}{d}.}

\author{Muhammad Imdad Ullah, Muhammad Aslam}

\note{
Variables are first looked for in \code{newdata} and then are searched for in the usual way (which will include the environment of the formula used in the fit). A warning will be given if the variables found are not of the same length as those in the \code{newdata} if it was supplied.}

\seealso{Liu model fitting \code{\link{liu}}, Liu residuals \code{\link{residuals.liu}}, Liu PRESS \code{\link{press.liu}}, Testing of Liu Coefficients \code{\link{summary.liu}} }

\examples{
mod <- liu(y~., data = as.data.frame(Hald), d = seq(-5, 5, 0.1))
predict(mod)

predict(mod, newdata=as.data.frame(Hald[1:5, -1]))
}

\keyword{ Liu Model Prediction }
