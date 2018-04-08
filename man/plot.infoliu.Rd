\name{plot.infoliu}
\alias{plot.infoliu}
\title{Model Selection Criteria Plots}
\description{
Plot of the Liu AIC and BIC model selection criteria against Liu degrees of freedom.}
\usage{\method{plot}{infoliu}(x, abline = TRUE, \dots)}

\arguments{
  \item{x}{An object of class "liu".}
  \item{abline}{Vertical line to show minimum value of Liu MSE at certain of Liu degrees of freedom.}
  \item{\dots}{Not presently used in this implementation.}
}

\details{
Plot of the Liu AIC and BIC against the Liu degree of freedom (sum of diagonal elements of the Liu Hat matrix). A vertical line represents the minimum Liu MSE at certain value of the Liu degree of freedom.
}
\value{Nothing returned}
\references{
Akaike, H. (1974). A new look at the Statistical Model Identification. \emph{IEEE Transaction on Automatic Control}, \strong{9}(6), 716--723. \url{https://doi.org/10.1109/TAC.1974.1100705}.

Imdad, M. U. (2017). \emph{Addressing Linear Regression Models with Correlated Regressors: Some Package Development in R} (Doctoral Thesis, Department of Statistics, Bahauddin Zakariya University, Multan, Pakistan).

Imdadullah, M., Aslam, M., and Altaf, S. (2017). liureg: A comprehensive R Package for the Liu Estimation of Linear Regression Model with Collinear Regressors. \emph{The  R Journal}, \strong{9} (2), 232--247.

Schwarz, G. (1978). Estimating the Dimension of a Model. \emph{Annals of Statistics}, \strong{6}(2), 461--464. \url{https://projecteuclid.org/euclid.aos/1176344136}.
}
\author{Muhammad Imdad Ullah, Muhammad Aslam}

\seealso{Liu model fitting \code{\link{liu}}, Liu residuals \code{\link{residuals.liu}}, Liu PRESS \code{\link{press.liu}}, Testing of Liu Coefficients \code{\link{summary.liu}}, bias variance trade-off \code{\link{plot.biasliu}} }

\examples{
mod<- liu(y~., as.data.frame(Hald), d = seq(-5, 5, 0.1))
## for indication of minimum MSE at Liu df (as vertical line)
plot.infoliu(mod)

## without vertical line
plot.infoliu(mod, abline = FALSE)
}

\keyword{ Model Selection Criteria }
\keyword{ AIC }
\keyword{ BIC }
