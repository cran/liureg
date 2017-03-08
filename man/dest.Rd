\name{dest.liu}
\alias{dest}
\alias{dest.liu}
\alias{print.dliu}

\title{Computation of Liu Biasing Parameter \eqn{d}{d}}
\description{The \code{dest} function computes different Liu biasing parameters available in the literature, proposed by different researchers.}

\usage{dest(object, \dots)
\method{dest}{liu}(object, \dots)
\method{print}{dliu}(x, \dots)
}
\arguments{
  \item{object}{An object of class "liu".}
  \item{x}{An object of class "dliu" for the \code{print.dest.dliu}.}
  \item{\dots}{Not presently used in this implementation.}
}
\details{
The \code{dest} function computes different biasing parameter for the Liu regression. All these methods are already available in the literature proposed by various authors.
}
\value{
This function returns the list of following estimators of Liu parameter, available in the literature.
\item{dopt}{By Liu (1993): \eqn{\frac{\sum_{j=1}^p [\frac{\alpha_j^2-\sigma^2}{(\lambda_j+1)^2}]} {\sum_{j=1}^p[\frac{\sigma^2+\lambda_j\times\alpha_j^2}{\lambda_j(\lambda_j+1)^2}]}}{[\sum(\alpha_j^2-\sigma^2)/((\lambda_j+1)^2)]/[\sum(\sigma^2+\lambda_j\alpha_j^2)/(\lambda_j(\lambda_j+1)^2)]}}
\item{dILE}{By Liu, (2011): \eqn{\frac{\sum_{j=1}^n \frac{\widetilde{e}_i}{1-g_{ii}}(\frac{\widetilde{e}_i}{1-h_{1-ii}}-\frac{\hat{e}_i}{1-h_{ii}})}{\sum_{i=1}^n (\frac{\widetilde{e}_i}{1-g_{ii}}-\frac{\hat{e}_i}{1-h_{ii}})^2}}{[\sum((\widetilde{e}_i)/(1-g_{ii})[(\widetilde{e}_i)/(1-h_{1-ii})-(\hat{e}_i)/(1-h_{ii})])]/[\sum((\widetilde{e}_i)/(1-g_{ii})-(\hat{e}_i)/(1-h_{ii}))^2]}, where \eqn{\hat{e}=y_i-x'_i(X'X-x_i x'_i)^{-1}(X'y-x_iy_i)}{\hat{e}=y_i-x'_i(X'X-x_i x'_i)^{-1}(X'y-x_iy_i)}, \eqn{\widetilde{e}=y_i-x'_i(X'X+I_p-x_i x'_i)^{-1}(X'y-x_iy_i)}{\widetilde{e}=y_i-x'_i(X'X+I_p-x_i x'_i)^{-1}(X'y-x_iy_i)}, \eqn{G=X(X'X+I_p)^{-1}X'}{G=X(X'X+I_p)^{-1}X'} and \eqn{H\cong X(X'X)^{-1}X'}{H=X(X'X)^{-1}X'}.}
\item{dmm}{Liu, (1993): \eqn{1-\hat{\sigma}^2 \frac{\sum_{j=1}^p\frac{1}{\lambda_j(\lambda_j+1)}}{\sum_{j=1}^p\frac{\hat{\alpha}_j^2}{(\lambda_j+1)^2}}}{1-\hat{\sigma}^2[\sum(1/(\lambda_j(\lambda_j+1)))/(\sum(\hat{\alpha}_j^2/(\lambda_j+1)^2))]}}
\item{dcl}{By Liu, (1993): \eqn{1-\hat{\sigma}^2[\frac{\sum_{j=1}^p \frac{1}{(\lambda_j+1)} } {\sum_{j=1}^p \frac{\lambda_j\hat{\alpha}_j^2}{(\lambda_j+1)^2}}]}{1-\hat{\sigma}^2[(\sum((1)/(\lambda_j+1)))/(\sum((\lambda_j\hat{\alpha}_j^2)/(\lambda_j+1)^2))]}.}
\item{GCV}{GCV criterion for selection of optimal \eqn{d}{d}, that is, \eqn{GCV=\frac{SSR_d}{(n-1-trace(H_d))}}{GCV=(SSR_d)/(n-1-trace(H_d))}, where \eqn{SSR_d}{SSR_d} is residuals sum of squares from Liu regression at certain value of \eqn{d}{d} and \eqn{trace(H_d)}{trace(H_d)} is trace of hat matrix from Liu.}
\item{dGCV}{returns value of \eqn{d}{d} at which GCV is minimum.}
}

\references{
Akdeniz, F. and Kaciranlar, S. (1995). On the Almost Unbiased Generalized Liu Estimators and Unbiased Estimation of the Bias and MSE. \emph{Communications in Statistics-Theory and Methods}, \strong{24}, 1789--1897.

Liu, K. (1993). A new Class of Biased Estimate in Linear Regression. \emph{Journal of Statistical Planning and Inference}, \strong{141}, 189--196.
}
\author{Muhammad Imdadullah, Dr. Muhammad Aslam}

\seealso{Liu model fitting \code{\link{liu}}, Liu residuals \code{\link{residuals.liu}}, Liu PRESS \code{\link{press.liu}}, Testing of Liu coefficients \code{\link{summary.liu}} }

\examples{
mod<-liu(y~.,as.data.frame(Hald), d=seq(-5,5,.1))
dest(mod)
## Vector of GCV values for each d
dest(mod)$GCV
}
\keyword{ Liu biasing parameter }
