\name{dest.liu}
\alias{dest}
\alias{dest.liu}
\alias{print.dliu}

\title{Computation of Liu Biasing Parameter \eqn{d}{d}}
\description{The \code{dest} function computes different Liu biasing parameters available in the literature, proposed by different researchers such as given in Liu (1993) <doi:10.1080/03610929308831027>, Liu (2011) <doi:10.1016/j.jspi.2010.05.030>, and Ozkale and Kaciranlar (2007) <doi:10.1080/03610920601126522>.}

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
\item{dopt}{By Liu (1993): \eqn{\frac{\sum_{j=1}^p \left[\frac{\alpha_j^2-\sigma^2}{(\lambda_j+1)^2}\right]}{\sum_{j=1}^p\left[\frac{\sigma^2+\lambda_j\times\alpha_j^2}{\lambda_j(\lambda_j+1)^2}\right]}}{[\sum(\alpha_j^2-\sigma^2)/((\lambda_j+1)^2)]/[\sum(\sigma^2+\lambda_j\alpha_j^2)/(\lambda_j(\lambda_j+1)^2)]}
}
\item{dILE}{By Liu, (2011): \eqn{\frac{\sum_{j=1}^n \left(\frac{\widetilde{e}_i}{1-g_{ii}}\left(\frac{\widetilde{e}_i}{1-h_{1-ii}}-\frac{\hat{e}_i}{1-h_{ii}}\right)\right)} {\sum_{i=1}^n \left(\frac{\widetilde{e}_i}{1-g_{ii}}-\frac{\hat{e}_i}{1-h_{ii}}\right)^2}}{[\sum(\widetilde{e}_i/(1-g_{ii})*(\widetilde{e}_i/(1-h_{1-ii})-(\hat{e}_i/(1-h_{ii}))))] /[\sum(\widetilde{e}_i/(1-g_{ii})-(\hat{e}_i/(1-h_{ii})))^2]},

where, \eqn{\hat{e}=y_i-x'_i(X'X-x_i x'_i)^{-1}(X'y-x_iy_i)}{\hat{e}=y_i-x'_i(X'X-x_i x'_i)^{-1}(X'y-x_iy_i)}, \eqn{\widetilde{e}=y_i-x'_i(X'X+I_p-x_i x'_i)^{-1}(X'y-x_iy_i)}{\widetilde{e}=y_i-x'_i(X'X+I_p-x_i x'_i)^{-1}(X'y-x_iy_i)}, \eqn{G=X(X'X+I_p)^{-1}X'}{G=X(X'X+I_p)^{-1}X'} and \eqn{H\cong X(X'X)^{-1}X'}{H=X(X'X)^{-1}X'}.}
\item{dmm}{Liu, (1993): \eqn{(1-\hat{\sigma}^2) \left[\frac{\sum_{j=1}^p\left[\frac{1}{\lambda_j(\lambda_j+1)}\right]}{\sum_{j=1}^p\left[\frac{\hat{\alpha}_j^2}{(\lambda_j+1)^2}\right]}\right]}{(1-\hat{\sigma}^2)[\sum(1/(\lambda_j(\lambda_j+1)))/(\sum(\hat{\alpha}_j^2/(\lambda_j+1)^2))]}}
\item{dcl}{By Liu, (1993): \eqn{(1-\hat{\sigma}^2)\left[\frac{\sum_{j=1}^p\left[\frac{1}{(\lambda_j+1)}\right]}{\sum_{j=1}^p \left[\frac{\lambda_j\hat{\alpha}_j^2}{(\lambda_j+1)^2}\right]}\right]}{(1-\hat{\sigma}^2)[\sum(1/(\lambda_j+1))/(\sum((\lambda_j\hat{\alpha}_j^2)/(\lambda_j+1)^2))]}.}
\item{GCV}{GCV criterion for selection of optimal \eqn{d}{d}, that is, \eqn{GCV=\frac{SSR_d}{(n-1-trace(H_d))}}{GCV=(SSR_d)/(n-1-trace(H_d))}, where \eqn{SSR_d}{SSR_d} is residuals sum of squares from Liu regression at certain value of \eqn{d}{d} and \eqn{trace(H_d)}{trace(H_d)} is trace of hat matrix from Liu.}
\item{dGCV}{returns value of \eqn{d}{d} at which GCV is minimum.}
}

\references{
Akdeniz, F. and Kaciranlar, S. (1995). On the Almost Unbiased Generalized Liu Estimators and Unbiased Estimation of the Bias and MSE. \emph{Communications in Statistics-Theory and Methods}, \strong{24}, 1789--1897. \url{http://doi.org/10.1080/03610929508831585}.

Imdad, M. U. (2017). \emph{Addressing Linear Regression Models with Correlated Regressors: Some Package Development in R} (Doctoral Thesis, Department of Statistics, Bahauddin Zakariya University, Multan, Pakistan).

Imdadullah, M., Aslam, M., and Altaf, S. (2017). liureg: A comprehensive R Package for the Liu Estimation of Linear Regression Model with Collinear Regressors. \emph{The  R Journal}, \strong{9} (2), 232--247.

Liu, K. (1993). A new Class of Biased Estimate in Linear Regression. \emph{Journal of Statistical Planning and Inference}, \strong{141}, 189--196. \url{http://doi.org/10.1080/03610929308831027}.

Liu, X-Q. (2011). Improved Liu Estimator in a Linear Regression Model. \emph{Journal of Statistical Planning and Inference},141, 189--196. \url{https://doi.org/10.1016/j.jspi.2010.05.030}.

Ozkale, R. M. and Kaciranlar, S. (2007). A Prediction-Oriented Criterion for Choosing the Biasing Parameter in Liu Estimation. \emph{Commincations in Statistics-Theory and Methods}, \strong{36}(10): 1889--1903. \url{http://doi.org/10.1080/03610920601126522}.
}

\author{Muhammad Imdad Ullah, Muhammad Aslam}

\seealso{Liu model fitting \code{\link{liu}}, Liu residuals \code{\link{residuals.liu}}, Liu PRESS \code{\link{press.liu}}, Testing of Liu coefficients \code{\link{summary.liu}} }

\examples{
mod<-liu(y ~ ., data = as.data.frame(Hald), d = seq(-5, 5, 0.1))
dest(mod)
## Vector of GCV values for each d
dest(mod)$GCV
}
\keyword{ Liu biasing parameter }
