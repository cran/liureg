liu <- function(formula, data, d = 1, scaling=c("centered", "sc", "scaled"), ...)

  UseMethod("liu")

liu.default <- function(formula, data, d = 1,
                        scaling=c("centered", "sc", "scaled"), ...) {
  #    x<-as.matrix(x)
  #    y<-as.matrix(y)

  est <- liuest(formula, data, d, scaling=scaling, ...)

  #est$fitted.values <- as.vector(est$xs %*% est$coef)
  #est$residuals <- as.vector(est$y - est$fitted.values)

  est$call <- match.call()

  class(est) <- "liu"
  est
}
