coef.liu <- function(object, ...) {
  scaledcoef <- t(as.matrix(object$coef / object$xscale))
  if (object$Inter) {
    inter <- mean(object$mf[,1]) - scaledcoef %*% object$xm
    scaledcoef <- cbind(Intercept = inter, scaledcoef)
    colnames(scaledcoef)[1] <- "Intercept"
  }else{
    scaledcoef <- t(as.matrix(object$coef / object$xscale))
  }

  drop(scaledcoef)
}
