fitted.liu <- function(object,...) {
  lfit1 <- object$lfit
  fit<-(round(lfit1, 5))
  rownames(fit)<-paste(seq(1:nrow(object$xs)))
  fit
}
