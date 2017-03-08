hatl <- function(object, ...)
  UseMethod("hatl")
hatl.liu <- function(object, ...) {

  x <- object$xs
  d <- object$d
  p <- ncol(x)

  hatl <- lapply(d, function(d) {
    x%*%solve(t(x)%*%x+diag(p))%*%(t(x)%*%x+d*diag(p))%*%solve(t(x)%*%x)%*%t(x)
  })
  names(hatl) <- paste("d=", d, sep = "")
  hatl
}
