vcov.liu <- function(object, ...) {
  resid <- resid(object)
  n <- nrow(resid)
  x <- object$xs
  p <- ncol(x)
  d <- object$d

  redf <-
    lapply(hatl(object), function(x) {
      n - sum(diag(2 * x - x %*% t(x)))
    })
  lsigma2 <-
    mapply(function(x, y) {
      x / y
    }, colSums(resid ^ 2), redf, SIMPLIFY = FALSE)

  A <-
    lapply(d, function(d) {
      solve(t(x) %*% x + diag(p)) %*% (t(x) %*% x + d * diag(p))
    })

  B <-
    lapply(d, function(d) {
      solve(t(x) %*% x) %*% (t(x) %*% x + d * diag(p)) %*% solve(t(x) %*% x +
                                                                   diag(p))
    })
  C <- mapply(function(x,y) {
    x %*% y
  }, A, B, SIMPLIFY = FALSE)

  vcovbl <- mapply(function(x,y) {
    x * y
  }, lsigma2, C, SIMPLIFY = FALSE)
  vcovbl
}
