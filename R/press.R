press <- function(object, predr=FALSE, ...)
  UseMethod("press")
press.liu <- function(object, predr=FALSE, ...) {

  #liu residuals
  res <- resid(object)
  #ols residuals
  resols <- lm.fit(object$xs, object$y)$residuals

#  n <- nrow(res)
#  d <- object$d
  x <- object$xs
  p <- ncol(x)

#  diaghat<-lapply(hatl(object), function(x){diag(x)})
#  diaghat<-do.call(cbind, diaghat)

#h1ii <- numeric(n)
#htd <- numeric(n)
#diaghat <- numeric(n)

  #Liu Hat matrix
  dhat<-lapply(hatl(object), function(x){diag(x)})
  dhat<-do.call(cbind, dhat)

  #ols Hat matrix
  hii <-diag(x%*%solve(t(x)%*%x)%*%t(x))
  h1ii<-diag(x%*%solve(t(x)%*%x+diag(p))%*%t(x))

  A<-res/(1-h1ii)
  B<-resols/((1-h1ii)*(1-hii))
  C<-h1ii-dhat
  press<- A-(B*C)

  if(predr==TRUE){
      round(press, 5)
  }else{
    round(colSums(press^2),5)
  }
  #press

  #   for (i in 1:n) {
  #   diaghat[i] <- t(x[i,]) %*% solve(t(x) %*% x) %*% x[i,]
  #   h1ii[i]    <- t(x[i,]) %*% solve(t(x) %*% x + diag(p)) %*% x[i,]
  #   htd[i] <- (1 - d) * h1ii[i] + d * diaghat[i]
  # }

  #  A <- res / (1 - h1ii)
  #  B <- resols / ((1 - h1ii) %*% (1 - diaghat))
  #  C <- h1ii - htd

  # press <- A - B * C
  # press
}
