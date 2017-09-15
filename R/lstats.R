lstats <- function(object,...)
  UseMethod("lstats")
lstats.liu <- function(object,...) {
  y <- object$y
  resid <- resid(object)
  n <- nrow(resid)
  d <- object$d
  x <- object$xs
  coef<-object$coef
  p<-ncol(x)

  Eval <- eigen(t(x) %*% x)$values
  Evec <- eigen(t(x) %*% x)$vector

  SSER <- apply(resid, 2, function(x) {
    sum(x ^ 2)
  })

  SSRR <- apply(object$lfit, 2, function(x) {
    sum(x ^ 2)
  })

  SSTR <- t(y) %*% y

  #residual effective degree of freedom (Hastie and Tibshirani, 1990)
  ledf <- lapply(hatl(object), function(x) {
    n - sum(diag(2 * x - x %*% t(x)))
  })
  ledf <- do.call(rbind,ledf)
  rownames(ledf) <- paste("d=", d, sep = "")
  colnames(ledf) <- c("EDF")

  lsigma2 <- mapply(function(x,y) {
    x / y }, SSER, ledf, SIMPLIFY = FALSE)
  lsigma2 <- do.call(rbind, lsigma2)
  rownames(lsigma2) <- paste("d=", d, sep = "")
  colnames(lsigma2) <- c("Sigma2")

  diaghat <- lapply(hatl(object), function(x) {
    diag(x)
  })
  diaghat <- do.call(cbind, diaghat)

  Cl <- lapply(1:length(d), function(i, SSRl, lsigma2, hatL) {
    SSRl[i] / lsigma2[i] - n + 2 + 2 * sum(diaghat[,i])
  }, SSRl = SSER, hatL = hatl(object), lsigma2 = lsigma2)
  Cl <- do.call(rbind, Cl)
  rownames(Cl) <- paste("d=", d, sep = " ")
  colnames(Cl) <- c("CL")

  bols <- lm.fit(x,y)$coef
  abeta<-bols%*%Evec
  #var <- mapply(function(x,y){x*y}, dsigma2, sum(((Eval+d)^2)/(Eval*(Eval+1)^2)),SIMPLIFY=FALSE)
  #var <- lapply(1:length(d), function(i, dsigma2){dsigma2[i]*sum(diag(vcov(object)[[i]]))},
  #              dsigma2=dsigma2)
  var <- lapply(vcov(object), function(x) {
    sum(diag(x))
  })
  var <- do.call(rbind, var)
  rownames(var) <- paste("d=", d, sep = "")
  colnames(var) <- c("VAR")

  bias2 <-
    lapply(d, function(d) {
      (d - 1) ^ 2 * sum( (abeta^2) / (Eval + 1) ^ 2)
    })
  bias2 <- do.call(rbind, bias2)
  rownames(bias2) <- paste("d=", d, sep = "")
  colnames(bias2) <- c("Bias^2")

  msel <- mapply(function(x,y) {
    x + y
  }, var, bias2, SIMPLIFY = FALSE)
  msel <- do.call(rbind, msel)
  rownames(msel) <- paste("d=", d, sep = "")
  colnames(msel) <- c("MSE")

  Fv<- lapply(1:length(d), function(i, b, v){1/p*t(b[,i])%*%solve(v[[i]])%*%b[,i]},
              b=coef, v=vcov(object))
  Fv<-do.call(rbind, Fv)
  rownames(Fv) <-paste("d=", d , sep="")
  colnames(Fv) <-c("F")

  R2l<-lapply(SSER, function(x){1-x/SSTR})
  R2l<-do.call(rbind,R2l)
  rownames(R2l) <-paste("d=", d, sep="")
  colnames(R2l)<-c("R2")

  adjR2l<-1-(n-1)/(n-p-1)*(1-R2l)
  rownames(adjR2l) <-paste("d=", d, sep="")
  colnames(adjR2l) <-c("adj-R2")

  #minimum mse
  minmse<-d[which.min(msel)]

  lstat <-list(
      lEDF = ledf,
      lsigma2 = lsigma2,
      Cl = Cl,
#      Eval=Eval,
      var = var,
      bias2 = bias2,
      mse = msel,
      Fv=Fv,
      R2=R2l,
      adjR2=adjR2l,
#      d=d,
      minmse=minmse,
      SSER=SSER
    )

  class(lstat) <- "lstats"
  lstat
}

print.lstats <- function(x, ...) {
  cat("\nLiu Regression Statistics:\n\n")
  res <-cbind(
      DEDF = x$lEDF,
      lsigma2 = x$lsigma2,
      Cl = x$Cl,
      var = x$var,
      bias2 = x$bias2,
      mse = x$mse,
      Fv=x$Fv,
      R2=x$R2,
      adjR2=x$adjR2
      )

  print(round(res,4), ...)
  #cat("\nminimum MSE occurred at d=", x$d[which.min(x$mse)], "\n")
  cat("\nminimum MSE occurred at d =", x$minmse, "\n")

  }
