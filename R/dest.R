dest <- function(object,...)
  UseMethod("dest")
dest.liu <- function(object,...) {
  x <- object$xs
  y <- object$y
  p <- ncol(x)
  n <- nrow(x)
  d <- object$d
  EVal <- eigen(t(x) %*% x)$values
  EVec <- eigen(t(x) %*% x)$vectors

  ols <- lm.fit(x, y)

  coefols  <- ols$coef
  fittedols<- ols$fitted.values
  residols <- ols$residuals

  sigma2 <- sum(residols ^ 2) / (n - p)

  alphaols <- t(EVec) %*% coefols
  rownames(alphaols) <- colnames(x)

  #dnum<-sum((alphaols^2 - sigma2)/(EVal+1)^2)
  #ddnum <- sum((sigma2 + EVal%*%alphaols^2)/(EVal*(EVal+1)^2))
  #d <- dnum/ddnum

  diaghat <- lapply(hatl(object), function(x) {
    diag(x)
  })
  diaghat <- do.call(cbind, diaghat)

  SSER<-lstats(object)$SSER

  GCV<-matrix(0,1,nrow=length(d) )

  for(i in seq(length(d))){
    GCV[i,]<-SSER[i]/(n-1-sum(diaghat[,i]))^2
  }
  rownames(GCV) <- paste("d=", d, sep = "")
  colnames(GCV) <- c("GCV")
  if (length(GCV) > 0) {
    l <- seq_along(GCV)[GCV == min(GCV)]
    dGCV <- object$d[l]
  }
  EVal1<-EVal+1
  #Liu, 1993
  dopt<-(sum((alphaols^2-sigma2)/EVal1^2))/(sum((sigma2+EVal*alphaols^2)/(EVal*EVal1^2)))

  #Liu, 1993 estimate of d
  numdmm <- 1 / (EVal * EVal1)
  dnumdmm <- (alphaols ^ 2) / (EVal1 ^ 2)
  dmm  <- 1 - (sigma2 * sum(numdmm) / sum(dnumdmm) )

  #Liu, 1993
  numdcl <- 1 / EVal1
  dnumdcl <- (EVal * alphaols ^ 2) / (EVal1 ^ 2)
  dcl  <- 1 - sigma2 * (sum(numdcl) / sum(dnumdcl))

  #Improved d
  xtx<-t(x)%*%x
  xty<-t(x)%*%y
  F1<-numeric(n)
  F2<-numeric(n)
  for(i in 1:n){
  #ehat
    ehat<-y[i]-(t(x[i,])%*%solve(xtx-x[i,]%*%t(x[i,]))%*%(xty-x[i,]*y[i]))
  #etild
    etild<-y[i]-(t(x[i,])%*%solve(xtx+diag(p)-x[i,]%*%t(x[i,])) %*% (xty-x[i,]*y[i]))

    H= (x%*%solve(xtx))%*%t(x)
    G=x%*%solve(xtx+diag(p))%*%t(x)
  #h<-as.vector(diag(H))
  #g<-as.vector(diag(G))
    F1[i] <- ((etild/(1-diag(G)[i])) *
             (etild/(1-diag(H)[i]) - ehat/(1-diag(H)[i])))
  #F1=sum((e1/(1-g))*((e1/(1-g))-(e/(1-h))))
    F2[i] <- (etild/(1-diag(G)[i]) - (ehat/(1-diag(H)[i])))
# F2=sum((e1/(1-g))-(e/(1-h)))     # h(1-ii) are same for g
}
  dILE=sum(F1)/sum(F2)

  desti <- list(
    dmm = dmm,
    dcl = dcl,
    GCV = GCV,
    dGCV=dGCV,
    dopt=dopt,
 #   sigma2=sigma2,
    dILE=dILE
    )
  class(desti) <- "dliu"

  desti
}

print.dliu <- function(x,...) {
  cat("Liu biasing parameter d\n")

  dest <- cbind(
    dmm=x$dmm,
    dcl=x$dcl,
    dopt=x$dopt,
    dILE=x$dILE,
    dGCV=x$dGCV
    )

  rownames(dest) <- "d values"
  colnames(dest) <- c("dmm",
                      "dcl",
                      "dopt",
                      "dILE",
                      "min GCV at")
  print(t(round(dest,5)),...)
}
