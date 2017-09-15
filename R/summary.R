summary.liu <- function(object,...) {
  res <- vector("list")
  res$call <- object$call
  res$d <- object$d
  y <- object$y
  n <- nrow(object$xs)
  lcoef <- object$coef
  vcov <- vcov(object)
  ym <- mean(object$mf[,1])

  SE <- lapply(vcov, function(x) {
    sqrt(diag(x))
  })
  SE <- do.call(cbind, SE)

  rownames(SE) <- rownames(lcoef)
  colnames(SE) <- colnames(lcoef)

  tstats <- lcoef / SE
  pvalue <- 2 * (1 - pnorm(abs(tstats)))

  summaries <- vector("list", length(res$d))

  coefs <- rbind(coef(object))
#  resid <- resid(object)
#  SSER  <- apply(resid, 2, function(x) {
#    sum(x ^ 2)
#  })
#  SSTR  <- t(y) %*% y

#  R2l <- lapply(SSER, function(x) {
#    1 - x / SSTR
#  })

  #seb0 <- 1 / n * var(y) + 1 / n * colSums(lcoef ^ 2)
  b0 <- ym - colSums((lcoef * object$xm))
  seb0<-numeric(length(res$d))
  pint<-numeric(length(res$d))
  for (i in seq(length(res$d))) {
        seb0[i]<-sqrt(var(y)/n+ (object$xm^2)%*%diag(vcov(object)[[i]]))
        pint[i]<-as.numeric(b0[i]/seb0[i])
    summary <- vector("list")
    if (object$Inter) {
      summary$coefficients <-
        cbind(coefs[i,], c(b0[i], lcoef[,i]), c(seb0[i], SE[,i]),
              c(pint[i], tstats[,i]), c(2*(1-pnorm(abs(pint[i]))), pvalue[,i]))
      colnames(summary$coefficients) <-
        c("Estimate", "Estimate (Sc)", "StdErr (Sc)",
          "t-val (Sc)", "Pr(>|t|)")
      #summary$stats <- cbind(R2l[i], lstats(object)$adjR2l,lstats(object)$Fv[i],
#                             infoliu(object)[i,1], infoliu(object)[i,2])
      #colnames(summary$stats)<-c("R2","adj-R2","F","AIC", "BIC")
      #summary$lmse<-lstats(object)$mse
    } else {
      summary$coefficients <-
        cbind(coefs[i,-1], lcoef[,i], SE[,i], tstats[,i], pvalue[,i])
      colnames(summary$coefficients) <-
        c("Estimate", "Estimate (Sc)", "StdErr (Sc)",
          "t-val (Sc)", "Pr(>|t|)")
    }

    summary$stats <- cbind(lstats(object)$R2[i], lstats(object)$adjR2[i], lstats(object)$Fv[i],
                           infoliu(object)[i,1], infoliu(object)[i,2], lstats(object)$mse[i])
    colnames(summary$stats)<-c("R2","adj-R2","F","AIC", "BIC", "MSE")
rownames(summary$stats)<-paste("d=", object$d[i], sep="")
    summary$d <- object$d[i]
    summaries[[i]] <- summary
    names(summaries)[[i]] <- paste("summary", i, sep = "")
    rm(summary)
  }
  res$summaries <- summaries
  class(res) <- "summary.liu"
  res

}
