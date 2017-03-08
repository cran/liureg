plot.liu <- function(x, abline=TRUE,...) {
  d <- x$d
  coefs <- x$coef
  lends = rownames(coefs)
  if (length(d) == 1) {
    plot(x = rep(d,length(coefs)), y = coefs, xlab = "Biasing Parameter d", ylab =
        "Liu Coef", main = "Liu Trace", col = 1:ncol(x$xs), pch = 19,...)
    legend("topright", legend = lends, lwd = 2, cex = .6, pt.cex = .8,
           col = 1:ncol(x$xs), y.intersp =.5  )
  } else {
    matplot(x = d, y = t(coefs), xlab = "Biasing Parameter d", ylab = "Liu Coef", main =
        "Liu Trace",col = 1:ncol(x$xs), lty = 1:ncol(x$xs),lwd = 2, type = "l",...)
    legend("topright", legend = lends, lwd = 2, cex = .6, pt.cex = .8,
           col = 1:ncol(x$xs), y.intersp =.5)
  }

  if (abline) {
#    abline(h = 0, lty = 2)
    abline(v = x$d[which.min(lstats(x)$mse)], lty = 2, col = "red")
    #  text(K[which.min(rstats1(x)$mse)], min(coefs), paste("K=", K[which.min(rstats1(x)$mse)]), pos = 4, col = "red")
    #  text(K[which.min(rstats1(x)$mse)], max(coefs), paste("MSE=", round(min(rstats1(x)$mse), 3) ), pos=4, col="red" )
    text(x$d[which.min(lstats(x)$mse)], max(x$coef),
         paste(c("minimum MSE =", " at d="),
         c(round(min(lstats(x)$mse), 3), x$d[which.min(lstats(x)$mse)]),collapse = '' ),
         pos=4, col = "red")
  }
}
