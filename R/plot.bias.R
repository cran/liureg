plot.biasliu<- function(x, abline=TRUE,...){
  bias2<-lstats(x)$bias2
  var<-lstats(x)$var
  mse<-lstats(x)$mse
  minmse<-min(mse)
  mind<-x$d[which.min(mse)]
  col=cbind("black", "red", "green")
  liutrace<-cbind(var, bias2, mse)

  if(length(x$d)==1){
    plot(x=rep(x$d, length(liutrace)), y=liutrace, main="Bias, Variance Trade-off",
         xlab="Liu Biasing Parameter", ylab="", col=col, lwd=2, lty=c(1,4,5))
    legend("topright", legend=c("var", "bias^2","mse"),col=col, lwd=2, fill=1:3,
           lty=c(1,4,5), cex=.6, pt.cex=.7, bty="o", y.intersp = .7)
  } else{
    matplot(x$d, liutrace, main="Bias, Variance Trade-off", xlab="Liu Biasing Parameter",
            col=col, lwd=2, lty=c(1,4,5), type='l')

    legend("topright", legend=c("var", "bias^2", "mse"), col=col, lwd=2, fill=1:3,
           lty=c(1,4,5), cex=.6, pt.cex=.5, bty="o", y.intersp = .7)
  }

  if(abline){
    abline(v=mind, lty=2)
    abline(h=minmse, lty=2)
    text(mind, max(lstats(x)$mse), paste("mase=",round(minmse,3)),col="blue", pos=1)
    text(mind, minmse, paste("d=", mind), pos=4, col="blue")
  }
}
