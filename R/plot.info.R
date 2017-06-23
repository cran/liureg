plot.infoliu<-function(x, abline=TRUE,...){

  df <- as.vector(lapply(hatl(x), function(x) {
    sum(diag(x))
  }))

  aic<-infoliu(x)[,1]
  bic<-infoliu(x)[,2]
  col=cbind("black", "red")
  mse<-lstats(x)$mse
  dfminmse<-df[which.min(mse)]
  mselect<-cbind(aic, bic)

  if(length(x$d)==1){
    plot(x=rep(df, length(mselect)),
         y=mselect,
         main="Model Selection Criteria",
         xlab="DF",
         ylab="Model Criteria",
         col=col,
         lwd=2,
         lty=c(1,4)
         )
  }
  else{
    matplot(df,
            mselect,
            main="Model Selection Criteria",
            xlab='DF',
            ylab='Model Criteria',
            col=col,
            lwd=2,
            type='l',
            lty=c(1,4)
            )
  }

legend("topright",
       legend=c("AIC", "BIC"),
       col=col,
       lwd=2,
       bty="o",
       bg="transparent",
       lty=c(1,4),
       cex=.7,
       pt.cex=.5,
       y.intersp = .4,
       x.intersp=.3,
       merge = TRUE
       )

if(abline){
  abline(v=dfminmse,
         lty=2,
         col="blue"
         )
  text(dfminmse,
       max(mselect),
       paste("Minimum mse=", round(min(mse),3)),
       col="blue",
       pos=1
       )
  text(dfminmse,
       min(mselect),
       paste("df=", round(as.numeric(df[which.min(mse)]), 3)),
       col="blue",
       pos=4
       )
}

}
