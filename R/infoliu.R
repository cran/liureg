infoliu<-function(object, ...)
  UseMethod("infoliu")
infoliu.liu<-function(object, ...){

  SSER<-apply(resid(object),2, function(x){sum(x^2)})

  df <- as.vector(lapply(hatl(object), function(x) {
    sum(diag(x))
  }))

  n<-nrow(object$xs)
  AIC<-mapply(function(x,y){n*log(x/n)+2*y}, SSER, df, SIMPLIFY = FALSE)
  AIC<-do.call(cbind,AIC)
  BIC<-mapply(function(x,y){n*log(x)+y*log(n)}, SSER, df, SIMPLIFY = FALSE)
  BIC<-do.call(cbind,BIC)
  resinfo<-rbind(AIC, BIC)
  rownames(resinfo)<-c("AIC", "BIC")
  t(resinfo)
}
