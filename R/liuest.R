liuest <-
  function(formula, data, d = 1.0, scaling = c("centered", "sc", "scaled"), ...) {
    if (is.null(d)) {
      d <- 1
    } else {
      d <- d
    }

    mf <- model.frame(formula = formula, data = data)
    x <- model.matrix(attr(mf, "term"), data = mf)
    y <- model.response(mf)
    mt <- attr(mf, "terms")

    p <- ncol(x)
    n <- nrow(x)

    if (Inter <- attr(mt, "intercept")) {
      Xm <- colMeans(x[,-Inter])
      Ym <- mean(y)
      Y <- y - Ym
      p <- p - 1
      X <- x[,-Inter] - rep(Xm, rep(n,p))
    } else{
      Xm <- colMeans(x)
      Ym <- mean(y)
      Y <- y - Ym
      X <- x - rep(Xm, rep(n,p))
    }

    scaling <- match.arg(scaling)

    if (scaling == "sc") {
      Xscale <- (drop(rep(1 / (n - 1), n) %*% X ^ 2) ^ 0.5) * sqrt(n - 1)
    } else if (scaling == "scaled") {
      Xscale <- drop(rep(1 / (n - 1), n) %*% X ^ 2) ^ 0.5
    } else{
      Xscale <- drop(rep(1,p))
      #names(Xscale)<-colnames(X)
    }

    X <- X / rep(Xscale, rep(n,p))

    bols <- lm.fit(X , as.matrix(Y))$coefficients
    #    Xs <- svd(X)
    #    rhs <- t(Xs$u)%*%Y
    #    dx <- Xs$d
    #    div <- lapply(d, function(x){dx^2+x})
    #    a <- lapply(div, function(x){drop(dx*rhs)/x})
    #    coef<-solve(t(X)%*%X + diag(p)) %*% (t(X)%*%X+d*diag(p))%*% bols

    coef <-
      lapply(d, function(d) {
        (solve(t(X) %*% X + diag(p)) %*% (t(X) %*% X + d * diag(p))) %*% bols
      })
    coef <- do.call(cbind, coef)
    #coef <-lapply(a, function(x){Xs$v %*% x})
    #coef <-do.call(cbind, coef)
    rownames(coef) <- colnames(X)
    colnames(coef) <- paste("d=", d, sep = "")

    lfit <- apply(coef, 2, function(x) {
      X %*% x
    })
    #Z <- lapply(d, function(x){solve(crossprod(X,X)+diag(x,p))%*%t(X)})

    list(
      coef = coef, xscale = Xscale, xs = X,  Inter = Inter, xm = Xm, y = Y,
      scaling = scaling, call = match.call(), d = d, lfit = lfit, mf =mf, terms = mt
      #ym =Ym,
    )
  }
