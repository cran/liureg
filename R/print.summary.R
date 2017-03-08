print.summary.liu <-
  function(x, digits = max(4, getOption("digits") - 3),
           signif.stars = getOption("show.signif.stars"),...) {
    summaries <- x$summaries
    cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep =
          "")
    for (i in seq(length(summaries))) {
      CSummary <- summaries[[i]]
      cat("\nCoefficients for Liu parameter d=", CSummary$d, "\n")
      coefs <- CSummary$coefficients
      printCoefmat(
        coefs, digits = digits, signif.stars = signif.stars, P.values = TRUE,
        has.Pvalue = TRUE, na.print = "NA", ...
      )
      cat ("\nLiu Summary\n")
      print(CSummary$stats,digits,...)
      #cat("Liu MSE=", CSummary$mse,"\n")
      cat("-------------------------------\n\n")
      invisible(x)
    }
  }
