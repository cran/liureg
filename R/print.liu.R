print.liu <- function(x, digits = max(5,getOption("digits") - 5),...) {
  cat("Call:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep =
        "")
  #print(coef(x),digits=digits, ...)
  print(round(coef(x),digits=digits))
  cat("\n")
  invisible(x)

}
