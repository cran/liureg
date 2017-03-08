residuals.liu <- function(object, ...) {
  y <- object$y
  lfitted <- object$lfit
  resid <- apply(lfitted, 2, function(x) {
    y - x
  })

  #class(resid)<-"liuresid"

  resid
}

# print.residuals <- function(x,  ...) {
#   cat("Call:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep =
#         "")
#
#   print(resid(x), ...)
#   cat("\n")
#   invisible(x)
# }
