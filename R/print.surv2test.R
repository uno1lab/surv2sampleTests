#' @name print.surv2test
#' @aliases print.surv2test
#' @title print.surv2test
#' @description S3 method for class 'surv2test'
#' @param x Object to be printed
#' @param digits Integer indicating the number of decimal places
#' @param ... Further arguments ignored in this function
#' @export
######################################
# print.surv2test (hidden)
######################################
print.surv2test <- function(x, digits=3, ...){

  cat("\n")

  tau   = x$tau
  tyear = round(x$tyear_pval, digits=digits)
  mc    = round(x$maxcombo_pval, digits=digits)

  cat ("<Test result> \n")

  cat("Specific time point:", tau, "\n")
  cat("\n")
  cat("P-value (T-year event rate difference test):", tyear,  "\n")
  cat("\n")
  cat("P-value (Max-Combo test):", mc,  "\n")
  invisible(x)
}
NULL
