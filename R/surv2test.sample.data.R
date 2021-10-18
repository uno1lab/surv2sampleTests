#' @name surv2test.sample.data
#' @aliases  surv2test.sample.data
#' @title Generate a sample data from the pbc data
#' @description This is a function to retrieve 312 randomized patients from the pbc data in survival package.
#' @usage surv2test.sample.data()
#' @details The function creates a sample dataset to illustrate the usage of the function \code{surv2test()} in this package.
#' The original pbc data in \code{survival} package consists of 418 patients data.
#' This function loads the pbc data, select the 312 patients who were randomized.
#' The status variable is edited, so that 1 indicates death and 0 indicates alive.
#' @seealso \code{pbc} in survival package
#' @examples
#' D=surv2test.sample.data()
#' head(D)
#' @export
#######################################
# surv2test sample data
#######################################
surv2test.sample.data <- function(){
  tmp = survival::pbc
  D   = tmp[1:312,c(2:4)]

  D$time   = D$time/365.25
  D$status = as.numeric(D$status==2)
  D$arm    = as.numeric(D$trt==1)

  DA = D[,-3]
  DA
}
NULL
