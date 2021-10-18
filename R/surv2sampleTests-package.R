#' @name surv2sampleTests-package
#' @aliases  surv2sampleTests-package
#' @docType  package
#' @title Conducting Two Sample Tests with Time-To-Event Data
#' @description
#' Performs two-sample comparisons using various tests. The current version includes the Max-Combo test and the t-year event rate difference test that were used in Horiguchi et al. (2020) <doi:10.1177/1740774520940256>.
#' @author Miki Horiguchi, Hajime Uno
#'
#' Maintainer: Miki Horiguchi <horiguchimiki@gmail.com>
#' @references
#' Horiguchi M, Hassett MJ, Uno H. Empirical power comparison of statistical tests in contemporary phase III randomized controlled trials
#' with time-to-event outcomes in oncology. Clinical Trials 2020. doi:10.1177/1740774520940256.
#' @keywords
#' survival
#' @seealso
#' survival
#' @import survival
#' @importFrom stats pnorm
#' @examples
#' #--- sample data ---#
#' D=surv2test.sample.data()
#' time=D$time
#' status=D$status
#' arm=D$arm
#' nmethod = 100 #This is only for example use.
#'               #Recommended to specify at least 10000 (default) or larger.
#' a=surv2test(time, status, arm, tau=10, nmethod=nmethod, seed=123)
#' print(a)
NULL
