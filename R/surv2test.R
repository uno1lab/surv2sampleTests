#' @name surv2test
#' @aliases surv2test
#' @title Conducting Two Sample Tests with Time-To-Event Data
#' @description Performs two-sample comparisons using various tests.
#' The current version includes the Max-Combo test and the t-year event rate difference test
#' that were used in Horiguchi et al. (2020) <doi:10.1177/1740774520940256>. All tests are two-sided where the alternative hypothesis is that
#' treatment group is not equal to control group with respect to survival. For the Max-Combo test, we used a purmutation resampling method to determine the null distribution of the test statistic under the null hypothesis.
#' @author Miki Horiguchi, Hajime Uno
#' @references
#' Horiguchi M, Hassett MJ, Uno H. Empirical power comparison of statistical tests in contemporary phase III randomized controlled trials
#' with time-to-event outcomes in oncology. Clinical Trials 2020. doi:10.1177/1740774520940256.
#' @usage surv2test(time, status, arm, tau, nmethod=10000, seed=1)
#' @param time The follow-up time for right censored data.
#' @param status The status indicator, 1=event, and 0=right censored.
#' @param arm The group indicator for comparison. The elements of this vector take either 1 or 0. Normally, 0=control group, 1=active treatment group.
#' @param tau A scaler value to specify the specific time point for calculating the t-year event rate difference.
#' \code{tau} needs to be smaller than the minimum of the largest observed time in each of the two groups. When \code{tau = NULL}, the default value (i.e., the minimum of the largest observed time in each of the two groups) is used.
#' @param nmethod A number of iterations for the resampling method. Recommended to specify at least 10000 (default) or larger.
#' @param seed An integer value, used for the random number generation in the resampling procedures. Default is \code{NULL}.
#' @return an object of class surv2test.
#' @return \item{tau}{the specific time point used for the t-year event rate differnce test}
#' @return \item{note}{a note regarding the specific time point}
#' @return \item{tyear_pval}{p-value of the t-year event rate difference test}
#' @return \item{maxcombo_pval}{p-value of the Max-Combo test}
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

#'@export
surv2test <- function(time, status, arm, tau=NULL, nmethod=10000, seed=1){
  #==================================
  #  initial check
  #==================================
  #-- tau --
  idx=arm==0; tt=time[idx]; tt0max=max(tt); ss=status[idx]; ss0max=min(ss[tt==tt0max]);
  idx=arm==1; tt=time[idx]; tt1max=max(tt); ss=status[idx]; ss1max=min(ss[tt==tt1max]);

  ttmax = max(tt0max, tt1max)
  ttmin = min(tt0max, tt1max)

  #--case 1: the last obs (smaller one)=event, the last obs (longer one)=event
  if(ss0max==1 & ss1max==1){
    if(!is.null(tau)){
      if(tau>ttmax){stop(paste("The specific time point, tau, needs to be shorter than or equal to ", round(ttmax, digits=2)))}
      if(tau<=ttmax){tau=tau; NOTE=paste("The specific time point: tau =", tau, " was specified.")}
    }else{
      tau = ttmax
      NOTE=(paste("The specific time point, tau, was not specified. Thus, the default tau ", round(ttmax, digits=2)," is used."))
    }
  }

  #--case 2: the last obs (smaller one)=event, the last obs (longer one)=censor
  if((ss0max==0 & ss1max==1 & tt0max>=tt1max) | (ss0max==1 & ss1max==0 & tt1max>tt0max)){
    if(!is.null(tau)){
      if(tau>ttmax){stop(paste("The specific time point, tau, needs to be shorter than or equal to ", round(ttmax, digits=2)))}
      if(tau<=ttmax){tau=tau; NOTE=paste("The specific time point: tau =", tau, " was specified.")}
    }else{
      tau = ttmax
      NOTE=paste("The specific time point, tau, was not specified. Thus, the default tau ", round(ttmax, digits=2)," is used.")
    }
  }

  #--case 3: the last obs (smaller one)=censor, the last obs (longer one)=event
  if((ss0max==1 & ss1max==0 & tt0max>=tt1max) | (ss0max==0 & ss1max==1 & tt1max>tt0max)){
    if(!is.null(tau)){
      if(tau>ttmin){stop(paste("The specific time point, tau, needs to be shorter than or equal to ", round(ttmin, digits=2)))}
      if(tau<=ttmin){tau=tau; NOTE=paste("The specific time point: tau =", tau, " was specified.")}
    }else{
      tau = ttmin
      NOTE=(paste("The specific time point, tau, was not specified. Thus, the default tau ", round(ttmin, digits=2)," is used."))
    }
  }

  #--case 4: the last obs (smaller one)=censor, the last obs (longer one)=censor
  if(ss0max==0 & ss1max==0){
    if(!is.null(tau)){
      if(tau<=ttmin){
        NOTE=paste("The specific time point: tau =", tau, " was specified.")
      }
      if(tau>ttmin){
        stop(paste("The specific time point, tau, needs to be shorter than or equal to the minimum of the largest observed time on each of the two groups: ", round(ttmin, digits=2)))
      }
    }else{
      tau = ttmin
      NOTE=(paste("The specific time point, tau, was not specified. Thus, the default tau ", round(ttmin, digits=2)," is used."))
    }
  }

  #-- output --
  XZ=list()
  XZ$tau=tau
  XZ$note=NOTE

  #==================================
  #  tests
  #==================================
  #-- data --
  dat = data.frame(time=time, status=status, arm=arm)

  #-- t-year test (cloglog) --
  dat1 = dat[dat$arm==1,]
  dat0 = dat[dat$arm==0,]

  kw1 = cloglog(dat1$time, dat1$status, t0=tau)
  kw0 = cloglog(dat0$time, dat0$status, t0=tau)

  diff    = kw1$surv-kw0$surv
  se      = sqrt(kw1$var + kw0$var)
  z_value = abs(diff/se)
  ty_pval = (1-pnorm(z_value))*2


  #-- Max-combo test --
  mc = maxcombo2(dat, nmethod=nmethod, seed=seed)
  mc_pval = mc$p_value


  #-- Output --
  XZ$tyear_pval    = ty_pval
  XZ$maxcombo_pval = mc_pval

  class(XZ) = "surv2test"
  return(XZ)
}
NULL
