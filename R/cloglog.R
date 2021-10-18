#################################################################
# Complementary log-log of S(t) and V[S(t)] (one-arm) -- hidden
#################################################################
cloglog <- function(time, status, t0){

  #--data frame--
  indat = data.frame(time, status)
  names(indat) = c("time", "status")

  #--
  ft     = survfit(Surv(time, status)~1, indat)
  time.a = ft$time

  fixt_i = t0
  wka    = time.a
  wka[time.a>fixt_i] = 0
  index.a = which(wka==max(wka))
  surv.a  = ft$surv[index.a]

  #-- V[-log(S(t))] --
  nlog.st.se.a = ft$std.err[index.a]
  nlog.st.var.a = nlog.st.se.a^2

  #-- complementary log-log --
  csurv = log(-log(surv.a))
  var_csurv = (1/-log(surv.a))^2*nlog.st.var.a

  #--output--
  Z=list()
  Z$surv = csurv
  Z$var  = var_csurv
  Z$se   = sqrt(var_csurv)

  class(Z) = "cloglog"

  return(Z)
}

##################
# Example
##################
# D    = surv2test.sample.data()
# t0   = median(D$time)
# dat1 = D[D$arm==1,]

# cloglog(dat1$time, dat1$status, t0)
