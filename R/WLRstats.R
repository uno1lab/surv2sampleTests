##################################################################################################
# Calculates summary tables to derive WLR stat and its variance (single weight)  -- hidden
##################################################################################################
WLRstats <- function(time, status, arm, weight="LR", add.t.idx=NULL){

  #--- time index ---
  if(length(weight)>=2){stop()}
  if(is.numeric(weight)){add.t.idx = unique(c(add.t.idx, weight))}
  t.idx = sort(unique(c(0, time[status==1], add.t.idx)))
  tk = length(t.idx)

  #--- by arm ---
  x1 = time[arm==1]
  x0 = time[arm==0]
  status1 = status[arm==1]
  status0 = status[arm==0]
  m1 = length(x1)
  m0 = length(x0)
  mm = m1+m0

  #--- processes indexed by t.idx ---
  Y1 = rep(0,length(t.idx))
  Y0 = rep(0,length(t.idx))
  N1 = rep(0,length(t.idx))
  N0 = rep(0,length(t.idx))

  a1 <- function(v){sum(as.numeric(x1>=v))}
  a0 <- function(v){sum(as.numeric(x0>=v))}
  b1 <- function(v){sum(as.numeric(x1==v & status1==1))}
  b0 <- function(v){sum(as.numeric(x0==v & status0==1))}
  Y1 = sapply(t.idx, a1)
  Y0 = sapply(t.idx, a0)
  N1 = sapply(t.idx, b1)
  N0 = sapply(t.idx, b0)
  YY = Y1+Y0
  NN = N1+N0

  #--- KM with the pooled sample
  ft0 = survfit(Surv(time, status)~1)
  aa  = summary(ft0, time=t.idx)
  KMsurv  = aa$surv
  KMsurv_ = c(1,KMsurv[-length(KMsurv)])

  #--- weights ---
  if(weight=="LR"){wt = rep(1, tk)}
  if(weight=="GW"){wt = YY/mm}
  if(weight=="PPW"){wt = KMsurv_}
  if(weight=="Grho0gamma0"){wt = rep(1, tk)}
  if(weight=="Grho1gamma0"){wt = KMsurv_}
  if(weight=="Grho0gamma1"){wt = 1-KMsurv_}
  if(weight=="Grho1gamma1"){wt = KMsurv_*(1-KMsurv_)}
  if(weight=="Grho0gamma2"){wt = {1-KMsurv_ }^2}

  #--- weight used in Zhenzhen Xu et al. (2018) ---
  if (is.numeric(weight)){
    wt = rep(1, tk)
    wt[t.idx<weight]=0
  }

  #--- class K ---
  K = wt*(Y1*Y0)/(YY)*sqrt((mm)/(m0*m1))
  #--- W will be positive if arm 1 is better than arm 0 --
  W = - K*((N1/Y1)-(N0/Y0))
  S = K*K/(Y1*Y0)*(1-(NN-1)/(YY-1))*NN

  stats = data.frame(cbind(t.idx, Y1, Y0, N0, N1, YY, NN, KMsurv, KMsurv_, wt, K, W, S))
  #print(stats)

  #--- test stat ---
  Wk     = sum(stats$W, na.rm=TRUE)
  sigma2 = sum(stats$S, na.rm=TRUE)
  zscore = Wk/sqrt(sigma2)

  #--- p-values ---
  p_1side = (1-pnorm(zscore))
  p_2side = (1-pnorm(abs(zscore)))*2

  #--- output ---
  Z=list()
  Z$stat_table = stats
  Z$Wk         = Wk
  Z$Wk_var     = sigma2
  Z$Z          = zscore
  Z$chisq      = zscore^2
  Z$p_1side    = p_1side
  Z$p_2side    = p_2side

  return(Z)
}
