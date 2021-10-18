######################################
# shuffle.flat (ver.2) -- hidden
######################################
shuffle.flat <- function(data, key.var, seed=NULL, by_miss_pattern=FALSE){

  if(!is.null(seed)) set.seed(seed)

  #--- figure out the patterns ---
  tmp = data[,-1]
  n   = nrow(tmp)
  k   = ncol(tmp)
  pattern = rep(0, n)
  for (i in 1:k){
    pattern = pattern + as.numeric(!is.na(tmp[,i]))*(10^(i-1))
  }
  pattern

  unique_pattern = sort(unique(pattern))
  npatterns      = length(unique_pattern)

  tmp2 = cbind(data, pattern)
  tmp2

  #--- permutate by patterns ---
  if(by_miss_pattern==TRUE){
    D=c()
    for (i in 1:npatterns){
      idx = pattern == unique_pattern[i]
      tmp_D   = data[idx,]
      tmp_n   = nrow(tmp_D)
      key.org = tmp_D[,key.var]
      key.new = sample(key.org, size=tmp_n)
      tmp_D[,key.var] = key.new
      D = rbind(D, tmp_D)
    }
  }
  #--- permutate entire (ver.1)---
  if(by_miss_pattern==FALSE){
    D = data
    n = nrow(D)
    key.org = D[,key.var]
    key.new = sample(key.org, size=n)
    D[,key.var] = key.new
  }
  D
}

######################################
# maxcombo2 (two-sample) -- hidden
######################################
maxcombo2 <- function(data, method="permutation", nmethod=10000, seed=1){

#---
indat = data

#--- observed Z* = max(Z1,Z2,Z3,Z4) ---
  wt1 = WLRstats(indat$time, indat$status, indat$arm, weight="LR")  #"Grho0gamma0"
  wt2 = WLRstats(indat$time, indat$status, indat$arm, weight="PPW") #"Grho1gamma0"
  wt3 = WLRstats(indat$time, indat$status, indat$arm, weight="Grho0gamma1")
  wt4 = WLRstats(indat$time, indat$status, indat$arm, weight="Grho1gamma1")

  obs_z1 = abs(wt1$Z)
  obs_z2 = abs(wt2$Z)
  obs_z3 = abs(wt3$Z)
  obs_z4 = abs(wt4$Z)

  obs_z_star = max(obs_z1, obs_z2, obs_z3, obs_z4)

#--- constructing null distribution --
  if(method=="permutation"){
    if(!is.null(seed)){set.seed(seed)}
    #-- resampling method (Permutation)
    perm_z_star = NULL
    for (k in 1:nmethod){
      perm_indat = shuffle.flat(indat, "arm")

      #---　Z* = max(Z1,Z2,Z3,Z4) ---
      perm_wt1 = WLRstats(perm_indat$time, perm_indat$status, perm_indat$arm, weight="LR")  #"Grho0gamma0"
      perm_wt2 = WLRstats(perm_indat$time, perm_indat$status, perm_indat$arm, weight="PPW") #"Grho1gamma0"
      perm_wt3 = WLRstats(perm_indat$time, perm_indat$status, perm_indat$arm, weight="Grho0gamma1")
      perm_wt4 = WLRstats(perm_indat$time, perm_indat$status, perm_indat$arm, weight="Grho1gamma1")

      perm_z1 = abs(perm_wt1$Z)
      perm_z2 = abs(perm_wt2$Z)
      perm_z3 = abs(perm_wt3$Z)
      perm_z4 = abs(perm_wt4$Z)

      perm_z_star[k] = max(perm_z1, perm_z2, perm_z3, perm_z4)
     }
  }
  if(method=="analytic"){
    stop("This method will be included in the package soon.")
  }

#--- get p-value (2_sided)---
     pval = sum(perm_z_star > obs_z_star)/nmethod

#--- output ---
Z=list()
Z$p_value    = pval
Z$obs.z.star = obs_z_star

class(Z) = "maxcombo2"
return(Z)
}


###############
# Example
###############
# D=surv2test.sample.data()
# data=D

# aa = maxcombo2(data, method="permutation", nmethod=50, seed=1, test="2_side")
# aa
