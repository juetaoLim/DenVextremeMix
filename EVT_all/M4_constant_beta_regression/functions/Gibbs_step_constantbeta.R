library(mvtnorm)
betaksi = list()
betasig = list()
gc_Vksi = function(tt, f_ksi, o_ksi, ksiT, betaksi, s, x)   # Here, x is x2
{
  sumVksi = 0 
  curksiT = ksiT[[s+1]]
  curbetaksi = betaksi[[s]]
  
  for (t in 1:tt)
  {
    betaksit = curbetaksi %*% x[t,]
    temp = (curksiT[t] - betaksit) ^ 2
    sumVksi  = sumVksi + temp
  }
 
  newVksi = rgamma(1, shape = f_ksi + tt / 2, 
                   rate = o_ksi + 0.5*sumVksi) 
  
  return(newVksi)  
}



gc_betaksi = function(Vksi,ksiT, s, x) # Here x is x2. Covariates for extreme value regression. 
{
  curVksi = Vksi[[s+1]]
  curksiT = ksiT[[s+1]]
  sig0    = diag(1/100, ncol(x))
  beta0   = rep(0, ncol(x))
  m   =   solve(t(x) %*% diag(curVksi, nrow(x)) %*% x + sig0) %*%
         (t(x) %*% diag(curVksi, nrow(x)) %*% curksiT + sig0 %*% beta0) 
  sig = solve(t(x) %*% diag(curVksi, nrow(x)) %*% x + sig0)
  newbeta = rmvnorm(1, mean = m, sigma = sig)
  return(newbeta)
}


gc_Vsig = function(tt, f_sig, o_sig, sigT, betasig, s, x)   # Here, x is x2
{
  sumVsig = 0 
  cursigT = sigT[[s+1]]
  curbetasig = betasig[[s]]
  
  for (t in 1:tt)
  {
    betasigt = curbetasig %*% x[t,]
    temp = (cursigT[t] - betasigt) ^ 2
    sumVsig  = sumVsig + temp
  }
  
  newVsig = rgamma(1, shape = f_sig + tt / 2, 
                   rate = o_sig + 0.5*sumVsig) 
  
  return(newVsig)  
}



gc_betasig = function(Vsig,sigT, s, x) # Here x is x2. Covariates for extreme value regression. 
{
  curVsig = Vsig[[s+1]]
  cursigT = sigT[[s+1]]
  sig0    = diag(1/100, ncol(x))
  beta0   = rep(0, ncol(x))
  m   =   solve(t(x) %*% diag(curVsig, nrow(x)) %*% x + sig0)  %*%
          (t(x) %*% diag(curVsig, nrow(x)) %*% cursigT + sig0 %*% beta0)
  sig = solve(t(x) %*% diag(curVsig, nrow(x)) %*% x + sig0)
  newbeta = rmvnorm(1, mean = m, sigma = sig)
  
  return(newbeta)
}



