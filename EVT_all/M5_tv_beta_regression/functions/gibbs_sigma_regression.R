
gr_Vsig = function(tt, f_sig, o_sig, sigT, betasig, s, x)  
{
  sumVsig = 0 
  cursigT = sigT[[s+1]]
  curbetasig = betasig[[s]]
  
  for (t in 1:tt)
  {
    betasigt = as.numeric(t(curbetasig[t+1,]) %*% x[t,])
    temp = (cursigT[t] - betasigt) ^ 2
    sumVsig  = sumVsig + temp
  }
  
  newVsig = rgamma(1, shape = f_sig + tt / 2, 
                      rate = o_sig + 0.5*sumVsig) 
  
  return(newVsig)  
}



gr_Wsig_k = function(tt, l_sig, m_sig, betasig, s, k) # k = 0,1,2,,,p
{
  sumWsig    = 0
  curbetasig = betasig[[s]]
  
  for (t in 1:tt)
  {
    temp = (curbetasig[t+1, k+1] - curbetasig[t, k+1]) ^ 2
    sumWsig = sumWsig + temp
  }
  
  newWsig = rgamma(1, shape = l_sig + tt / 2,
                      rate  = m_sig + 0.5*sumWsig) 
  
  return(newWsig) 
  
}


gr_betasig0_k = function(Wsig, betasig, m_sig0, C_sig0, s, k)   # k = 0,1,2,,,p
{
  curWsigk   = Wsig[[s+1]][k+1]  # Wsig = [Wsig0, Wsig1,...Wsigp]
  curbetasig = betasig[[s]]
  newbetasig0_k = rnorm(1, mean = (curWsigk* curbetasig[2, k+1] + m_sig0 / C_sig0) / (curWsigk + 1 / C_sig0), 
                           sd = sqrt(1 /(curWsigk + 1 / C_sig0)))
  return(newbetasig0_k)
} 


gr_betasig_k = function(tt, sigT, betasig, Vsig, Wsig, s, i, k, x)
{
  curVsig  = Vsig[[s+1]]
  curWsigk = Wsig[[s+1]][k+1]
  cursigT  = sigT[[s+1]]
  
  S_tk = cursigT[i] - as.numeric(t(betasig[[s]][i+1,]) %*% x[i,]) + 
                      betasig[[s]][i+1, k+1]
  
  newbetasig_k = rnorm(1, mean = (curVsig * S_tk * x[i, k+1] + curWsigk * (betasig[[s]][i+2, k+1] + betasig[[s+1]][i, k+1])) / 
                         (curVsig * x[i,k+1]^2 + 2 * curWsigk), 
                          sd   = sqrt(1 / (curVsig * x[i,k+1]^2 + 2 * curWsigk)))
  
  return(newbetasig_k)
}


gr_betasigT_k = function(tt, sigT, betasig, Vsig, Wsig, s, k, x)
{
  curVsig  = Vsig[[s+1]]
  curWsigk = Wsig[[s+1]][k+1]
  cursigT  = sigT[[s+1]]
  
  S_tk     = cursigT[tt] - as.numeric(t(betasig[[s]][tt+1,]) %*% x[tt,]) + 
                           betasig[[s]][tt+1, k+1] 
  
  newbetasigT_k = rnorm(1, mean = (curVsig * S_tk * x[tt, k+1] + curWsigk * betasig[[s+1]][tt, k+1]) / 
                          (curVsig * x[tt,k+1]^2  + curWsigk) ,
                           sd = sqrt(1 / (curVsig * x[tt,k+1]^2  + curWsigk)))
  return(newbetasigT_k)
  
}

