
g_Vsig = function(tt, f_sig, o_sig, sigT, thetasigT, s)
{
  sumVsig = 0 
  cursigT = sigT[[s+1]]
  curthetasigT = thetasigT[[s]]
  for (t in 1:tt)
  {
    temp = (cursigT[t] - curthetasigT[t+1]) ^ 2
    sumVsig  = sumVsig + temp
  }
  
  newVsig = rgamma(1, shape = f_sig + tt / 2, 
                      rate  = o_sig + 0.5*sumVsig)  
  
  return(newVsig)  
}


g_Wsig = function(tt, l_sig, m_sig, thetasigT, s)
{
  sumWsig      = 0
  curthetasigT = thetasigT[[s]]
  for (t in 1:tt)
  {
    temp = (curthetasigT[t+1] - curthetasigT[t]) ^2
    sumWsig = sumWsig + temp
  }
  
  newWsig = rgamma(1, shape = l_sig + tt / 2,
                      rate = m_sig + 0.5*sumWsig) 
  return(newWsig) 
  
}


g_thetasig0 = function(Wsig, thetasigT, m_sig0, C_sig0, s)
{
  curWsig = Wsig[[s+1]]
  curthetasigT = thetasigT[[s]]
  
  newthetasig0 = rnorm(1, mean = (curWsig * curthetasigT[2] + m_sig0 / C_sig0) / (curWsig + 1 / C_sig0), 
                       sd = sqrt(1 /(curWsig + 1 / C_sig0)))
  return(newthetasig0)
} 


g_thetasig =  function(tt, sigT, thetasigT, Vsig, Wsig, s, i)
{
  curVsig = Vsig[[s+1]]
  curWsig = Wsig[[s+1]]
  cursigT = sigT[[s+1]]
  curthetasigT = thetasigT[[s]]
  updatedthetasig = thetasigT[[s+1]][i]
  newthetasig = rnorm(1, mean = (curVsig * cursigT[i] + curWsig * (curthetasigT[i+2] + updatedthetasig)) / 
                             (curVsig + 2 * curWsig), 
                           sd = sqrt(1 / (curVsig + 2 * curWsig)))
  return(newthetasig)
}



g_thetasigT = function(tt, sigT, thetasigT, Vsig, Wsig, s)
{
  curVsig = Vsig[[s+1]]
  curWsig = Wsig[[s+1]]
  cursigT = sigT[[s+1]]
  curthetasigT = thetasigT[[s+1]]
  newthetasigT= rnorm(1, mean = (curVsig * cursigT[tt] + curWsig * curthetasigT[tt]) / (curVsig + curWsig) ,
                      sd = sqrt(1 / (curVsig + curWsig)))
  
  return(newthetasigT)
  
}







































