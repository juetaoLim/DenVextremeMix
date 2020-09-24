
gr_Vksi = function(tt, f_ksi, o_ksi, ksiT, betaksi, s, x)   # Here, x is x2
{
  sumVksi = 0 
  curksiT = ksiT[[s+1]]
  curbetaksi = betaksi[[s]]
  
  for (t in 1:tt)
  {
     betaksit = as.numeric(t(curbetaksi[t+1,]) %*% x[t,])
     temp = (curksiT[t] - betaksit) ^ 2
     sumVksi  = sumVksi + temp
  }
  
  newVksi = rgamma(1, shape = f_ksi + tt / 2, 
                   rate = o_ksi + 0.5*sumVksi) 

  return(newVksi)  
}



gr_Wksi_k = function(tt, l_ksi, m_ksi, betaksi, s, k)  # k = 0,1,2,,,p
{
  sumWksi    = 0
  curbetaksi = betaksi[[s]]
  
  for (t in 1:tt)
  {
    temp = (curbetaksi[t+1, k+1] - curbetaksi[t, k+1]) ^ 2
    sumWksi = sumWksi + temp
  }
  
  newWksi = rgamma(1, shape = l_ksi + tt / 2,
                      rate  = m_ksi + 0.5*sumWksi) 

  return(newWksi) 
  
}  


gr_betaksi0_k = function(Wksi, betaksi, m_ksi0, C_ksi0, s, k)  # k = 0,1,2...p
{
  curWksik = Wksi[[s+1]][k+1]
  curbetaksi = betaksi[[s]]
  newbetaksi0_k = rnorm(1, mean = (curWksik* curbetaksi[2, k+1] + m_ksi0 / C_ksi0) / (curWksik + 1 / C_ksi0), 
                       sd = sqrt(1 /(curWksik + 1 / C_ksi0)))
  return(newbetaksi0_k)
} 


gr_betaksi_k =  function(tt, ksiT, betaksi, Vksi, Wksi, s, i, k, x) # k = 0,1,2...p  i <---> t
{
  curVksi  = Vksi[[s+1]]
  curWksik = Wksi[[s+1]][k+1]
  curksiT  = ksiT[[s+1]]
  
  S_tk = curksiT[i] - as.numeric(t(betaksi[[s]][i+1,]) %*% x[i,]) + 
                      betaksi[[s]][i+1, k+1]
  
  newbetaksi_k = rnorm(1, mean = (curVksi * S_tk * x[i, k+1] + curWksik * (betaksi[[s]][i+2, k+1] + betaksi[[s+1]][i, k+1])) / 
                                 (curVksi * x[i, k+1]^2 + 2 * curWksik), 
                         sd   = sqrt(1 / (curVksi * x[i, k+1]^2 + 2 * curWksik)))


  return(newbetaksi_k)
}


gr_betaksiT_k = function(tt, ksiT, betaksi, Vksi, Wksi, s, k, x)
{
  curVksi  = Vksi[[s+1]]
  curWksik = Wksi[[s+1]][k+1]
  curksiT  = ksiT[[s+1]]

  S_tk = curksiT[tt] - as.numeric(t(betaksi[[s]][tt+1,]) %*% x[tt,]) + 
                      betaksi[[s]][tt+1, k+1] 
  
  newbetaksiT_k = rnorm(1, mean = (curVksi * S_tk * x[tt, k+1] + curWksik * betaksi[[s+1]][tt, k+1]) / 
                          (curVksi * x[tt,k+1]^2  + curWksik) ,
                           sd   = sqrt(1 / (curVksi * x[tt,k+1]^2  + curWksik)))
 
  return(newbetaksiT_k)
  
}


