
g_Vksi = function(tt, f_ksi, o_ksi, ksiT, thetaksiT, s)
{
  sumVksi = 0 
  curksiT = ksiT[[s+1]]
  curthetaksiT = thetaksiT[[s]]
  for (t in 1:tt)
  {
      temp = (curksiT[t] - curthetaksiT[t+1]) ^ 2
      sumVksi  = sumVksi + temp
  }

  newVksi = rgamma(1, shape = f_ksi + tt / 2, 
                      rate = o_ksi + 0.5*sumVksi) 
  return(newVksi)  
}

g_Wksi = function(tt, l_ksi, m_ksi, thetaksiT, s)
{
  sumWksi = 0
  curthetaksiT = thetaksiT[[s]]
  for (t in 1:tt)
  {
    temp = (curthetaksiT[t+1] - curthetaksiT[t]) ^ 2
    sumWksi = sumWksi + temp
  }
  
  newWksi = rgamma(1, shape = l_ksi + tt / 2,
                      rate  = m_ksi + 0.5*sumWksi) 
  
  return(newWksi) 
  
}

  
g_thetaksi0 = function(Wksi, thetaksiT, m_ksi0, C_ksi0, s)
{
  curWksi = Wksi[[s+1]]
  curthetaksiT = thetaksiT[[s]]
  newthetaksi0 = rnorm(1, mean = (curWksi* curthetaksiT[2] + m_ksi0 / C_ksi0) / (curWksi + 1 / C_ksi0), 
                       sd = sqrt(1 /(curWksi + 1 / C_ksi0)))
  return(newthetaksi0)
} 


g_thetaksi =  function(tt, ksiT, thetaksiT, Vksi, Wksi, s, i)
{
  curVksi = Vksi[[s+1]]
  curWksi = Wksi[[s+1]]
  curksiT = ksiT[[s+1]]
  curthetaksiT = thetaksiT[[s]]
  updatedthetaksi   = thetaksiT[[s+1]][i]
  newthetaksi = rnorm(1, mean = (curVksi * curksiT[i] + curWksi * (curthetaksiT[i+2] + updatedthetaksi)) / (curVksi + 2 * curWksi), 
                          sd = sqrt(1 / (curVksi + 2 * curWksi)))
  
  return(newthetaksi)
}


g_thetaksiT = function(tt, ksiT, thetaksiT, Vksi, Wksi, s)
{
  curVksi = Vksi[[s+1]]
  curWksi = Wksi[[s+1]]
  curksiT = ksiT[[s+1]]
  curthetaksiT = thetaksiT[[s+1]]
  newthetaksiT= rnorm(1, mean = (curVksi * curksiT[tt] + curWksi * curthetaksiT[tt]) / (curVksi + curWksi) ,
                         sd = sqrt(1 / (curVksi + curWksi)))
  return(newthetaksiT)

}

   
  

  
  
  