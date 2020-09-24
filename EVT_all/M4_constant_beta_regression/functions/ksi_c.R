
newksit_c = function(y,x2,ksiT, sigT, betaksi, thre,
                     Vksi, s, K_ksit) 
{
  tt      = length(y) 
  curksiT = ksiT[[s]]
  cursigT = sigT[[s]]
  curbetaksi  = betaksi[[s]]
  curthre     = thre[[s]]
  curVksi     = Vksi[[s]]
  newksiT     = c()
  for (i in 1:tt)
  {
    m = curbetaksi %*% x2[i,]
    if (y[i] < curthre)
    {
      newksiT[i] = rnorm(1, mean = m, sd = sqrt(1/curVksi))
    }
    
    else
    {  
      newksiT[i] = rnorm(1, mean = curksiT[i], sd = sqrt(K_ksit))
      
      r_M = log(ksipri(newksiT[i], m, curVksi) / 
                  ksipri(curksiT[i], m, curVksi)) + 
        log(dgpd(y[i], loc = curthre, scale = exp(cursigT[i]), shape = exp(newksiT[i])-1)) -
        log(dgpd(y[i], loc = curthre, scale = exp(cursigT[i]), shape = exp(curksiT[i])-1)) 
      
      if (runif(1) < exp(r_M))
      {newksiT[i] = newksiT[i]}
      else(newksiT[i] = curksiT[i]) 
    }   
  }
  
  return(newksiT)
}



ksipri = function(ksi, thetaksi, Vksi)
{
  return(dnorm(ksi, mean = thetaksi, sd = sqrt(1 / Vksi)))
}


