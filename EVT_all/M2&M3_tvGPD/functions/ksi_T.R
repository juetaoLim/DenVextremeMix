# logksi = log(kis + 1)
# Here, ksiT  == logksiT


newksit = function(y, x, ksiT, sigT, thetaksiT, thre,
                   Vksi, s,K_ksit) 
{
  tt      = length(y) 
  curksiT = ksiT[[s]]
  cursigT = sigT[[s]]
  curthetaksiT = thetaksiT[[s]]
  curthre     = thre[[s]]
  curVksi     = Vksi[[s]]
  newksiT     = c()
  for (i in 1:tt)
  {
     if (y[i] < curthre)
     {
       newksiT[i] = rnorm(1, mean = curthetaksiT[i+1],
                           sd = sqrt(1/curVksi))
     }
     
     else
     {  
       newksiT[i] = rnorm(1, mean = curksiT[i], sd = sqrt(K_ksit))
      
       r_M = log(ksipri(newksiT[i], curthetaksiT[i+1], curVksi) / 
                 ksipri(curksiT[i], curthetaksiT[i+1], curVksi)) + 
         log(dgpd(y[i], loc = curthre, scale = exp(cursigT[i]), shape = exp(newksiT[i])-1)) -
         log(dgpd(y[i], loc = curthre, scale = exp(cursigT[i]), shape = exp(curksiT[i])-1)) 
      
       r_M = exp(r_M)
       if (runif(1) < r_M)
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

