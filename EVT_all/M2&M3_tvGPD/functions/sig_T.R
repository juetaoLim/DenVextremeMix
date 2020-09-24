# logsigma = log(sigma)
# Here, sigT == logsigmaT


newsigt = function(y,x,ksiT, sigT, thetasigT, thre,
                   Vsig, s, K_sigt)
{ 
  tt           = length(y)
  curksiT      = ksiT[[s+1]]
  cursigT      = sigT[[s]]
  curthetasigT = thetasigT[[s]]
  curthre = thre[[s]]
  curVsig = Vsig[[s]]
  newsigT = c()  
  for (i in 1:tt)
  {
    if (y[i] < curthre)
    {
      newsigT[i] = rnorm(1, mean = curthetasigT[i+1],
                            sd = sqrt(1 / curVsig))
    }
    
    else
    {
      NewKsiT = exp(curksiT[i]) - 1 
      if (NewKsiT > 0)
      {
        newsigT[i] = rnorm(1, mean = cursigT[i],  sd = sqrt(K_sigt))
    
        r_M = log(sigpri(newsigT[i], curthetasigT[i+1], curVsig) / 
                  sigpri(curksiT[i], curthetasigT[i+1], curVsig)) + 
          log(dgpd(y[i], loc = curthre, scale = exp(newsigT[i]), shape = exp(curksiT[i])-1)) -
          log(dgpd(y[i], loc = curthre, scale = exp(cursigT[i]), shape = exp(curksiT[i])-1)) 
        r_M = exp(r_M)
        if (runif(1) < r_M)
        {newsigT[i] = newsigT[i]}
        else
        {newsigT[i] = cursigT[i]}
      }
      
      else
      {
        newsigT[i] = rnorm(1, mean = cursigT[i], sd = sqrt(K_sigt))
        r_M = log(sigpri(newsigT[i], curthetasigT[i+1], curVsig) / 
                  sigpri(curksiT[i], curthetasigT[i+1], curVsig)) + 
          log(dgpd(y[i], loc = curthre, scale = exp(newsigT[i]), shape = exp(curksiT[i])-1)) -
          log(dgpd(y[i], loc = curthre, scale = exp(cursigT[i]), shape = exp(curksiT[i])-1)) 
        r_M = exp(r_M)
        if (runif(1) < r_M)
        {newsigT[i] =  newsigT[i]}
        else(newsigT[i]= cursigT[i]) 
      }
     } 
   }   
  
  return(newsigT)
  
}


sigpri = function(sig, thetasig, Vsig)
{
  return(dnorm(sig, mean = thetasig, sd = sqrt(1 / Vsig)))
}


