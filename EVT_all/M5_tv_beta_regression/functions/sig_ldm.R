
newsigt_l = function(y,x2,ksiT, sigT, betasig, thre,
                     Vsig, s, K_sigt)
{ 
  tt           = length(y)
  curksiT      = ksiT[[s+1]]
  cursigT      = sigT[[s]]
  curbetasig   = betasig[[s]]
  curthre = thre[[s]]
  curVsig = Vsig[[s]]
  newsigT = c()  

  for (i in 1:tt)
  {
    m = as.numeric(t(curbetasig[i+1,]) %*% x2[i,])
    if (y[i] < curthre)
    {
      newsigT[i] = rnorm(1, mean = m, sd = sqrt(1 / curVsig))
    }
    
    else
    {
      NewKsiT = exp(curksiT[i]) - 1 
      if (NewKsiT > 0)
      {
        newsigT[i] = rnorm(1, mean = cursigT[i],  sd = sqrt(K_sigt))
        r_M = log(sigpri(newsigT[i], m, curVsig) / 
                  sigpri(cursigT[i], m, curVsig)) + 
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
        temp = cursigT
        temp[i] = newsigT[i]
        r_M = log(sigpri(newsigT[i], m, curVsig) / 
                  sigpri(cursigT[i], m, curVsig)) + 
          log(dgpd(y[i], loc = curthre, scale = exp(newsigT[i]), shape = exp(curksiT[i])-1)) -
          log(dgpd(y[i], loc = curthre, scale = exp(cursigT[i]), shape = exp(curksiT[i])-1)) 
        
        if (runif(1) < exp(r_M))
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


