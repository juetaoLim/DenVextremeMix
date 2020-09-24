thre_T = function(y, x, ksiT, sigT, thre, s, Vu, beta, sigb)
{
  a       = min(y)
  tt      = length(y)
  curksiT = ksiT[[s+1]]
  cursigT = sigT[[s+1]]
  KSIT    = exp(curksiT)  - 1
  SIGT    = exp(cursigT)
  bb      = rep(0, tt)
  curthre = thre[[s]]
  curbeta = beta[[s+1]]
  cursigb = sigb[[s+1]]
  
  for (i in 1:tt)
  {
    if (KSIT[i] < 0 & y[i] > curthre)
    {bb[i] = y[i] + SIGT[i] / (KSIT[i] * (1 + KSIT[i]))}
  }
  
  b = max(bb)
  if (a > b)
  {uL = a}
  else{uL = b}
  newthre = rtnorm(1, mean = curthre, sd = sqrt(Vu), lower = uL)
  r_M = log(threpri(newthre) / 
            threpri(curthre)) + 
        llh(y, x, curbeta, cursigb, curksiT, cursigT, newthre) -
        llh(y, x, curbeta, cursigb, curksiT, cursigT, curthre)
  r_M = exp(r_M)
  r   = r_M * pnorm((curthre - uL) / sqrt(Vu)) / 
              pnorm((newthre - uL) / sqrt(Vu))

  if (runif(1) < r)
  {newthre = newthre}  
  else{newthre = curthre}
  
  return(newthre)
}



mu_thre   = 0.6   
sig_thre  = 0.1    
threpri = function(thre)
{
  return(1/sqrt(2*pi*sig_thre^2) * exp(-0.5*(thre-mu_thre)^2/sig_thre^2) / pnorm(mu_thre/sig_thre))
}


