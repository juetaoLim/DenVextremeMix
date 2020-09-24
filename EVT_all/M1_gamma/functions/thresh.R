newthre = function(param,Vthre,data)
{
  thre    = param[3]
  sigma   = param[4]
  ksi     = param[5]
  M       = max(data)
  if (ksi >= 0)
  {
    a_j = min(data)    
  }
  else
  {
    a_j = M + sigma / ksi
  }
  
  newthre   = rtnorm(1, mean = thre, sd = sqrt(Vthre), lower = a_j, upper = M)
  temp      = param
  temp[3]   = newthre
  mu_thre   = 0.3 
  sig_thre  = 0.01     
  threpri = function(thre)
  {
    return(1/sqrt(2*pi*sig_thre^2) * exp(-0.5*(thre-mu_thre)^2/sig_thre^2) / pnorm(mu_thre/sig_thre))
  }
  
  r_M     = log(threpri(newthre) / threpri(thre)) + ll(temp,data) - ll(param,data)
  r_M     = exp(r_M)
  accthre = r_M  *  (pnorm((M - thre) / sqrt(Vthre)) - pnorm((a_j - thre) / sqrt(Vthre))) /
    (pnorm((M - newthre) / sqrt(Vthre)) - pnorm((a_j - newthre) / sqrt(Vthre)))
  if (runif(1) < accthre)
  {thre = newthre}
  else {thre = thre}
  return(thre)
}

