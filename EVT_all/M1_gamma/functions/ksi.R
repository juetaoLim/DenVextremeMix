# ------Sampling ksi-----

newksi = function(param,Vksi,data)
{
  thre  = param[3]
  sigma = param[4]
  ksi   = param[5]
  M     = max(data)                 
  newksi= rtnorm(1, mean = ksi, sd = sqrt(Vksi), lower = - sigma / (M - thre))
  temp  = param
  temp[5] = newksi
  ksipri   = function(ksi,sigma)
  {
    ksipri = sigma^(-1) * (1 + ksi)^(-1) * (1 + 2*ksi)^(-0.5)
    return(ksipri)
  }
  r_M = log(ksipri(newksi,sigma) / ksipri(ksi,sigma)) + ll(temp,data) - ll(param,data)
  r_M = exp(r_M)  
  accksi   = r_M * pnorm((ksi + sigma / (M - thre)) / sqrt(Vksi)) / pnorm((newksi + sigma / (M - thre)) / sqrt(Vksi))
  if(runif(1) < accksi)
  {
    ksi  = newksi
  }
  else
  {ksi = ksi}
  return(ksi)
}
