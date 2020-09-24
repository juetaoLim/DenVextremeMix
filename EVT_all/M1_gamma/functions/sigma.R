newsigma = function(param,Vsig,data)
{
  thre     = param[3]
  sigma    = param[4]
  ksi      = param[5]
  M        = max(data)
  temp     = param
  sigpri   = function(ksi,sigma)
  {
    sigpri  = sigma^(-1) * (1 + ksi)^(-1) * (1 + 2*ksi)^(-0.5)
    return(sigpri)
  }
  if (ksi >= 0)
  {
    a = sigma^2 / Vsig
    b = sigma   / Vsig
    newsigma = rgamma(1, shape = a, rate = b)
    newa = newsigma^2 / Vsig
    newb = newsigma   / Vsig
    temp[4]  = newsigma
    r_M = log(sigpri(ksi,newsigma) / sigpri(ksi, sigma)) + ll(temp,data) - ll(param,data)
    r_M = exp(r_M)
    accsigma =  r_M * dgamma(sigma, shape = a, rate = b) / dgamma(newsigma, shape = newa, rate = newb)
    if (runif(1) < accsigma)
    {sigma = newsigma}
    else
    {sigma = sigma}
  }
  else
  {
    newsigma = rtnorm(1, mean = sigma, sd = sqrt(Vsig), lower = - ksi * (M - thre))
    temp[4]  = newsigma
    r_M = log(sigpri(ksi,newsigma) / sigpri(ksi, sigma)) + ll(temp,data) - ll(param,data)
    r_M = exp(r_M)
    accsigma =  r_M * pnorm((sigma + ksi * (M - thre)) / sqrt(Vsig)) / pnorm((newsigma + ksi * (M - thre)) / sqrt(Vsig))
    if (runif(1) < accsigma)
    {sigma = newsigma}
    else
    {sigma = sigma}
  }
  
  return(sigma)
}
