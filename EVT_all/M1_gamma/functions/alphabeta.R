
# ------Sampling alpha and beta------------

new_bulk = function(param,Va,Vb,Ga,data)
{
  alpha    =  param[1]
  beta     =  param[2]
  newalpha =  rtnorm(1, mean = alpha, sd = sqrt(Va), lower = 0.1)
  newbeta  =  rtnorm(1, mean = beta, sd = sqrt(Vb), lower = 0.1)
  temp     =  param
  temp[1]  =  newalpha
  temp[2]  =  newbeta
  prior_bulk = function(alpha, beta)
  {
    a = Ga[1]
    b = Ga[2]
    c = Ga[3]
    d = Ga[4]
    pri = dgamma(alpha, shape = a, rate = b) * 
      dgamma(alpha / beta, shape = c, rate = d) * 
      (alpha / beta^2)
    return(pri)
  }   
  r_M      = log(prior_bulk(newalpha,newbeta) / prior_bulk(alpha,beta)) + ll(temp, data) - ll(param, data)
  r_M      = exp(r_M)
  acc_bulk =  r_M  * dtnorm(alpha, mean = newalpha, sd = sqrt(Va) , lower = 0.1) * dtnorm(beta, mean = newbeta, sd = sqrt(Vb)) /
    dtnorm(newalpha, mean = alpha, sd = sqrt(Va) , lower = 0.1) / dtnorm(newbeta, mean = beta, sd = sqrt(Vb))
  
  if (runif(1) < acc_bulk)
  {
    alpha     =  newalpha
    beta      =  newbeta
  }
  else
  {
    alpha     =  alpha
    beta      =  beta
  }
  
  return(c(alpha,beta))
  
}
