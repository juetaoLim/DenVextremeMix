
# ------Likelihood-----------------
ll = function(param, data)
{
  alpha = param[1]
  beta  = param[2]
  thre  = param[3]
  sigma = param[4]
  ksi   = param[5]
  A     = data[data < thre]
  B     = data[data >= thre]  
  ll_A  = sum(dgamma(A, shape = alpha, rate = beta, log = T))
  ll_B  = length(B) * log(1 - pgamma(thre, shape = alpha, rate = beta)) + sum(dgpd(B, loc = thre, scale = sigma, shape = ksi, log = T))
  return(ll_A + ll_B)
}
