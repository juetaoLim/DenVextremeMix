
# -------MCMC for extreme mixture model-------

MCMC_mixture = function(niter, startvalue, data, V, Ga)
{
  Va      = V[1]
  Vb      = V[2]
  Vthre   = V[3]
  Vsig    = V[4]
  Vksi    = V[5]
  chain = array(dim=c(niter,length(startvalue)))
  chain[1,] = startvalue
  for (i in 2:niter)
  {
    tempchain = chain[i-1,]
    # Sampling ksi
    tempchain[5] = newksi(tempchain, Vksi, data)
    
    # Sampling sigma
    tempchain[4] = newsigma(tempchain,Vsig, data)
    
    # Sampling thre
    tempchain[3] = newthre(tempchain, Vthre, data)
    
    # Sampling alpha and beta
    tempchain[1:2] = new_bulk(tempchain, Va, Vb, Ga, data)
    
    chain[i,] = tempchain
    print(i)
  }
  
  return(chain)
}
