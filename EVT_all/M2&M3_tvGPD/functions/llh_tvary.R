llh <- function(y,x,beta,sigb,ksi,sigma,u){   # Here, x is x1 
  
  # compute LLH for bulk
  
  indBulk <- which(y < u)
  yb      <- y[indBulk]
  xb      <- x[indBulk,]
  n       <- length(yb)
  bulkLLH <- -n*log(sigb * sqrt(2 * pi)) - 0.5*sum((yb- xb %*% beta)) / sigb^2  
  
  # compute LLH for extreme
  # no covariates x for the time being
  indExt   <- which(y >=u)
  ye       <- y[indExt]
  
  # subset to reference y and ksi/sigma better
  
  ksi1      <- exp(ksi[indExt]) - 1   
  sigma1    <- exp(sigma[indExt])
  
  ind0     <- which(ksi1 == 0)
  ind1     <- which(ksi1 != 0)
  
  temp1 <- 0
  
  #compute LLH values for ksi !=0
  for (i in ind1){
    temp1 <- temp1 + log(dgpd(ye[i], loc = u, scale = sigma1[i], shape = ksi1[i]))
    #log(1 / sigma1[i] * (1 + ksi1[i] * (ye[i] - u) / sigma1[i])^(- (1 + ksi1[i]) / ksi1[i]))
  }
  
  #compute LLH values for ksi ==0
  for (i in ind0){
    temp1 <- temp1 + log(dgpd(ye[i], loc = u, scale = sigma1[i], shape = ksi1[i]))
    #log(1 / sigma1[i] * exp((ye[i] - u) / sigma1[i]))
  }
  
  C  = sum(1 - pnorm(u, mean = xb %*% beta, sd = sigb))

  out <- bulkLLH + temp1 + C 
  
  return(out)
}

