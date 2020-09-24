library(MASS)     # for mvnorm
library(invgamma) # for invgamma

beta_post <- function(x, y, B0, P0, T0 = 1, D0 = 1, s, thre, sigb){
  
  u       <- thre[[s]]
  sig1    <- (sigb[[s]])^2
  indBulk <- which(y<u)
  yb      <- y[indBulk]
  xb      <- x[indBulk,]
  n       <- length(yb)  

  ####Beta
  SigStar  <- solve((solve(P0) + 1/sig1 * (t(xb) %*% xb)))   
  BetaStar <- SigStar  %*% (solve(P0) %*% B0 + (1/sig1) * t(xb) %*% yb)
  newbeta  <- mvrnorm(n = 1, mu = BetaStar, Sigma = SigStar)         
  
  
  ####Sigma^2   
  T1 <- T0 + n 
  D1 <- D0 + t(yb - xb %*% newbeta) %*% (yb - xb %*% newbeta)
  newsig <- rinvgamma(n = 1, shape = T1 / 2, scale = D1 / 2)  

  return(list(Sigb  = sqrt(newsig),
              Beta  = newbeta))
}



