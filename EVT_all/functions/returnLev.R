returnLev <- function(u,sig,ksi,n,r){
  if (ksi!=0){
    out <- u + (sig/ksi) * ((n*r)^ksi - 1)}  
  if (ksi==0){
    out <- u + sig*log(n*r)}
  return(out)
}


# u threshold parameter (GPD to estimate)
# sig sigma parameter (GPD to estimate)
# ksi ksi parameter (GPD to estimate)
# n return period (to specify)
# r number of exceedances divided by the total length of the series (length(y>u) / tt) 

convert = function(x)
{
  return((x - 0.1) * (max(time.week) - min(time.week)) + min(time.week))
}

timeVarLev <- function(thre=thre,sig,ksi,N=N,r=r){
  storeLev <- list()
  k <- 1
  for (n in N){
    temp <- c()
    for (i in 1:length(y)){
      temp[i] <-  returnLev(u=thre,sig=sig[i],ksi=ksi[i],n=n,r=r)
    }
    storeLev[[k]] <- temp 
    k <- k + 1
  }
  
  return(storeLev)
}
