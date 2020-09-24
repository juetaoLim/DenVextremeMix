rm(list=ls())
setwd("~/nBox/EVT_Singapore/EVT_SG3.0/functions")
lapply(list.files(),source)


###############COMPUTE model summary for M2###############
###############COMPUTE model summary for M2###############
###############COMPUTE model summary for M2###############
###############COMPUTE model summary for M2###############
###############COMPUTE model summary for M2###############
load("~/nBox/EVT_Singapore/EVT_SG3.0/M2&M3_tvGPD/out/out.RData")
ksi_m2 <- c()
sig_m2 <- c()


tt <- length(y)
burnIn <- 1000
for (i in 1:tt){
  ksi_m2[i] = exp(mean(unlist(lapply(MCMC_M2$lksiT, function(x) x[i]))[-(1:burnIn)])) - 1 
  sig_m2[i] = exp(mean(unlist(lapply(MCMC_M2$lsigT, function(x) x[i]))[-(1:burnIn)]))
}

beta_m2 <- Reduce(rbind,MCMC_M2$beta)
beta_m2 <- apply(beta_m2[-(1:burnIn),],MARGIN=2,function(x)c(mean(x),quantile(x,probs=c(0.025,0.975))))
thre <- mean(unlist(MCMC_M2$thre)[-(1:burnIn)])
r    <- sum(y > thre)/length(y)
N <- seq(52,10000,by=52)

m2Lev <- timeVarLev(thre=thre,sig=sig_m2,ksi=ksi_m2,N=N,r=r)

m2Out <- list(m2Lev,ksi_m2,sig_m2,beta_m2,thre)
names(m2Out) <-c("m2Lev",'ksi_m2','sig_m2',"beta_m2","thre")
###############COMPUTE Time Varying Returns for M3###############
###############COMPUTE Time Varying Returns for M3###############
###############COMPUTE Time Varying Returns for M3###############
###############COMPUTE Time Varying Returns for M3###############
###############COMPUTE Time Varying Returns for M3###############
ksi_m3 <- c()
sig_m3 <- c()
for (i in 1:tt){
  ksi_m3[i] = exp(mean(unlist(lapply(MCMC_M3$lksiT, function(x) x[i]))[-(1:burnIn)])) - 1 
  sig_m3[i] = exp(mean(unlist(lapply(MCMC_M3$lsigT, function(x) x[i]))[-(1:burnIn)]))
}

beta_m3 <- Reduce(rbind,MCMC_M3$beta)
beta_m3 <- apply(beta_m3[-(1:burnIn),],MARGIN=2,function(x)c(mean(x),quantile(x,probs=c(0.025,0.975))))
thre <- mean(unlist(MCMC_M3$thre)[-(1:burnIn)])
r    <- sum(y > thre)/length(y)
N <- seq(52,10000,by=52)

m3Lev <- timeVarLev(thre=thre,sig=sig_m3,ksi=ksi_m3,N=N,r=r)

m3Out <- list(m3Lev,ksi_m3,sig_m3,beta_m3,thre)
names(m3Out) <-c("m3Lev",'ksi_m3','sig_m3',"beta_m3","thre")

###############COMPUTE Time Varying Returns for M4###############
###############COMPUTE Time Varying Returns for M4###############
###############COMPUTE Time Varying Returns for M4###############
###############COMPUTE Time Varying Returns for M4###############
###############COMPUTE Time Varying Returns for M4###############
load("~/nBox/EVT_Singapore/EVT_SG3.0/M4_constant_beta_regression/out/out.RData")
ksi_m4 <- c()
sig_m4 <- c()
for (i in 1:tt){
  ksi_m4[i] = exp(mean(unlist(lapply(MCMC_M4$lksiT, function(x) x[i]))[-(1:burnIn)])) - 1 
  sig_m4[i] = exp(mean(unlist(lapply(MCMC_M4$lsigT, function(x) x[i]))[-(1:burnIn)]))
}

beta_m4 <- Reduce(rbind,MCMC_M4$beta)
beta_m4 <- apply(beta_m4[-(1:burnIn),],MARGIN=2,function(x)c(mean(x),quantile(x,probs=c(0.025,0.975))))

betaKsi_m4 <- Reduce(rbind,MCMC_M4$betaksi)
betaSig_m4 <- Reduce(rbind,MCMC_M4$betasig)
betaKsi_m4 <- apply(betaKsi_m4[-(1:burnIn),],MARGIN=2,function(x)c(mean(x),quantile(x,probs=c(0.025,0.975))))
betaSig_m4 <- apply(betaSig_m4[-(1:burnIn),],MARGIN=2,function(x)c(mean(x),quantile(x,probs=c(0.025,0.975))))


thre <- mean(unlist(MCMC_M4$thre)[-(1:burnIn)])
threCI <- quantile(unlist(MCMC_M4$thre)[-(1:burnIn)],probs=c(0.025,0.975))
r    <- sum(y > thre)/length(y)
N <- seq(52,10000,by=52)

m4Lev <- timeVarLev(thre=thre,sig=sig_m4,ksi=ksi_m4,N=N,r=r)

m4Out <- list(m4Lev,ksi_m4,sig_m4,beta_m4,betaKsi_m4,betaSig_m4,thre,threCI)
names(m4Out) <-c("m4Lev",'ksi_m4','sig_m4',"beta_m4","betaKsi_m4","betaSig_m4","thre","threCI")

###############COMPUTE Time Varying Returns for M5###############
###############COMPUTE Time Varying Returns for M5###############
###############COMPUTE Time Varying Returns for M5###############
###############COMPUTE Time Varying Returns for M5###############
###############COMPUTE Time Varying Returns for M5###############
load("~/nBox/EVT_Singapore/EVT_SG3.0/M5_tv_beta_regression/out/out.RData")
ksi_m5 <- c()
sig_m5 <- c()
betaKsi_m5 <- c()
betaSig_m5 <- c()
for (i in 1:tt){
  ksi_m5[i] = exp(mean(unlist(lapply(MCMC_M5$lksiT, function(x) x[i]))[-(1:burnIn)])) - 1 
  sig_m5[i] = exp(mean(unlist(lapply(MCMC_M5$lsigT, function(x) x[i]))[-(1:burnIn)]))
}

beta_m5 <- Reduce(rbind,MCMC_M5$beta)
beta_m5 <- apply(beta_m5[-(1:burnIn),],MARGIN=2,function(x)c(mean(x),quantile(x,probs=c(0.025,0.975))))
thre <- mean(unlist(MCMC_M5$thre)[-(1:burnIn)])
r    <- sum(y > thre)/length(y)
N <- seq(52,10000,by=52)

m5Lev <- timeVarLev(thre=thre,sig=sig_m5,ksi=ksi_m5,N=N,r=r)

m5Out <- list(m5Lev,ksi_m5,sig_m5,beta_m5,thre)
names(m5Out) <-c("m5Lev",'ksi_m5','sig_m5',"beta_m5","thre")

########################combine output and store for plot the next function#############
########################combine output and store for plot the next function#############
########################combine output and store for plot the next function#############
modOut <- list(m2Out,m3Out,m4Out,m5Out)
names(modOut) <- list("m2Out","m3Out","m4Out","m5Out")
save(modOut,file="~/nBox/EVT_Singapore/EVT_SG3.0/out_summary/out.RData")
