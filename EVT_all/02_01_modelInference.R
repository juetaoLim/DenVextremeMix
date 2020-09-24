rm(list=ls())
load("~/nBox/EVT_Singapore/EVT_SG3.0/out_summary/out.RData")
load("~/nBox/EVT_Singapore/data/timecount_weathermerge_31Jul.RData")
setwd("~/nBox/EVT_Singapore/EVT_SG3.0/functions")
lapply(list.files(),source)

##################COMPUTE 1 UNIT SHOCK########################
##################COMPUTE 1 UNIT SHOCK########################
##################COMPUTE 1 UNIT SHOCK########################
##################COMPUTE 1 UNIT SHOCK########################
ksi_m4 <- c()
sig_m4 <- c()

betaKsi_m4 <- modOut$m4Out$betaKsi_m4
betaSig_m4 <- modOut$m4Out$betaSig_m4
ksi <- modOut$m4Out$ksi_m4
sig <- modOut$m4Out$sig_m4

shock_ksi <- list()
shock_sig <- list()
k<- 1

#one unit shock to time varying parameter
for (i in 5:13){
  shock_ksi[[k]] <- log(ksi+1) + betaKsi_m4[1,i]*0.01
  shock_sig[[k]] <- log(sig) + betaSig_m4[1,i]*0.01
  k <- k + 1
}
#recover unlogged time varying parameter
shock_ksi <- lapply(shock_ksi,function(x)exp(x)-1)
shock_sig <- lapply(shock_sig,function(x)exp(x))


thre <- modOut$m4Out$thre
y <- merge_df_weekly[,1]
y <- (y - min(y))/ (max(y)-min(y))
y <- y[-c(1:3)]
r    <- sum(y > thre)/length(y)
N <- seq(52,10000,by=52)
#compute time varying return levels given time varying shock
shockLev <- list()

for (i in 1:length(shock_ksi)){
shockLev[[i]] <- timeVarLev(thre=thre,sig=shock_sig[[i]],ksi=shock_ksi[[i]],N=N,r=r)
}
save(shockLev,file="~/nBox/EVT_Singapore/EVT_SG3.0/out_summary/out_shockLev1.RData")


rm(list=ls())
load("~/nBox/EVT_Singapore/EVT_SG3.0/out_summary/out.RData")
load("~/nBox/EVT_Singapore/data/timecount_weathermerge_31Jul.RData")
setwd("~/nBox/EVT_Singapore/EVT_SG3.0/functions")
lapply(list.files(),source)

##################COMPUTE 1 UNIT SHOCK########################
##################COMPUTE 1 UNIT SHOCK########################
##################COMPUTE 1 UNIT SHOCK########################
##################COMPUTE 1 UNIT SHOCK########################
ksi_m4 <- c()
sig_m4 <- c()

betaKsi_m4 <- modOut$m4Out$betaKsi_m4
betaSig_m4 <- modOut$m4Out$betaSig_m4
ksi <- modOut$m4Out$ksi_m4
sig <- modOut$m4Out$sig_m4

shock_ksi <- list()
shock_sig <- list()
k<- 1

#one unit shock to time varying parameter
for (i in 5:13){
  shock_ksi[[k]] <- log(ksi+1) + betaKsi_m4[1,i]*0.05
  shock_sig[[k]] <- log(sig) + betaSig_m4[1,i]*0.05
  k <- k + 1
}
#recover unlogged time varying parameter
shock_ksi <- lapply(shock_ksi,function(x)exp(x)-1)
shock_sig <- lapply(shock_sig,function(x)exp(x))


thre <- modOut$m4Out$thre
y <- merge_df_weekly[,1]
y <- (y - min(y))/ (max(y)-min(y))
y <- y[-c(1:3)]
r    <- sum(y > thre)/length(y)
N <- seq(52,10000,by=52)
#compute time varying return levels given time varying shock
shockLev <- list()

for (i in 1:length(shock_ksi)){
  shockLev[[i]] <- timeVarLev(thre=thre,sig=shock_sig[[i]],ksi=shock_ksi[[i]],N=N,r=r)
}
save(shockLev,file="~/nBox/EVT_Singapore/EVT_SG3.0/out_summary/out_shockLev5.RData")
