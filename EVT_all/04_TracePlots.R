rm(list=ls())
setwd("~/nBox/EVT_Singapore/EVT_SG3.0/functions")
lapply(list.files(),source)
#Trace Plots M2,M3
load("~/nBox/EVT_Singapore/EVT_SG3.0/M2&M3_tvGPD/out/out.RData")
load("~/nBox/EVT_Singapore/EVT_SG3.0/M4_constant_beta_regression/out/out.RData")

pdf("~/nBox/EVT_Singapore/EVT_SG3.0/plot_append/Trace.pdf",width=7,height=9)
par(mfrow=c(3,2))

#M2 Trace
M2_df_thre <- unlist(MCMC_M2$thre)
M2_df_beta <- Reduce(rbind,MCMC_M2$beta)
M2_df_sig  <- unlist(MCMC_M2$sigb)

plot(M2_df_thre,x=1:length(M2_df_thre),main="M2, Threshold",type='l',ylab="value",xlab="MCMC Iteration",xaxt='n')
axis(side=1,at=c(1,2500,5000),labels=c(5000,7500,"10000"))
     
plot(M2_df_sig,x=1:length(M2_df_sig),main="M2, Sigma",type='l',ylab="value",xlab="MCMC Iteration",xaxt='n')
axis(side=1,at=c(1,2500,5000),labels=c(5000,7500,"10000"))

for (i in 1:ncol(M2_df_beta)){
  plot(M2_df_beta[,i],x=1:length(M2_df_sig),main=c("M2, Beta",i),ylab='l',xlab="value",xlab="MCMC Iteration",xaxt='n')
  axis(side=1,at=c(1,2500,5000),labels=c(5000,7500,"10000"))
}


M3_df_thre <- unlist(MCMC_M3_0.5$thre)
M3_df_beta <- Reduce(rbind,MCMC_M3_0.5$beta)
M3_df_sig  <- unlist(MCMC_M3_0.5$sigb)

plot(M3_df_thre,x=1:length(M3_df_thre),main="M3, Threshold",type='l',ylab="value",xlab="MCMC Iteration",xaxt='n')
axis(side=1,at=c(1,2500,5000),labels=c(5000,7500,"10000"))

plot(M3_df_sig,x=1:length(M3_df_sig),main="M3, Sigma",type='l',ylab="value",xlab="MCMC Iteration",xaxt='n')
axis(side=1,at=c(1,2500,5000),labels=c(5000,7500,"10000"))

for (i in 1:ncol(M2_df_beta)){
  plot(M3_df_beta[,i],x=1:length(M3_df_sig),main=c("M3, Beta",i),type='l',ylab="value",xlab="MCMC Iteration",xaxt='n')
  axis(side=1,at=c(1,2500,5000),labels=c(5000,7500,"10000"))
}




M4_df_thre <- unlist(MCMC_M4_0.5$thre)
M4_df_beta <- Reduce(rbind,MCMC_M4_0.5$beta)
M4_df_sig  <- unlist(MCMC_M4_0.5$sigb)
M4_df_betaEksi <- Reduce(rbind,MCMC_M4_0.5$betaksi)
M4_df_betaEsig <- Reduce(rbind,MCMC_M4_0.5$betasig)

plot(M4_df_thre,x=1:length(M4_df_thre),main="M4, Threshold",type='l',ylab="value",xlab="MCMC Iteration",xaxt='n')
axis(side=1,at=c(1,2500,5000),labels=c(5000,7500,"10000"))

plot(M4_df_sig,x=1:length(M4_df_sig),main="M4, Sigma",type='l',ylab="value",xlab="MCMC Iteration",xaxt='n')
axis(side=1,at=c(1,2500,5000),labels=c(5000,7500,"10000"))

for (i in 1:ncol(M4_df_beta)){
  plot(M4_df_beta[,i],x=1:length(M4_df_sig),main=c("M4, Beta",i),type='l',ylab="value",xlab="MCMC Iteration",xaxt='n')
  axis(side=1,at=c(1,2500,5000),labels=c(5000,7500,"10000"))
}

# for (i in 1:ncol(M4_df_betaEksi)){
#   plot(M4_df_betaEksi[,i],x=1:length(M4_df_sig),main=c("M4, Extreme Xi Beta",i),type='l',xlab="value",ylab="MCMC Iteration",xaxt='n')
#   axis(side=1,at=c(1,2500,5000),labels=c(5000,7500,"10000"))
# }
# 
# for (i in 1:ncol(M4_df_betaEsig)){
#   plot(M4_df_betaEsig[,i],x=1:length(M4_df_sig),main=c("M4, Extreme Sigma Beta",i),type='l',xlab="value",ylab="MCMC Iteration",xaxt='n')
#   axis(side=1,at=c(1,2500,5000),labels=c(5000,7500,"10000"))
# }

dev.off()