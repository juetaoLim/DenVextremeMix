rm(list=ls())
load("~/nBox/EVT_Singapore/EVT_SG3.0/out_summary/out.RData")
load("~/nBox/EVT_Singapore/data/timecount_weathermerge_31Jul.RData")
y <- merge_df_weekly[,1]
y <- (y - min(y))/(max(y)-min(y))+0.1
#Plots on Model 4

df <- modOut$m4Out

sig <- df$sig_m4
ksi <- df$ksi_m4
thre <- df$thre
threCI <- df$threCI

indExt <- which(y >=thre)
indBulk <- which(y<thre)
cutoff <- min(y[indExt])
indCut <- which(y==cutoff)
cutoff_unscale <- unique(merge_df_weekly[indCut,1])

cutoff975 <- which(y>=threCI[1])
cutoff275 <- which(y>=threCI[2])
cutoff975 <- min(y[cutoff975])
cutoff275 <- min(y[cutoff275])
cutoff975 <- which(y==cutoff975)
cutoff275 <- which(y==cutoff275)
cutoff975 <- unique(merge_df_weekly[cutoff975,1])
cutoff275 <- unique(merge_df_weekly[cutoff275,1])

pdf("~/nBox/EVT_Singapore/EVT_SG3.0/plot_main/plot1.pdf",width=7,height=9)
#First plot
#First plot
#First plot
#First plot
#First plot
par(las=1,mfrow=c(3,1),cex.axis=1.3,cex.lab=1.5,oma=c(0,2,0,0),mar=c(3.1 ,4.1 ,2.1 ,2.1))
plot(y=merge_df_weekly[,1],
     x=1:length(merge_df_weekly[,1]),
     type="l",
     xaxt='n',
     ylab="Counts",
     xlab="",
     col="white")
polygon(x=c(-100,length(merge_df_weekly[,1])+100,length(merge_df_weekly[,1])+100,-100,-100),
        y=c(cutoff_unscale,cutoff_unscale,99999,99999,cutoff_unscale),col="lightblue",border="white")
lines(y=merge_df_weekly[,1],
     x=1:length(merge_df_weekly[,1]))
legend(x="topleft",legend=c("Extreme","Bulk"),bty='n',col=c("black","white"),pch=c(0,15),cex=1.4)
abline(h=cutoff275,lty=2)
abline(h=cutoff975,lty=2)

indTime <- seq(1,length(merge_df_weekly[,1]),length.out=19)
axis(side=1,at=indTime,labels=NA)
axis(side=1,at=indTime[-length(indTime)]+26,labels=c("00","01","02","03","04","05","06","07","08","09",10:17),tick=F)
box()
#Second plot
#Second plot
#Second plot
#Second plot
#Second plot
plot(y=sig,
     x=1:length(sig),
     type="l",
     xaxt='n',
     ylab=expression(sigma),
     xlab="",
     col="white")
abline(v=indExt,col="lightblue")
lines(y=sig,
      x=1:length(sig))


indTime <- seq(1,length(merge_df_weekly[,1]),length.out=19)
axis(side=1,at=indTime,labels=NA)
axis(side=1,at=indTime[-length(indTime)]+26,labels=c("00","01","02","03","04","05","06","07","08","09",10:17),tick=F)
legend(x="topleft",legend=c("Extreme","Bulk"),bty='n',col=c("lightblue","black"),pch=c(15,0),cex=1.4)
box()
#third plot
#third plot
#third plot
#third plot
#third plot
plot(y=ksi,
     x=1:length(ksi),
     type="l",
     xaxt='n',
     ylab=expression(xi),
     xlab="",
     col="white")
abline(v=indExt,col="lightblue")
lines(y=ksi,
      x=1:length(ksi))


indTime <- seq(1,length(merge_df_weekly[,1]),length.out=19)
axis(side=1,at=indTime,labels=NA)
axis(side=1,at=indTime[-length(indTime)]+26,labels=c("00","01","02","03","04","05","06","07","08","09",10:17),tick=F)
legend(x="topleft",legend=c("Extreme","Bulk"),bty='n',col=c("lightblue","black"),pch=c(15,0),cex=1.4)
box()

dev.off()     

######################return level inference######################
######################return level inference######################
######################return level inference######################
######################return level inference######################
pdf("~/nBox/EVT_Singapore/EVT_SG3.0/plot_main/plot2.pdf",width=10,height=4.5)
par(las=1,mfrow=c(1,3),cex.axis=1.3,cex.lab=1.5,oma=c(4,4,0,0),mar=c(3.1 ,4.1 ,2.1 ,2.1),pty='s')

#############no coefficients
#############no coefficients
#############no coefficients
#############no coefficients
m3Out <- modOut$m3Out
m4Out <- modOut$m4Out
m5Out <- modOut$m5Out

rEv <- m3Out$m3Lev
indExt <- which(y >=m3Out$thre)
rEv <- lapply(rEv,function(x)x[indExt])
rEvMeans <- unlist(lapply(rEv,mean))
rEvAll <- Reduce(rbind,rEv)

plot(y=c(min(rEvAll),
         max(rEvMeans)+0.2),
     x=c(0,length(rEvMeans)),
     type="l",
     lwd=3,
     xlab="",
     ylab="",
     xaxt="n",
     yaxt='n',
     col="white",
     main="Time Varying Extreme")
axis(at=c(0,10,50,100,150,190),label=c(1,10,50,100,150,190),side=1)
for (i in which(colSums(rEvAll) < 350)){
  lines(rEvAll[,i],col="grey")
}
lines(rEvMeans,col="black",lwd=2)
abline(v=0,lty=2)
abline(v=1,lty=2)
axis(side=2,at=round(seq(0,range(rEvMeans)[2],length.out=6),2))
box()
#############constant beta
#############constant beta
#############constant beta
#############constant beta
rEv <- m4Out$m4Lev
indExt <- which(y >=m4Out$thre)
rEv <- lapply(rEv,function(x)x[indExt])
rEvMeans <- unlist(lapply(rEv,mean))
rEvAll <- Reduce(rbind,rEv)

plot(y=c(min(rEvAll),
         max(rEvMeans)+0.2),
     x=c(0,length(rEvMeans)),
     type="l",
     lwd=3,
     xlab="",
     ylab="",
     xaxt="n",
     yaxt='n',
     col="white",
     main="Constant Beta")
axis(at=c(0,10,50,100,150,190),label=c(1,10,50,100,150,190),side=1)
for (i in which(colSums(rEvAll) < 350)){
  lines(rEvAll[,i],col="grey")
}
lines(rEvMeans,col="black",lwd=2)
abline(v=0,lty=2)
abline(v=1,lty=2)
axis(side=2,at=round(seq(0,range(rEvMeans)[2],length.out=6),2))
box()
#############time varying beta
#############time varying beta
#############time varying beta
#############time varying beta

rEv <- m5Out$m5Lev
indExt <- which(y >=m5Out$thre)
rEv <- lapply(rEv,function(x)x[indExt])
rEvMeans <- unlist(lapply(rEv,mean))
rEvAll <- Reduce(rbind,rEv)

plot(y=c(min(rEvAll),
         max(rEvMeans)+0.2),
     x=c(0,length(rEvMeans)),
     type="l",
     lwd=3,
     xlab="",
     ylab="",
     xaxt="n",
     yaxt='n',
     col="white",
     main="Time Varying Beta")
axis(at=c(0,10,50,100,150,190),label=c(1,10,50,100,150,190),side=1)
for (i in which(colSums(rEvAll) < 350)){
  lines(rEvAll[,i],col="grey")
}
lines(rEvMeans,col="black",lwd=2)
abline(v=0,lty=2)
abline(v=1,lty=2)
axis(side=2,at=round(seq(0,range(rEvMeans)[2],length.out=6),2))
mtext("Return Period (Years)",side=1,cex=2,out=T,padj=-1.5)
par(las=0)
mtext("Scaled Return Levels",side=2,cex=2,out=T)
box()
dev.off()
######################Coefficient Plot######################
######################Coefficient Plot######################
######################Coefficient Plot######################
######################Coefficient Plot######################
######################Coefficient Plot######################


m4betaKsi <- modOut$m4Out$betaKsi_m4[,-1]
m4betaSig <- modOut$m4Out$betaSig_m4[,-1]
m4betaBulk <- modOut$m4Out$beta_m4[,-c(1:4)]

cols <- c('#990000','#FF0000','#FF9999',
          '#990000','#FF0000','#FF9999',
          '#990000','#FF0000','#FF9999',
          '#990000','#FF0000','#FF9999')
pdf("~/nBox/EVT_Singapore/EVT_SG3.0/plot_main/plot3.pdf",width=12,height=10)
par(mfrow=c(1,3),cex.axis=1.5,cex.lab=2,oma=c(4,4,0,0.2),mar=c(3.1 ,1 ,2.1 ,0.1),cex.main=1.5)

####################ksi PLOT
####################ksi PLOT
####################ksi PLOT
####################ksi PLOT



plot(y=c(1,ncol(m4betaKsi)),
     x=c(min(m4betaKsi),
         max(m4betaKsi)),
     yaxt='n',
     ylab="",xlab="",col="white",main=expression(beta[xi]))

polygon(x=c(min(m4betaKsi)-10,min(m4betaKsi)-10,max(m4betaKsi)+10,max(m4betaKsi)+10,min(m4betaKsi)-10),
        y=c(0,3.5,3.5,0,0),
        border='white',
        col='lightblue')

polygon(x=c(min(m4betaKsi)-10,min(m4betaKsi)-10,max(m4betaKsi)+10,max(m4betaKsi)+10,min(m4betaKsi)-10),
        y=c(6.5,9.5,9.5,6.5,6.5),
        border='white',
        col='lightblue')

points(m4betaKsi[1,],
       y=c(1:ncol(m4betaKsi)),col=cols,pch=16,cex=2)
for (i in 1:ncol(m4betaKsi)){
  lines(x=c(m4betaKsi[2,i],m4betaKsi[3,i]),y=c(i,i),col=cols[i])
}
legend(x='bottomright',legend=c('Lag 1','Lag 2','Lag 3'),col=cols[3:1],pch=16,bty='n',cex=2)
abline(v=0,lty=2)
axis(side=2,
     at=seq(2,ncol(m4betaKsi),by=3),
     tick=F,
     labels=c('Precipitation','Temperature','Absolute Humidity','Relative Humidity'),
     cex.axis=2)
box()


####################SIG PLOT
####################SIG PLOT
####################SIG PLOT
####################SIG PLOT
plot(y=c(1,ncol(m4betaSig)),
     x=c(min(m4betaSig),
         max(m4betaSig)),
     yaxt='n',
     ylab="",xlab="",col="white",main=expression(beta[sigma]))

polygon(x=c(min(m4betaSig)-10,min(m4betaSig)-10,max(m4betaSig)+10,max(m4betaSig)+10,min(m4betaSig)-10),
        y=c(0,3.5,3.5,0,0),
        border='white',
        col='lightblue')

polygon(x=c(min(m4betaSig)-10,min(m4betaSig)-10,max(m4betaSig)+10,max(m4betaSig)+10,min(m4betaSig)-10),
        y=c(6.5,9.5,9.5,6.5,6.5),
        border='white',
        col='lightblue')


points(m4betaSig[1,],
       y=c(1:ncol(m4betaSig)),col=cols,pch=16,cex=2)
for (i in 1:ncol(m4betaSig)){
  lines(x=c(m4betaSig[2,i],m4betaSig[3,i]),y=c(i,i),col=cols[i])
}
legend(x='bottomright',legend=c('Lag 1','Lag 2','Lag 3'),col=cols[3:1],pch=16,bty='n',cex=2)
abline(v=0,lty=2)
box()
mtext(text="Coefficient Value",side=1,cex=2.5,outer=T,padj=0.5)

####################beta bulk PLot
####################beta bulk PLot
####################beta bulk PLot
####################beta bulk PLot
####################beta bulk PLot
bulk <- 'bulk'
plot(y=c(1,ncol(m4betaBulk)),
     x=c(min(m4betaBulk),
         max(m4betaBulk)),
     yaxt='n',
     ylab="",xlab="",col="white",main=expression(beta[bulk]))

polygon(x=c(min(m4betaBulk)-10,min(m4betaBulk)-10,max(m4betaBulk)+10,max(m4betaBulk)+10,min(m4betaBulk)-10),
        y=c(0,3.5,3.5,0,0),
        border='white',
        col='lightblue')

polygon(x=c(min(m4betaBulk)-10,min(m4betaBulk)-10,max(m4betaBulk)+10,max(m4betaBulk)+10,min(m4betaBulk)-10),
        y=c(6.5,9.5,9.5,6.5,6.5),
        border='white',
        col='lightblue')


points(m4betaBulk[1,],
       y=c(1:ncol(m4betaBulk)),col=cols,pch=16,cex=2)
for (i in 1:ncol(m4betaBulk)){
  lines(x=c(m4betaBulk[2,i],m4betaBulk[3,i]),y=c(i,i),col=cols[i])
}
legend(x='bottomright',legend=c('Lag 1','Lag 2','Lag 3'),col=cols[3:1],pch=16,bty='n',cex=2)
abline(v=0,lty=2)
box()
mtext(text="Coefficient Value",side=1,cex=2.5,outer=T,padj=0.5)


dev.off()


#####################return level shocks########################### 1 unit
#####################return level shocks########################### 1 unit
#####################return level shocks########################### 1 unit
#####################return level shocks########################### 1 unit

#return level shock plot
load("~/nBox/EVT_Singapore/EVT_SG3.0/out_summary/out_shockLev1.RData")

#canonical (no shock)
rEv <- m4Out$m4Lev
indExt <- which(y >=m4Out$thre)
rEv <- lapply(rEv,function(x)x[indExt])
rEvMeans <- unlist(lapply(rEv,mean))
rEvAll <- Reduce(rbind,rEv)
#canonical (with shock)
rEvShockMeans <- list()
rEvShockAll   <- list()
for (i in 1:length(shockLev)){
  temp <- shockLev[[i]]
  temp <- lapply(temp,function(x)x[indExt])
  tempMeans <- unlist(lapply(temp,mean))
  tempAll <- Reduce(rbind,temp)
  rEvShockMeans[[i]] <- tempMeans
  rEvShockAll[[i]]   <- tempAll
}

main <- c('Temperature Lag 1 Shock','Temperature Lag 2 Shock','Temperature Lag 3 Shock',
          'Absolute Humidity Lag 1 Shock','Absolute Humidity Lag 2 Shock','Absolute Humidity Lag 3 Shock',
          'Relative Humidity Lag 1 Shock','Relative Humidity Lag 2 Shock','Relative Humidity Lag 3 Shock')
pdf("~/nBox/EVT_Singapore/EVT_SG3.0/plot_main/plot4.pdf",width=10,height=9)
par(mfrow=c(3,3),cex.axis=1.5,cex.lab=2,oma=c(4,8,0,0.2),mar=c(3.1 ,1 ,2.1 ,2.1),cex.main=1.5)
for (i in 1:length(rEvShockAll)){
temp_rEvShockMeans <- rEvShockMeans[[i]]
temp_rEvShockAll   <- rEvShockAll[[i]]
plot(y=c(min(cbind(rEvAll,temp_rEvShockAll)),
         max(c(rEvMeans,temp_rEvShockMeans))+0.2),
     x=c(0,length(rEvMeans)),
     type="l",
     lwd=3,
     xlab="",
     ylab="",
     xaxt="n",
     yaxt='n',
     col="white",
     main=main[i])
axis(at=c(0,10,50,100,150,190),label=c(1,10,50,100,150,190),side=1)
for (i in which(colSums(rEvAll) < 350)){
  lines(rEvAll[,i],col="grey")
}

for (i in which(colSums(temp_rEvShockAll) < 350)){
  lines(temp_rEvShockAll[,i],col="lightblue")
}
lines(rEvMeans,col="black",lwd=2)
lines(temp_rEvShockMeans,col="blue",lwd=2)
abline(v=0,lty=2)
abline(v=1,lty=2)
axis(side=2,at=round(seq(0,range(rEvShockMeans,rEvMeans)[2],length.out=6),2))
box()
}
legend(cex=2,x="bottomright",col=c("black","blue"),bty='n',legend=c("baseline","1 unit shock"),pch=15)
mtext("Return Period", side=1,cex=2,outer=T,padj=1)
mtext("Scaled Return Level",side=2,cex=2,outer=T,padj=-2)
dev.off()


#####################return level shocks########################### 5 unit
#####################return level shocks########################### 5 unit
#####################return level shocks########################### 5 unit
#####################return level shocks########################### 5 unit

#return level shock plot
load("~/nBox/EVT_Singapore/EVT_SG3.0/out_summary/out_shockLev5.RData")

#canonical (no shock)
rEv <- m4Out$m4Lev
indExt <- which(y >=m4Out$thre)
rEv <- lapply(rEv,function(x)x[indExt])
rEvMeans <- unlist(lapply(rEv,mean))
rEvAll <- Reduce(rbind,rEv)
#canonical (with shock)
rEvShockMeans <- list()
rEvShockAll   <- list()
for (i in 1:length(shockLev)){
  temp <- shockLev[[i]]
  temp <- lapply(temp,function(x)x[indExt])
  tempMeans <- unlist(lapply(temp,mean))
  tempAll <- Reduce(rbind,temp)
  rEvShockMeans[[i]] <- tempMeans
  rEvShockAll[[i]]   <- tempAll
}

main <- c('Temperature Lag 1 Shock','Temperature Lag 2 Shock','Temperature Lag 3 Shock',
          'Absolute Humidity Lag 1 Shock','Absolute Humidity Lag 2 Shock','Absolute Humidity Lag 3 Shock',
          'Relative Humidity Lag 1 Shock','Relative Humidity Lag 2 Shock','Relative Humidity Lag 3 Shock')
pdf("~/nBox/EVT_Singapore/EVT_SG3.0/plot_main/plot5.pdf",width=10,height=9)
par(mfrow=c(3,3),cex.axis=1.5,cex.lab=2,oma=c(4,8,0,0.2),mar=c(3.1 ,1 ,2.1 ,2.1),cex.main=1.5)
for (i in 1:length(rEvShockAll)){
  temp_rEvShockMeans <- rEvShockMeans[[i]]
  temp_rEvShockAll   <- rEvShockAll[[i]]
  plot(y=c(min(cbind(rEvAll,temp_rEvShockAll)),
           max(c(rEvMeans,temp_rEvShockMeans))+0.2),
       x=c(0,length(rEvMeans)),
       type="l",
       lwd=3,
       xlab="",
       ylab="",
       xaxt="n",
       yaxt='n',
       col="white",
       main=main[i])
  axis(at=c(0,10,50,100,150,190),label=c(1,10,50,100,150,190),side=1)
  for (i in which(colSums(rEvAll) < 350)){
    lines(rEvAll[,i],col="grey")
  }
  
  for (i in which(colSums(temp_rEvShockAll) < 350)){
    lines(temp_rEvShockAll[,i],col="pink")
  }
  lines(rEvMeans,col="black",lwd=2)
  lines(temp_rEvShockMeans,col="red",lwd=2)
  abline(v=0,lty=2)
  abline(v=1,lty=2)
  axis(side=2,at=round(seq(0,range(rEvShockMeans,rEvMeans)[2],length.out=6),2))
  box()
}
legend(cex=2,x="bottomright",col=c("black","red"),bty='n',legend=c("baseline","5 unit shock"),pch=15)
mtext("Return Period", side=1,cex=2,outer=T,padj=1)
mtext("Scaled Return Level",side=2,cex=2,outer=T,padj=-2)
dev.off()