# Note that x1,x2 have already include the 1 column for intercept 
rm(list=ls())
require(msm)       # for truncated normal distribution
require(evd)       # for Generalized Pareto Distribution 

# Note that x1,x2 have already include the 1 column for intercept 

# Data 
load("~/nBox/EVT_Singapore/data/timecount_weathermerge_31Jul.RData")
setwd("~/nBox/EVT_Singapore/EVT_SG3.0/M2&M3_tvGPD/functions/")
lapply(list.files(),source)

library(quantmod)
y <- merge_df_weekly[,1]
y <- as.numeric(y)
# y <- (y-min(y))/(max(y)-min(y))+0.1
p = 3
x <- lapply(1:p, function(x) Lag(y,x))
x <- Reduce(cbind,x)

y <- y[-c(1:p)]
x <- x[-c(1:p),]
x <- cbind(rep(1,length(y)), x)

preci   = as.matrix(scale(merge_df_weekly[,2]))
tmp     = as.matrix(scale(merge_df_weekly[,4]))
AH      = as.matrix(scale(merge_df_weekly[,7]))
RH      = as.matrix(scale(merge_df_weekly[,8]))


preci <- lapply(1:p, function(x) Lag(preci,x))
preci <- Reduce(cbind,preci)
preci <- preci[-c(1:p),]

tmp   <- lapply(1:p, function(x) Lag(tmp,x))
tmp   <- Reduce(cbind,tmp)
tmp   <- tmp[-c(1:p),]

AH    <- lapply(1:p, function(x) Lag(AH,x))
AH    <- Reduce(cbind,AH)
AH    <- AH[-c(1:p),]

RH    <- lapply(1:p, function(x) Lag(RH,x))
RH    <- Reduce(cbind,RH)
RH    <- RH[-c(1:p),]


climate_mat = cbind(rep(1,length(y)), preci, tmp, AH, RH)



# MCMC
require(quantmod)
require(MASS)
require(invgamma)
require(evd)
require(msm)
require(mvtnorm)



# Model 2 : normal bulk AR(3)  + TV gpd

Xbulk2 = x

ksiT = sigT = thre = thetaksiT = thetasigT = Vksi = Wksi = Vsig = Wsig = beta = sigb = list()

MCMC_M2 = mcmc_tvary(5000, y = y, x = Xbulk2)       # set threshold prior at 0.6

MCMC_M2_0.5 = mcmc_tvary(5000, y = y, x = Xbulk2)   # set threshold prior at 0.5. Change mu_thre = 0.5 in function thre_T 
 


# Model 3 : normal bulk AR(3) + climate(4*3)  + TV gpd


Xbulk3 = cbind(x,preci, tmp, AH, RH)

ksiT = sigT = thre = thetaksiT = thetasigT = Vksi = Wksi = Vsig = Wsig = beta = sigb = list()

MCMC_M3 = mcmc_tvary(5000, y = y, x = Xbulk3)      # set threshold prior at 0.6

MCMC_M3_0.5 = mcmc_tvary(5000, y = y, x = Xbulk3)  # set threshold prior at 0.5

save.image("~/nBox/EVT_Singapore/EVT_SG3.0/M2&M3_tvGPD/out/out.RData")