# Note that x1,x2 have already include the 1 column for intercept 

# Data 
rm(list=ls())
require(msm)       # for truncated normal distribution
require(evd)       # for Generalized Pareto Distribution 

# Note that x1,x2 have already include the 1 column for intercept 

# Data 
load("~/nBox/EVT_Singapore/data/timecount_weathermerge_31Jul.RData")
setwd("~/nBox/EVT_Singapore/EVT_SG3.0/M4_constant_beta_regression/functions/")
lapply(list.files(),source)

library(quantmod)
y <- merge_df_weekly[,1]
y <- as.numeric(y)
y <- (y-min(y))/(max(y)-min(y))+0.1
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




# Model 4 : normal bulk AR(3) + climate(4*3) + TV gpd regression

Xbulk4 = cbind(x,preci, tmp, AH, RH)
Xextre = climate_mat

ksiT = sigT = thre = thetaksiT = thetasigT = Vksi = Wksi = Vsig = Wsig = beta = sigb = list()


# constant beta
MCMC_M4 = mcmc_c(5000, y = y, x1 = Xbulk4, x2 = Xextre) # set threshold prior = 0.6
MCMC_M4_0.5 = mcmc_c(5000, y = y, x1 = Xbulk4, x2 = Xextre) # set threshold prior = 0.5. Remember change prior in thre_T 

save.image("~/nBox/EVT_Singapore/EVT_SG3.0/M4_constant_beta_regression/out/out.RData")


