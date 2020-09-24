rm(list=ls())
require(msm)       # for truncated normal distribution
require(evd)       # for Generalized Pareto Distribution 

# Note that x1,x2 have already include the 1 column for intercept 

# Data 
load("~/nBox/EVT_Singapore/data/timecount_weathermerge_31Jul.RData")
setwd("~/nBox/EVT_Singapore/EVT_SG3.0/M1_gamma/functions/")
lapply(list.files(),source)

library(quantmod)
y <- merge_df_weekly[,1]
y <- as.numeric(y)
y <- (y-min(y))/(max(y)-min(y))+0.1
p = 3


# MCMC
require(quantmod)
require(MASS)
require(invgamma)
require(evd)
require(msm)
require(mvtnorm)

# Note that for Model 1, set threshold prior at 0.3. (0.5 can not run longer)

MCMC_M1 = MCMC_mixture(startvalue = c(6,  #initial value alpha
                                      40,   #initial value beta
                                      0.3,  #initial value threshold
                                      0.15, #initial value Sigma
                                      0.1), #initial value Xi
                       data = y,
                       V = c(2,      #proposal variance alpha
                             2,      #proposal variance beta
                             0.01,   #proposal variance threshold
                             0.1,    #proposal variance Sigma
                             0.5),   #proposal variance Xi
                       Ga = c(1,   
                              1,
                              1,
                              1),
                       niter=10000)


plot.ts(MCMC_M1)
# ksi:    mean(MCMC_M1[-(1:5000),5]) -0.11
# sigma : mean(MCMC_M1[-(1:5000),4]) 0.198
# These two values above give sensible return levels
save.image("~/nBox/EVT_Singapore/EVT_SG3.0/M1_gamma/out/out.RData")
