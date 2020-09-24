# Model 5 : constant beta model 


mcmc_c = function(niter, y, x1, x2) # x1 is for normal bulk. x2 is for GPD regression
{
  tt   = length(y)
  p    = ncol(x2) - 1
  thre[[1]] = 0.3                                           
  ksiT[[1]] = rep(-0.116, tt)                      
  sigT[[1]] = rep(-1.66,tt)           
  betaksi[[1]]   = c(-0.127,-0.006,-0.005,-0.004,-0.126,-0.175,-0.45,0.07,0.126,0.335,-0.08,-0.126,-0.33)
  #  these start values from lm(MCMC_M2$lksiT[[5000]] ~ Xextre - 1)  
  betasig[[1]]   = c(-1.6,-0.0077,0.0002,-0.018,-0.5,-0.35,-0.3,0.4,0.26,0.26,-0.38,-0.25,-0.22)
  #  these start values from lm(MCMC_M2$lsigT[[5000]] ~ Xextre - 1)  
  Vksi[[1]]      = 200                                  
  Vsig[[1]]      = 200                           
  sigb[[1]] = 0.038                                     
  beta[[1]] = rep(0, ncol(x1)) 
  f_ksi  = 700                 
  o_ksi  = 6
  K_ksit = 0.0001          
  K_sigt = 0.0001
  Vu     = 0.01
  f_sig  = 700
  o_sig  = 6
 
  B0     = rep(0, ncol(x1)) 
  P0     = diag(100, nrow = ncol(x1), ncol = ncol(x1))
  
  for (s in 1:(niter-1))
  {
    # Sampling bulk
    temp <- beta_post(x1, y, B0, P0, T0 = 1, D0 = 1, s, thre, sigb)
    beta[[s+1]] = temp$Beta
    sigb[[s+1]] = temp$Sigb
    
    # Sampling ksi
    ksiT[[s+1]] = newksit_c(y,x2,ksiT, sigT, betaksi, thre, Vksi, s, K_ksit) 
    
    # Gibbs steps updated for ksiT
    
 
    Vksi[[s+1]]    = gc_Vksi(tt, f_ksi, o_ksi, ksiT, betaksi, s, x2) 
    
    
    betaksi[[s+1]] = gc_betaksi(Vksi, ksiT, s, x2)
  
    
    # Sampling sigma
    sigT[[s+1]]    = newsigt_c(y,x2, ksiT, sigT, betasig, thre,
                            Vsig, s, K_sigt)
   
    
    
    # Gibbs steps updated for sigT

    
    Vsig[[s+1]]    = gc_Vsig(tt, f_sig, o_sig, sigT, betasig, s, x2)
    
    betasig[[s+1]] = gc_betasig(Vsig,sigT, s, x2)
    
    # Sampling thre
    thre[[s+1]]    = thre_T(y, x1, ksiT, sigT, thre, s, Vu, beta, sigb)
    
    print(s)
  }
  
  return(list('lksiT'   = ksiT,
              'lsigT'   = sigT,
              'betaksi' = betaksi,
              'betasig' = betasig,
              'thre' = thre,
              'beta' = beta,
              'sigb' = sigb))
  
  
}











