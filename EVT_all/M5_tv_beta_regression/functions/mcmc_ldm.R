# Model 5 : linear dynamic model 


betaksi = list()
betasig = list()
mcmc_ldm = function(niter, y, x1, x2) # x1 is for normal bulk. x2 is for GPD regression
{
  tt   = length(y)
  p    = ncol(x2) - 1
  thre[[1]] = 0.3            
  ksiT[[1]] = rep(-0.116, tt)                            
  sigT[[1]] = rep(-1.66,  tt)                             
  betaksi[[1]]   = matrix(rep(c(-0.127,-0.006,-0.005,-0.004,-0.126,-0.175,-0.45,0.07,0.126,0.335,-0.08,-0.126,-0.33),(tt+1) * (p+1)), 
                          nrow = tt+1, ncol = p+1, byrow = T)  
  #  these start values from lm(MCMC_M2$lksiT[[5000]] ~ Xextre - 1)  
  betasig[[1]]   = matrix(rep(c(-1.6,-0.0077,0.0002,-0.018,-0.5,-0.35,-0.3,0.4,0.26,0.26,-0.38,-0.25,-0.22),(tt+1) * (p+1)), 
                          nrow = tt+1, ncol = p+1, byrow = T)  
  #  these start values from lm(MCMC_T$lsigT ~ extre  - 1)  
  Vksi[[1]]      = 200                                  
  Wksi[[1]]      = 10000                                # use 10000 instead of 1000                                 
  Vsig[[1]]      = 200                           
  Wsig[[1]]      = 10000     
  sigb[[1]] = 0.038                                    
  beta[[1]] = rep(0, ncol(x1)) 
  f_ksi  = 700                 
  o_ksi  = 6
  l_ksi  = 81000      
  m_ksi  = 9
  m_ksi0 = 0.01             
  C_ksi0 = 1000          
  K_ksit = 0.0001          
  K_sigt = 0.0001
  Vu     = 0.01
  f_sig  = 700
  o_sig  = 6
  l_sig  = 81000  
  m_sig  = 9
  m_sig0 = -6.4  
  C_sig0 = 1000
  B0     = rep(0, ncol(x1)) 
  P0     = diag(100, nrow = ncol(x1), ncol = ncol(x1))
  
  for (s in 1:(niter-1))
  {
    # Sampling bulk
    temp <- beta_post(x1, y, B0, P0, T0 = 1, D0 = 1, s, thre, sigb)
    beta[[s+1]] = temp$Beta
    sigb[[s+1]] = temp$Sigb
    
    # Sampling ksi
    ksiT[[s+1]] = newksit_l(y, x2, ksiT, sigT, betaksi, thre,
                            Vksi, s, K_ksit) 
    
    
    # Gibbs steps updated for ksiT
    
    # Vksi
    Vksi[[s+1]] = gr_Vksi(tt, f_ksi, o_ksi, ksiT, betaksi, s, x2)
    
    # Wksi_k
    Wksi[[s+1]] = rep(NA, p+1)
    for (i in 1:(p+1))  
    {
      Wksi[[s+1]][i] = gr_Wksi_k(tt, l_ksi, m_ksi, betaksi, s, k = i-1) # because k = 0,1,2,..p
    }
    
    betaksi[[s+1]] = matrix(0, nrow = tt+1, ncol = p+1)
    
    
    # betaksi 0,k   --> i.e. betaksi[1,]
    for (i in 1:(p+1))
    {
      betaksi[[s+1]][1,i] = gr_betaksi0_k(Wksi, betaksi, m_ksi0, C_ksi0, s, k = i-1)
    }
    
    
    
    # betaksi t,k   -->  t = 1,2,...T
    
    for (i in 1:(tt-1))
    {
      for (j in 1:(p+1))
      {
        betaksi[[s+1]][i+1,j] = gr_betaksi_k(tt, ksiT, betaksi, Vksi, Wksi, s, i, k = j-1, x2)
      }
      
    }    
    
    # betaksi T,k  
    
    for (j in 1:(p+1))
    {
      betaksi[[s+1]][tt+1,j] = gr_betaksiT_k(tt, ksiT, betaksi, Vksi, Wksi, s, k = j-1, x2)  
    }
    
    
    # Sampling sigma
    sigT[[s+1]] = newsigt_l(y, x2, ksiT, sigT, betasig, thre,
                            Vsig, s, K_sigt)
    
    
    # Gibbs steps updated for sigT
    
    # Vsig
    Vsig[[s+1]] = gr_Vsig(tt, f_sig, o_sig, sigT, betasig, s, x2)
    
    Wsig[[s+1]] = rep(NA, p+1)
    # Wsig_k
    for (i in 1:(p+1))  
    {
      Wsig[[s+1]][i] = gr_Wsig_k(tt, l_sig, m_sig, betasig, s, k = i-1) # because k = 0,1,2,..p
    }
    
    betasig[[s+1]] = matrix(0, nrow = tt+1, ncol = p+1)
    
    # betasig 0,k   --> i.e. betasig[1,]
    for (i in 1:(p+1))
    {
      betasig[[s+1]][1,i] = gr_betasig0_k(Wsig, betasig, m_sig0, C_sig0, s, k = i-1)
    }
    
    # betasig t,k   -->  t = 1,2,...T
    
    for (i in 1:(tt-1))
    {
      for (j in 1:(p+1))
      {
        betasig[[s+1]][i+1,j] = gr_betasig_k(tt, sigT, betasig, Vsig, Wsig, s, i, k = j-1, x2)
      }
      
    }
    
    # betasig T,k  
    
    for (j in 1:(p+1))
    {
      betasig[[s+1]][tt+1,j] = gr_betasigT_k(tt, sigT, betasig, Vsig, Wsig, s, k = j-1, x2)  
    }
    
    # Sampling thre
    thre[[s+1]]  = thre_T(y, x1, ksiT, sigT, thre, s, Vu, beta, sigb)
    
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











