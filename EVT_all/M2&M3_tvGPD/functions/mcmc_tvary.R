# y = y 

mcmc_tvary = function(niter, y, x)
{
  tt   = length(y)
  thre[[1]] = 0.3                               
  ksiT[[1]] = rep(-0.116, tt)                          
  sigT[[1]] = rep(-1.66,tt)                             
  thetaksiT[[1]] = rep(-0.116, tt+1)     
  thetasigT[[1]] = rep(-1.66, tt+1)      
  Vksi[[1]]      = 200        
  Wksi[[1]]      = 1000      
  Vsig[[1]]      = 200
  Wsig[[1]]      = 1000     
  sigb[[1]] = 0.038           # Starting value of sigma : sigma OLS
  beta[[1]] = rep(0, ncol(x)) # Actually Not used here.because starts at sigb[[1]] --> beta[[2]] --> sigb[[2]]
  f_ksi  = 700                 
  o_ksi  = 6
  l_ksi  = 8100               
  m_ksi  = 9
  m_ksi0 = thetaksiT[[1]][1]               
  C_ksi0 = 1000          
  K_ksit = 0.0001           
  K_sigt = 0.0001
  Vu     = 0.01
  f_sig  = 700
  o_sig  = 6
  l_sig  = 8100  
  m_sig  = 9
  m_sig0 = thetasigT[[1]][1]  
  C_sig0 = 1000
  B0     = rep(0, ncol(x)) 
  P0     = diag(100, nrow = ncol(x), ncol = ncol(x))
  
  for (s in 1:(niter-1))
  {
    # Sampling bulk
    temp <- beta_post(x = x, y = y, B0, P0, T0 = 1, D0 = 1, s, thre, sigb)
    beta[[s+1]] = temp$Beta
    sigb[[s+1]] = temp$Sigb
    
   
    # Sampling ksi
    ksiT[[s+1]] = newksit(y = y, x = x, ksiT, sigT, thetaksiT, thre,
                          Vksi, s, K_ksit) 
    
    # Gibbs steps updated for ksiT
    Vksi[[s+1]] = g_Vksi(tt, f_ksi, o_ksi, ksiT, thetaksiT, s)
    Wksi[[s+1]] = g_Wksi(tt, l_ksi, m_ksi, thetaksiT, s)
    thetaksiT[[s+1]] = g_thetaksi0(Wksi, thetaksiT, m_ksi0, C_ksi0, s)
    
    
    for (i in 1:(tt-1))
    {
      temp = g_thetaksi(tt, ksiT, thetaksiT, Vksi, Wksi, s, i)
      thetaksiT[[s+1]] = append(thetaksiT[[s+1]], temp)
    }
    
    thetaksiT[[s+1]] = append(thetaksiT[[s+1]], 
                              g_thetaksiT(tt, ksiT, thetaksiT, Vksi, Wksi, s))
    
  
    # Sampling sigma
    sigT[[s+1]] = newsigt(y = y,x = x, ksiT, sigT, thetasigT, thre,
                          Vsig, s, K_sigt)
    
    # Gibbs steps updated for sigT
    Vsig[[s+1]] = g_Vsig(tt, f_sig, o_sig, sigT, thetasigT, s)
    Wsig[[s+1]] = g_Wsig(tt, l_sig, m_sig, thetasigT, s)
    
    thetasigT[[s+1]] = g_thetasig0(Wsig, thetasigT, m_sig0, C_sig0, s)
    
    for (i in 1:(tt-1))
    {
      temp = g_thetasig(tt, sigT, thetasigT, Vsig, Wsig, s, i)
      thetasigT[[s+1]] = append(thetasigT[[s+1]], temp)
    }
    
    thetasigT[[s+1]] = append(thetasigT[[s+1]], 
                              g_thetasigT(tt, sigT, thetasigT, Vsig, Wsig, s))
    
    # Sampling thre
    thre[[s+1]]  = thre_T(y, x, ksiT, sigT, thre, s, Vu, beta, sigb)
    print(s)     
    
  }
  
  return(list('lksiT' = ksiT,
              'thetaksiT' = thetaksiT,
              'lsigT' = sigT,
              'thetasigT' = thetasigT,
              'thre' = thre,
              'beta' = beta,
              'sigb' = sigb))

}



