model{
  for (i in 1:833){
    
    for (num_cutpt in 1:3){
      Z[i,num_cutpt] <- ( alpha_SD*SD[i] 
                          + alpha_N*Nitrogen[i]
                          + alpha_P*Phosphorus[i]
                          + alpha_DIN*DIN[i]
                          + alpha_DIP*DIP[i]
                          + alpha_SubR[Subregion[i]]
                          - C[num_cutpt])
      / s
      
      Q[i,num_cutpt] <- 1/(1+exp(-Z[i,num_cutpt]))
    }
    
    P[i,1] <- max(min(1 - Q[i,1],1),0)
    P[i,2] <- Q[i,1] - Q[i,2]
    P[i,3] <- Q[i,2] - Q[i,3]
    P[i,4] <- max(min(Q[i,3],1),0)
    
    TS[i] ~ dcat(P[i,])
  }
  
  # PRIORS
  # POLR Coefficients
  alpha_SD ~ dnorm(0,.0001)
  alpha_N ~ dnorm(0,.0001)
  alpha_P ~ dnorm(0,.0001)
  alpha_DIN ~ dnorm(0,.0001)
  alpha_DIP ~ dnorm(0,.0001)
  
  for (j in 1:33){
    alpha_SubR[j] ~ dnorm(0,.0001)
  }
  
  s ~ dlnorm(mu.log.s, tau.log.s)
  mu.log.s ~ dnorm(0,0.0001)
  tau.log.s <- pow(sigma.log.s,-2)
  sigma.log.s ~ dunif(0,1000)
  
  for (i in 1:3) {
    cutpt_raw[i] ~ dnorm(0, .0001)
  }
  C <- sort(cutpt_raw)
  
}