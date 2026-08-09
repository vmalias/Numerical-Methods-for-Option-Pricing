KammradAmEuCall = function(S,K,T,N,l,sigma,r){
  
  dt = T/N
  
  Kammrad = matrix(0, 2*N + 1, N+1 )

  u = exp(l*sigma*dt^0.5)
  
  pu = (1 / (2*l^2)) + ((r-0.5*sigma^2)*dt^0.5) / (2*l*sigma)
  pd = (1 / (2*l^2)) - ((r-0.5*sigma^2)*dt^0.5) / (2*l*sigma)
  pm = 1 - (1 / l^2)
  
  j = seq(-N, N, 1)
  Kammrad[,ncol(Kammrad)] = pmax(S* u^j - K, 0)
  for (i in (ncol(Kammrad)-1):1) {                      
    for (j in 1:(2*(i-1)+1) ) {                            
      Kammrad[j,i] = max( 
        exp(-r*dt)* (pu * Kammrad[j+2,i+1] + pm * Kammrad[j+1,i+1] + pd * Kammrad[j,i+1]))
    }
  }
  
  return(Kammrad[1,1])
}


# Endly, the way to programm Kammrad tree, is exactly
# the same with Boyle's tree, except for probabilities computation. 

KammradAmEuCall(100,100,1,20,1.22474,0.2,0.05)

