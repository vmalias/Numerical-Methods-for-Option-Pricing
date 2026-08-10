kr_american_put = function(S,K,T,N,l,sigma,r){
  
  dt = T/N
  
  Stock = matrix(0, 2*N + 1, N+1 )
  Stock[1] = S
  Kammrad = matrix(0, 2*N + 1, N+1 )
  
  u = exp(l*sigma*dt^0.5)
  
  p1 = (1 / (2*l^2)) + ((r-0.5*sigma^2)*dt^0.5) / (2*l*sigma)
  p3 = (1 / (2*l^2)) - ((r-0.5*sigma^2)*dt^0.5) / (2*l*sigma)
  p2 = 1 - (1 / l^2)
  
  
  for (i in 1:N) {
    for (j in seq(i,-i,-1)) {
      Stock[j+i+1, i+1] = S * u^j    
    }                               
  }                               

  Kammrad[,ncol(Kammrad)] = pmax(K - Stock[,ncol(Stock)], 0)
  for (i in (ncol(Kammrad)-1):1) {                      
    for (j in 1:(2*(i-1)+1) ) {                            
      Kammrad[j,i] = max( K - Stock[j,i],
        exp(-r*dt)* (p3 * Kammrad[j,i+1] + p2 * Kammrad[j+1,i+1] + p1 * Kammrad[j+2,i+1]))
    }
  }
  
  return(Kammrad[1])
}


