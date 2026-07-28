KammradAmEuCall = function(S,K,T,N,l,sigma,r){
  
  dt = T/N
  
  # create a matrix with really small numbers, because later we have to compute the exp() of this matrix
  Stock = matrix(0, 2*N + 1, N+1 )
  Stock[1] = S
  Kammrad = matrix(0, 2*N + 1, N+1 )

  u = exp(l*sigma*dt^0.5)
  
  p1 = (1 / (2*l^2)) + ((r-0.5*sigma^2)*dt^0.5) / (2*l*sigma)
  p3 = (1 / (2*l^2)) - ((r-0.5*sigma^2)*dt^0.5) / (2*l*sigma)
  p2 = 1 - (1 / l^2)
  
  
  for (i in 1:N) {
    for (j in seq(i,-i,-1)) {
      Stock[j+i+1, i+1] = S * u^j    # another expression could be log(S) + j*u, 
                                     # if i had define u as l*sigma*dt^0.5   
    }                                # and maybe fits better in the method of Kammrad n' Ritchken
  }                                  # BUT i believe, with this way i can, clearly,      
                                     # show that it's only an extension of Boyle's one.
                                     
  Kammrad[,ncol(Kammrad)] = pmax(Stock[,ncol(Stock)] - K, 0)
  for (i in (ncol(Kammrad)-1):1) {                      
    for (j in 1:(2*(i-1)+1) ) {                            
      Kammrad[j,i] = max( 
        exp(-r*dt)* (p3 * Kammrad[j,i+1] + p2 * Kammrad[j+1,i+1] + p1 * Kammrad[j+2,i+1]))
    }
  }
  
  return(Kammrad[1])
  #cat("Option price is:", Kammrad[1])
}


# Endly, the way to programm Kammrad tree, is exactly
# the same with Boyle's tree, except for probabilities computation. 

KammradAmEuCall(100,100,1,50,1.22474,0.2,0.05)

