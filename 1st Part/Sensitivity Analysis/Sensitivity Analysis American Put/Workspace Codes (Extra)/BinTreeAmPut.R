  ###################################
# Binomial tree pricing           #
# American Put option             #
# Iput: SO -> Initial stock price #
#       K  -> Strike price        #
#       T -> Maturity             #
#       N -> steps in tree        #
#   sigma -> stock volatility     #
#       r -> risk-free rate       #
###################################

BinTreeAmPut<- function(S0,K,T,N,sigma,r){
  
  dt <- T/N
  u  <- exp(sigma*sqrt(dt)) 
  d  <- 1/u                  
  p  <- (exp(r*dt) - d)/(u-d)
  
  tree = matrix(0, nrow=N+1, ncol=N+1)
  
  for (j in 0:N){
    tree[N+1,j+1] <- max(0,K- S0*(u^j)*d^(N-j))
  }
  
  for (i in seq(from=N-1, to=0, by=-1)){                                
    for (j in 0:i){                                         
      tree[i+1,j+1] = max(K- S0*(u^j)*d^(i-j), 
                   exp(-r*dt)*(p*tree[i+2,j+2] + (1-p)*tree[i+2,j+1]))
    }
  }
  #print(paste("American Put Option Price is:", round(tree[1,1],3)))
  return(tree[1])
}

BinTreeAmPut(100,100,1,50,0.2,0.05)