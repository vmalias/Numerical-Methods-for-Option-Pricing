###################################
# Binomial tree pricing           #
# European Call option            #
# Iput: SO -> Initial stock price #
#       K  -> Strike price        #
#       T -> Maturity             #
#       N -> steps in tree        #
#   sigma -> stock volatility     #
#       r -> risk-free rate       #
###################################

BinTreeEuCall<- function(S0,K,T,N,sigma,r){
  
  dt <- T/N
  u  <- exp(sigma*sqrt(dt)) 
  d  <- 1/u                  
  p  <- (exp(r*dt) - d)/(u-d)
  
tree = matrix(0, nrow=N+1, ncol=N+1)
  
for (i in 0:N){ #option values at end of tree
  tree[N+1,i+1] <- max(0, S0*(u^i)*d^(N-i) - K)
}
  
for (i in seq(from=N-1, to=0, by=-1)){ #CRR formula                        
for (j in 0:i){                                         
  tree[i+1,j+1] = exp(-r*dt)*(p*tree[i+2,j+2] + (1-p)*tree[i+2,j+1])
 }
}
#print(paste("European Call Option Price is:", round(tree[1,1],3)))
return(tree[1,1])
}

BinTreeEuCall(100,100,1,20,0.2,0.05)