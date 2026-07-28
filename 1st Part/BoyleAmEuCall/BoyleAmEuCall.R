BoyleAmEuCall = function(S,K,T,N,l,sigma,r){

 dt = T/N  
  
 Stock = matrix(0, 2*N + 1, N+1 )
 Stock[1] = S
 Boyle = matrix(0, 2*N + 1, N+1 )

 M = exp(r * dt) 
 Var = M^2 *(exp(sigma^2 * dt) - 1)
 u = exp(l*sigma*dt^0.5)

 p1 = ( (Var + M^2 - M)*u - (M - 1) ) / ( (u - 1) * (u^2 - 1))
 p3 = ( u^2 * (Var + M^2 - M) - u^3 * (M - 1) ) / ( (u - 1) * (u^2 - 1))
 p2 = 1 - p1 - p3


 for (i in 1:N) {
   for (j in seq(-i,i,1)) {
     Stock[j+i+1, i+1] = S * u^j
   }
 }

 Boyle[,ncol(Boyle)] = pmax(Stock[,ncol(Stock)] - K, 0)
 for (i in (ncol(Boyle)-1):1) {                       # switchEuAm will take the value 1  
   for (j in 1:(2*i-1) ) {                            # if we examine the case of American option  
     Boyle[j,i] = max(                                # and value 0 in European option  
       exp(-r*dt)* (p3 * Boyle[j,i+1] + p2 * Boyle[j+1,i+1] + p1 * Boyle[j+2,i+1]))
   }
 }

# return(Stock)
 cat("Option value is:", Boyle[1])
}

# i cant, usually, observe any difference between Eu and Am option, 
# but these parameters bellow, are ideal to watch actual differences.

BoyleAmEuCall(100,100,1,64,1.22474,0.2,0.05)

