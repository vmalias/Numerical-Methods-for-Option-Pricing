# Black-Scholes option pricing model / European Put Option 

# K : Strike Price
# S0 : Price of the underlying asset at time 0
# T : Maturity
# sigma : Volatility of the underlying asset
# r : Risk-free rate                             


BlackScholesEuPut <- function(S0,K,T,sigma,r){
    
  # d1 and d2 are parameters of the Black-Scholes formula
  d1 = (log(S0/K) + (r + 0.5*sigma^2)*T)/(sigma*sqrt(T))
  d2 = d1 - sigma*sqrt(T)
  
  # Calculate the price of the European option (P) using the Black-Scholes formula
  P = K*exp(-r*T)*pnorm(-d2) - S0*pnorm(-d1)
  return(P)
}

# An example of inputs:
# BlackScholesEuPut(50,50,5/12,0.4,0.1)

