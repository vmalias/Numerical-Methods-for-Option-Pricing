# Black-Scholes option pricing model / European Call Option 

# K : Strike Price
# S0 : Price of the underlying asset at time 0
# T : Maturity
# sigma : Volatility of the underlying asset
# r : Risk-free rate                             


BlackScholesEuCall <- function(S0,K,T,sigma,r){
  
  # d1 and d2 are parameters of the Black-Scholes formula
  d1 = (log(S0/K) + (r + 0.5*sigma^2)*T)/(sigma*sqrt(T))
  d2 = d1 - sigma*sqrt(T)
  
  # Calculate the price of the European option using the Black-Scholes formula
  C = S0*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
  return(C)
}

# An example of inputs:
BlackScholesEuCall(100,100,1,0.2,0.01)
