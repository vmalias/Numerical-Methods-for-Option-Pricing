#------------------------------------#
# Black-Scholes option pricing model #
#      European Call Option          #
# Inputs: S0,K,T,σ,r                 #
# Output: Option price               #
#------------------------------------#

BlackScholesEuCall <- function(S0,K,T,sigma,r){
  d1 <- (log(S0/K) + (r + 0.5*sigma^2)*T)/(sigma*sqrt(T))
  d2 <- d1 - sigma*sqrt(T)
  C <- S0*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
  return(C)
}
BlackScholesEuCall(100,100,1,0.2,0.05)
