BlackScholesEuPut <- function(S0,K,T,sigma,r){
  d1 <- (log(S0/K) + (r + 0.5*sigma^2)*T)/(sigma*sqrt(T))
  d2 <- d1 - sigma*sqrt(T)
  C <- K*exp(-r*T)*pnorm(-d2) - S0*pnorm(-d1)
  return(C)
}
BlackScholesEuPut(50,50,5/12,0.4,0.1)

