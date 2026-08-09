# Black-Scholes option pricing model / European Call Option 

# strike_price : Strike Price
# current_price : Price of the underlying asset at time 0
# maturity : Maturity
# volatility : Volatility of the underlying asset
# risk_free_rate : Risk-free rate                             


black_scholes_european_call <- function(current_price,strike_price,maturity,volatility,risk_free_rate){
  
  # d1 and d2 are parameters of the Black-Scholes formula
  d1 = (log(current_price/strike_price) + (risk_free_rate + 0.5*volatility^2)*maturity)/(volatility*sqrt(maturity))
  d2 = d1 - volatility*sqrt(maturity)
  
  # Calculate the price of the European option using the Black-Scholes formula
  premium = current_price*pnorm(d1) - strike_price*exp(-risk_free_rate*maturity)*pnorm(d2)
  return(premium)
}

# An example of inputs:
black_scholes_european_call(100,100,1,0.2,0.05)
