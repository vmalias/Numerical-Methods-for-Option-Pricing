# Cox-Ross-Rubinstein binomial tree option pricing model / European Put Option

# current_price  : Price of the underlying asset at time 0
# strike_price   : Strike Price
# maturity       : Maturity
# n_steps        : Number of steps in the binomial tree
# volatility     : Volatility of the underlying asset
# risk_free_rate : Risk-free rate

crr_european_put = function(current_price, strike_price, maturity, n_steps, volatility, risk_free_rate){
  
  # Time increment per step
  dt = maturity / n_steps
  
  # Up and down factors and the corresponding risk-neutral probability
  u = exp(volatility * sqrt(dt))
  d = 1 / u
  p = (exp(risk_free_rate * dt) - d) / (u - d)
  
  # Tree of option values; tree[i+1, j+1] holds the value at step i, with j up-moves
  tree = matrix(0, nrow = n_steps + 1, ncol = n_steps + 1)
  
  # Payoffs at maturity
  for (j in 0:n_steps){
    tree[n_steps + 1, j + 1] = max(0, strike_price - current_price * (u^j) * d^(n_steps - j))
  }
  
  # Going backwards to calculate the option value at time 0, i.e. the premium.
  for (i in seq(from = n_steps - 1, to = 0, by = -1)){
    for (j in 0:i){
      tree[i + 1, j + 1] = exp(-risk_free_rate * dt) * (p * tree[i + 2, j + 2] + (1 - p) * tree[i + 2, j + 1])
    }
  }
  
  # Price of the option
  premium = tree[1, 1]
  
  return(premium)
}