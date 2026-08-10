# Kamrad-Ritchken trinomial tree option pricing model / European Put Option

# current_price  : Price of the underlying asset at time 0
# strike_price   : Strike Price
# maturity       : Maturity
# n_steps        : Number of steps in the trinomial model
# lambda         : Special parameter of the trinomial model
# volatility     : Volatility of the underlying asset
# risk_free_rate : Risk-free rate

kr_european_put = function(current_price, strike_price, maturity, n_steps, lambda, volatility, risk_free_rate){
  
  # Time increment per step
  dt = maturity / n_steps
  
  # Up-move factor
  u = exp(lambda * volatility * sqrt(dt))
  
  # Risk-neutral probabilities of an up-move (p_up), flat-move (p_flat), and down-move (p_down)
  p_up = (1 / (2 * lambda^2)) + ((risk_free_rate - 0.5 * volatility^2) * sqrt(dt)) / (2 * lambda * volatility)
  p_down = (1 / (2 * lambda^2)) - ((risk_free_rate - 0.5 * volatility^2) * sqrt(dt)) / (2 * lambda * volatility)
  p_flat = 1 - (1 / lambda^2)
  
  # Tree of underlying asset prices; stock_tree[j+i+1, i+1] holds the price at step i, j up-moves from the middle
  stock_tree = matrix(0, nrow = 2 * n_steps + 1, ncol = n_steps + 1)
  stock_tree[1, 1] = current_price
  
  for (i in 1:n_steps){
    for (j in seq(i, -i, -1)){
      stock_tree[j + i + 1, i + 1] = current_price * u^j
    }
  }
  
  # Tree of option values, aligned node-for-node with stock_tree
  option_tree = matrix(0, nrow = 2 * n_steps + 1, ncol = n_steps + 1)
  
  # Payoffs at maturity
  option_tree[, ncol(option_tree)] = pmax(strike_price - stock_tree[, ncol(stock_tree)], 0)
  
  # Going backwards to calculate the option value at time 0, i.e. the premium.
  # (European exercise, so no early-exercise comparison at each node)
  for (i in (ncol(option_tree) - 1):1){
    for (j in 1:(2 * (i - 1) + 1)){
      option_tree[j, i] = exp(-risk_free_rate * dt) * (p_down * option_tree[j, i + 1] + p_flat * option_tree[j + 1, i + 1] + p_up * option_tree[j + 2, i + 1])
    }
  }
  
  # Price of the option
  premium = option_tree[1, 1]
  
  return(premium)
}
