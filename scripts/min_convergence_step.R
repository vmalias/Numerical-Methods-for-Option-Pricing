# Minimum Convergence Step
# We find the smallest number of steps (N)
# beyond which the model price stays within a given precision 
# of the benchmark model's price for three different strike prices.
# On one hand the benchmark price for European options is the one that
# Black-Scholes provides us. On the other hand, for the case of the 
# American Put we consider as benchmark model, the trinomial tree of
# Kamrad-Ritchken for 1000 iterations.

# Load the functions we are going to employ
source("black_scholes_european_call.R")
source("black_scholes_european_put.R")
source("cox_ross_rubinstein_european_call.R")
source("cox_ross_rubinstein_european_put.R")
source("cox_ross_rubinstein_american_put.R")
source("kamrad_ritchken_european_call.R")
source("kamrad_ritchken_european_put.R")
source("kamrad_ritchken_american_put.R")

# Create preliminary sequences for different numbers of iterations/periods,
# strike prices and precision levels.
strike_price = seq(90, 110, 10)
periods = seq(1, 100)
precision_levels = c(0.005, 0.01, 0.05)

# Function that evaluates the relative error for different strike prices and
# number of iteration. The function accepts the tree and benchmark models 
# as arguments and handles properly the special parameter λ of the trinomial model. 
error = function(model, benchmark_model, strike_price, periods,
                                current_price, maturity, volatility, risk_free_rate,
                                extra_arguments = list()){
  
  # Matrix initialized with zeros, which for every combination of numbers of periods 
  # and strike price contains the relative errors.
  err = matrix(0, nrow = length(strike_price), ncol = length(periods))
  
  for (i in seq_along(strike_price)){

    # Calculate the benchmark model's value
    real_value = benchmark_model(current_price, strike_price[i], maturity, volatility, risk_free_rate)
    
    for (j in seq_along(periods)){

      # Create a list with the model's arguments
      model_arguments = c(list(current_price, strike_price[i], maturity, periods[j]), extra_arguments, list(volatility, risk_free_rate))
      
      # Calculate the option price
      model_price = do.call(model, model_arguments)

      # Fill the matrix with the relative errors
      err[i, j] = abs((model_price - real_value) / real_value)
    }
  }
  
  # Name the rows and columns of the error matrix
  colnames(err) = as.character(periods)
  rownames(err) = as.character(strike_price)

  return(err)
}

# For each precision level and strike price, find the smallest number of iterations
# needed such that our model produces error below that precision.
minimum_convergence_step = function(error_table, precision_levels, periods){

  # Matrix initialized with zeros, which for every combination  
  # of precision level and relative error.  
  mcs = matrix(0, nrow = length(precision_levels), ncol = nrow(error_table))
  
  for (i in seq_along(precision_levels)){
    for (j in seq_len(nrow(error_table))){
      
      # Find the indices of the table with the relative errors, that correspond
      # to a precision higher than the desired level.
      indices_above_precision_level = which(error_table[j, ] >= precision_levels[i])

      # Find the greater of the above indices
      greatest_index_above_precision_level = if (length(indices_above_precision_level) == 0) 0 else max(indices_above_precision_level)
      
      # Calculate the minimum convergence step
      mcs[i, j] = periods[greatest_index_above_precision_level + 1]
    }
  }
  
  # Name the rows and columns of the minimum convergence step matrix
  colnames(mcs) = as.character(rownames(error_table))
  rownames(mcs) = as.character(precision_levels)

  return(mcs)
}


################################### European Call #######################################

error_crr_european_call = error(crr_european_call, bs_european_call, strike_price, periods,
                                      current_price = 100, maturity = 1, volatility = 0.2, risk_free_rate = 0.05)
error_kr_european_call = error(kr_european_call, bs_european_call, strike_price, periods,
                                     current_price = 100, maturity = 1, volatility = 0.2, risk_free_rate = 0.05,
                                     extra_arguments = list(1.22474))

minimum_convergence_step_crr_european_call = minimum_convergence_step(error_crr_european_call, precision_levels, periods)
minimum_convergence_step_kr_european_call = minimum_convergence_step(error_kr_european_call, precision_levels, periods)


############################# European Put #####################################

error_crr_european_put = error(crr_european_put, bs_european_put, strike_price, periods,
                                     current_price = 100, maturity = 1, volatility = 0.2, risk_free_rate = 0.05)
error_kr_european_put = error(kr_european_put, bs_european_put, strike_price, periods,
                                    current_price = 100, maturity = 1, volatility = 0.2, risk_free_rate = 0.05,
                                    extra_arguments = list(1.22474))

minimum_convergence_step_crr_european_put = minimum_convergence_step(error_crr_european_put, precision_levels, periods)
minimum_convergence_step_kr_european_put = minimum_convergence_step(error_kr_european_put, precision_levels, periods)


############################## American Put ####################################

# We set the benchmark model as the Kamrad-Ritchken trinomial tree with 1000 iterations
benchmark_american_put = function(current_price, strike_price, maturity, volatility, risk_free_rate){
  kr_american_put(current_price, strike_price, maturity, 1000, 1.22474, volatility, risk_free_rate)
}

error_crr_american_put = error(crr_american_put, benchmark_american_put, strike_price, periods,
                                        current_price = 100, maturity = 1, volatility = 0.2, risk_free_rate = 0.05)
error_kr_american_put = error(kr_american_put, benchmark_american_put, strike_price, periods,
                                       current_price = 100, maturity = 1, volatility = 0.2, risk_free_rate = 0.05,
                                       extra_arguments = list(1.22474))

minimum_convergence_step_crr_american_put = minimum_convergence_step(error_crr_american_put, precision_levels, periods)
minimum_convergence_step_kr_american_put = minimum_convergence_step(error_kr_american_put, precision_levels, periods)