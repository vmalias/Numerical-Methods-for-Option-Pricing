# RMSE Comparison
# For the European Call, European Put, and American Put, we compare how
# quickly the CRR and KR trees converge to their benchmark price as the
# number of steps increases, using the same shared set of random
# parameters (loaded from parameters.xlsx) for all three cases. For the
# European options, the benchmark is the Black-Scholes closed-form price;
# for the American Put, which has no closed form, the benchmark is the
# Kamrad-Ritchken tree with 1000 steps.

library(ggplot2)
library(openxlsx)

# Load the functions we are going to employ
source("black_scholes_european_call.R")
source("black_scholes_european_put.R")
source("cox_ross_rubinstein_european_call.R")
source("cox_ross_rubinstein_european_put.R")
source("cox_ross_rubinstein_american_put.R")
source("kamrad_ritchken_european_call.R")
source("kamrad_ritchken_european_put.R")
source("kamrad_ritchken_american_put.R")

# Load the shared set of random parameters generated in generate_parameters.R
parameters = read.xlsx("parameters.xlsx")
current_price = parameters$current_price
maturity = parameters$maturity
volatility = parameters$volatility
risk_free_rate = parameters$risk_free_rate

n = nrow(parameters)
strike_price = 100
periods = c(25, 50, 100, 200, 400, 800)

# For a given pair of tree pricing functions and a benchmark price vector,
# this computes each tree's price across all scenarios and step counts,
# then the RMSE of relative errors against the benchmark (restricted to
# scenarios where the benchmark price is at least 0.5, to avoid RMSE
# being dominated by scenarios where the option is worth almost nothing).
compute_rmse = function(crr_price_fn, kr_price_fn, benchmark_price, current_price, strike_price,
                         maturity, volatility, risk_free_rate, periods){

  crr_price = matrix(0, n, length(periods))
  colnames(crr_price) = as.character(periods)
  kr_price = matrix(0, n, length(periods))
  colnames(kr_price) = as.character(periods)

  for (i in 1:n){
    for (j in seq_along(periods)){
      crr_price[i, j] = crr_price_fn(current_price[i], strike_price, maturity[i], periods[j], volatility[i], risk_free_rate[i])
      kr_price[i, j] = kr_price_fn(current_price[i], strike_price, maturity[i], periods[j], 1.22474, volatility[i], risk_free_rate[i])
    }
  }

  # Only keep scenarios where the benchmark price is not too close to zero
  valid_scenarios = which(benchmark_price >= 0.5)

  rmse_crr = matrix(0, length(periods), 1)
  rmse_kr = matrix(0, length(periods), 1)

  for (j in seq_along(periods)){
    rmse_crr[j] = sqrt(mean(((crr_price[valid_scenarios, j] - benchmark_price[valid_scenarios]) / benchmark_price[valid_scenarios])^2))
    rmse_kr[j] = sqrt(mean(((kr_price[valid_scenarios, j] - benchmark_price[valid_scenarios]) / benchmark_price[valid_scenarios])^2))
  }

  rmse_table = data.frame(rmse_crr, rmse_kr)
  colnames(rmse_table) = c("RMSE_CRR", "RMSE_KR")
  rownames(rmse_table) = as.character(periods)

  return(list(crr_price = crr_price, kr_price = kr_price, rmse_table = rmse_table))
}

# Plots log(RMSE) against log(number of steps) for CRR and KR
plot_rmse_convergence = function(rmse_table, periods, title){
  ggplot(rmse_table) +
    geom_line(mapping = aes(log(periods), log(RMSE_CRR), color = 'CRR')) +
    geom_line(mapping = aes(log(periods), log(RMSE_KR), color = 'KR')) +
    scale_color_manual(values = c('CRR' = 'deeppink4', 'KR' = 'cornflowerblue')) +
    labs(color = '') +
    xlab("Log_Iterations") +
    ylab("Log_RMSE") +
    theme_classic() +
    theme(legend.position = "top") +
    ggtitle(title)
}


######################### EUROPEAN CALL #########################################

bs_call_price = mapply(bs_european_call, current_price, strike_price, maturity, volatility, risk_free_rate)

results_call = compute_rmse(crr_european_call, kr_european_call, bs_call_price, current_price, strike_price,
                             maturity, volatility, risk_free_rate, periods)

write.xlsx(bs_call_price, "european_call_bs.xlsx", colNames = FALSE, rowNames = FALSE)
write.xlsx(results_call$crr_price, "european_call_crr.xlsx", colNames = TRUE, rowNames = FALSE)
write.xlsx(results_call$kr_price, "european_call_kr.xlsx", colNames = TRUE, rowNames = FALSE)
write.xlsx(results_call$rmse_table, "european_call_rmse.xlsx", colNames = TRUE, rowNames = TRUE)

plot_rmse_convergence(results_call$rmse_table, periods, "European Call: RMSE Convergence")


######################### EUROPEAN PUT #########################################

bs_put_price = mapply(bs_european_put, current_price, strike_price, maturity, volatility, risk_free_rate)

results_put = compute_rmse(crr_european_put, kr_european_put, bs_put_price, current_price, strike_price,
                            maturity, volatility, risk_free_rate, periods)

write.xlsx(bs_put_price, "european_put_bs.xlsx", colNames = FALSE, rowNames = FALSE)
write.xlsx(results_put$crr_price, "european_put_crr.xlsx", colNames = TRUE, rowNames = FALSE)
write.xlsx(results_put$kr_price, "european_put_kr.xlsx", colNames = TRUE, rowNames = FALSE)
write.xlsx(results_put$rmse_table, "european_put_rmse.xlsx", colNames = TRUE, rowNames = TRUE)

plot_rmse_convergence(results_put$rmse_table, periods, "European Put: RMSE Convergence")


######################### AMERICAN PUT #########################################

# No closed-form benchmark exists for the American Put, so the Kamrad-Ritchken
# tree with 1000 steps is used as the benchmark instead of Black-Scholes
kr_1000steps_price = mapply(kr_american_put, current_price, strike_price, maturity, 1000, 1.22474, volatility, risk_free_rate)

results_american_put = compute_rmse(crr_american_put, kr_american_put, kr_1000steps_price, current_price, strike_price,
                                     maturity, volatility, risk_free_rate, periods)

write.xlsx(kr_1000steps_price, "american_put_kr1000.xlsx", colNames = FALSE, rowNames = FALSE)
write.xlsx(results_american_put$crr_price, "american_put_crr.xlsx", colNames = TRUE, rowNames = FALSE)
write.xlsx(results_american_put$kr_price, "american_put_kr.xlsx", colNames = TRUE, rowNames = FALSE)
write.xlsx(results_american_put$rmse_table, "american_put_rmse.xlsx", colNames = TRUE, rowNames = TRUE)

plot_rmse_convergence(results_american_put$rmse_table, periods, "American Put: RMSE Convergence")