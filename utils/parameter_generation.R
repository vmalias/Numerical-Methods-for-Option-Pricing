# Parameter greneration, using the uniform distribution.

# We generate sets of random parameters (current price,
# maturity, volatility, risk-free rate) that will be reused across the
# European Call, European Put, and American Put RMSE comparisons.

# We will select an interval for each parameter, such that
# 2000 random values will be generated from it.

# We round to the sixth digit.

library(openxlsx)

n = 2000

# For the current price we use the interval [95, 105]
current_price = round(runif(n, min = 95, max = 105), 6)

# For the maturity we use the interval [0.1, 1]
maturity = round(runif(n, min = 0.1, max = 1), 6)

# For the volatility we use the interval [0.16, 0.45]
volatility = round(runif(n, min = 0.16, max = 0.45), 6)

# For the risk-free rate we use the interval [0, 0.15]
risk_free_rate = round(runif(n, min = 0, max = 0.15), 6)

# Save them in a data frame called 'parameters'.
parameters = data.frame(current_price, maturity, volatility, risk_free_rate)

# Create an excel file for reference
write.xlsx(parameters, "parameters.xlsx", colNames = TRUE, rowNames = FALSE)