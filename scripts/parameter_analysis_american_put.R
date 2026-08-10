# In the following lines of code we conduct the parameter analysis of the Tree-based Models
# of Cox-Ross-Rubinstein and Kamrad-Ritcken for the case of the American Put option. 
# The Trinomial model of Kamrad-Ritcken introduces another parameter called λ. 
# Up until then we fix λ = 1.22474,
# which is the value that Tian (1993) proposed.
# When we examine how the change of a parameter's value affects the price of the European Put,
# we assign the other parameters an indicative value whithin a reasonable range e.g. risk free 
# rate is difficult to reach the level of 20%.

# Load libraries
library(ggplot2)
library(openxlsx)

# Load the functions we are going to use
source("cox_ross_rubinstein_american_put.R")
source("kamrad_ritchken_american_put.R")

# Common Parameters of the Cox-Ross-Rubinstein 
# binomial model and the Kamrad-Ritchken trinomial model. 
# ========================================================================
# strike_price : Strike Price
# maturity : Maturity
# sigma : Volatility of the underlying asset
# risk_free_rate : Risk-free rate                             
# =========================================================================


# Extra parameter used by the iterative Tree-based models.
# ========================================================
# Periods : Number of iterations 
# ========================================================


# Special parameter of the Kamrad-Ritchken trinomial tree.
# ========================================================
# lambda : Parameter λ
# ========================================================

# Create preliminary sequences for different numbers of iterations/periods,
# strike prices and values of the parameter λ.
# We are going to use them throughout the code.
# ================================
Periods = seq(5,100)

strike_price = seq(60,140,1)

lamda  = seq(1.1,2.1,0.2)
# Tian's λ
lamda[2] = 1.22474
# ================================

# ================================================================================
# We need to mention that Black-Scholes formula doesn't work for the case of the 
# American Put. Thus we don't have a "real value" to compare it with the results
# of our lattice models. 

# We consider the model of Kamrad-Ritchken for 1000 iterations as the benchmark 
# model. See Tian (1993).
# ================================================================================

# Matrices initialized with zeros, which for every combination of numbers of periods 
# and strike price (Black-Scholes has no periods) contain the values of:

# 1. Black-Scholes formula
real_value_strike_price = matrix(0, 1, length(strike_price))

# 2. Cox-Ross-Rubinstein binomial model
crr_strike_price = matrix(0, length(Periods), length(strike_price))

# 3. Kamrad-Ritcken trinomial model
kr_strike_price = matrix(0, length(Periods), length(strike_price))

# 4. The distance between the binomial model and the "real value"
diff_crr_strike_price = matrix(0, length(Periods), length(strike_price))

# 5. The distance between the trinomial model and the "real value"
diff_kr_strike_price = matrix(0, length(Periods), length(strike_price))


# In this section we are going to study grafically the behaviour 
# of the models for different values of the strike price (strike_price)
# and number of periods, while fixing the following values:
# ===============================================================
# current_price = 100
# maturity = 1
# volatility = 0.2
# risk_free_rate = 0.05                            
# ===============================================================


# Calculate 1
for (j in seq(1,length(strike_price))) {
  real_value_strike_price[j] = kr_american_put(100,strike_price[j],1,1000,1.22474,0.2,0.05)
}

# Calculate 2-5
for (i in seq(1,length(Periods))) {
  for (j in seq(1,length(strike_price))) {
    crr_strike_price[i,j] = crr_american_put(100,strike_price[j],1,Periods[i],0.2,0.05)
    kr_strike_price[i,j] = kr_american_put(100,strike_price[j],1,Periods[i],1.22474,0.2,0.05)
    diff_crr_strike_price[i,j] = real_value_strike_price[j] - crr_american_put(100,strike_price[j],1,Periods[i],0.2,0.05)
    diff_kr_strike_price[i,j] = real_value_strike_price[j] - kr_american_put(100,strike_price[j],1,Periods[i],1.22474,0.2,0.05)
  }
}

# Apply the corresponding number of periods and strike price
# as row name and column name, respectively.
# ==================================================================================================
set_dimnames = function(mats, rownames_val, colnames_val) {
  lapply(mats, function(m) {
    rownames(m) = as.character(rownames_val)
    colnames(m) = as.character(colnames_val)
    m
  })
}

named = set_dimnames(list(CRRStrikePrice = crr_strike_price, KRStrikePrice = kr_strike_price, DiffCRRStrikePrice = diff_crr_strike_price, DiffKRStrikePrice = diff_kr_strike_price),
                       rownames_val = Periods, colnames_val = strike_price)
crr_strike_price = named$CRRStrikePrice; kr_strike_price = named$KRStrikePrice; diff_crr_strike_price = named$DiffCRRStrikePrice; diff_kr_strike_price = named$DiffKRStrikePrice

# real_value_strike_price has no Periods dimension, so it's separate
colnames(real_value_strike_price) = as.character(strike_price)   
# ==================================================================================================

# Save the values of the quantities we calculated previously in a .xlsx file, which contains
# five different sheets that correspond to each matrix.
data_list = list(CRR_Strike_Price = crr_strike_price, KR_Strike_Price = kr_strike_price, Real_Value_Strike_Price = real_value_strike_price, Diff_CRR_Strike_Price = diff_crr_strike_price, Diff_KR_Strike_Price = diff_kr_strike_price)

write.xlsx(data_list, "results_strike_price_american_put.xlsx", colNames = TRUE, rowNames = TRUE)


# Data frame with distances of both tree-based models from Black-Scholes,
# at the maximum period count, across strike prices
mapping_strike_price <- data.frame(
  Strike_Price = strike_price,
  Diff_CRR_Strike_Price = diff_crr_strike_price[nrow(diff_crr_strike_price), ],
  Diff_KR_Strike_Price = diff_kr_strike_price[nrow(diff_kr_strike_price), ]
)

p <- ggplot(mapping_strike_price) +
  geom_line(aes(x = Strike_Price, y = Diff_KR_Strike_Price, col = "KR")) +
  geom_line(aes(x = Strike_Price, y = Diff_CRR_Strike_Price, col = "CRR")) +
  scale_color_manual(values = c('KR' = 'cornflowerblue', 'CRR' = 'deeppink4')) +
  labs(color = '') +
  xlab("Strike Price") +
  ylab("Distance from the 'real value'") +
  theme_classic() +
  theme(legend.position = "right")

print(p)
ggsave("distance_from_bs_strike_price_american_put.png", plot = p, width = 7, height = 5, dpi = 300)



########################################################################################
########################################################################################

# In this section we are going to study the behaviour of the models for three different
# values of the strike price:

# 1. strike_price = 90 (Out of The Money)
# 2. strike_price = 100 (At The Money)
# 3. strike_price = 110 (In The Money)

# We change the number of periods the lattice models should run and the parameter λ, 
# because we want to paint the full picture of the trinomial model's possibilities.
# We fix the values:
# ===============================================================
# current_price = 100
# maturity = 1
# volatility = 0.2
# risk_free_rate = 0.05                            
# ===============================================================


##################################   Stike Price = 90    #########################################

# Matrices initialized with zeros, which for every combination of numbers of periods 
# and λ contain:

# 1. The distance between the binomial model and the "real value"
diff_crr_strike_price_90 = matrix(0, length(Periods), 1)

# 2. The distance between the trinomial model and the "real value"
diff_kr_strike_price_90 = matrix(0, length(Periods), length(lamda))

# Black-Scholes/"real" value for Strike Price = 90
real_value_strike_price_90 = kr_american_put(100,90,1,1000,1.22474,0.2,0.05)

# Calculate 1 and 2
for (i in seq(1,length(Periods))) {
  diff_crr_strike_price_90[i] = real_value_strike_price_90 - crr_american_put(100,90,1,Periods[i],0.2,0.05)
  for (j in seq(1,length(lamda))) {
    diff_kr_strike_price_90[i,j] = real_value_strike_price_90 - kr_american_put(100,90,1,Periods[i],lamda[j],0.2,0.05)
  }
}

# Declare column and row names
dimnames(diff_kr_strike_price_90) = list(as.character(Periods), as.character(lamda))
dimnames(diff_crr_strike_price_90) = list(as.character(Periods), "CRR_Strike_Price_90")

# Create a data frame that contains diff_crr_strike_price_90 and diff_kr_strike_price_90 and name the columns
# after the values of λ.
mapping_strike_price_90 = data.frame(diff_kr_strike_price_90, diff_crr_strike_price_90)
lamda_labels = c("Lamda_1.1", "Lamda_1.22474", "Lamda_1.5", "Lamda_1.7", "Lamda_1.9", "Lamda_2.1")

colnames(mapping_strike_price_90) = c(lamda_labels, "CRR_Strike_Price_90")
mapping_strike_price_90$Periods = Periods

# One plot per λ, CRR always included for comparison
for (j in seq_along(lamda)) {
  lam_col = lamda_labels[j]
  
  p = ggplot(mapping_strike_price_90) +
    geom_line(aes(x = Periods, y = .data[[lam_col]], col = "KR")) +
    geom_line(aes(x = Periods, y = CRR_Strike_Price_90, col = "CRR")) +
    scale_color_manual(values = c('KR' = 'cornflowerblue', 'CRR' = 'deeppink4')) +
    labs(color = '') +
    xlab("Number of Iterations") +
    ylab("Distance from the 'real value'") +
    theme_classic() +
    theme(legend.position = "right") 

  print(p)
  ggsave(filename = paste0("distance_from_bs_strike_price_90_lambda_", lamda[j], "_american_put.png"), plot = p, width = 7, height = 5, dpi = 300)
}


##################################   Strike Price = 110    #########################################

# Matrices initialized with zeros, which for every combination of numbers of periods 
# and λ contain:

# 1. The distance between the binomial model and the "real value"
diff_crr_strike_price_110 = matrix(0, length(Periods), 1)

# 2. The distance between the trinomial model and the "real value"
diff_kr_strike_price_110 = matrix(0, length(Periods), length(lamda))

# Black-Scholes/"real" value for Strike Price = 110
real_value_strike_price_110 = kr_american_put(100,110,1,1000,1.22474,0.2,0.05)

# Calculate 1 and 2
for (i in seq(1,length(Periods))) {
  diff_crr_strike_price_110[i] = real_value_strike_price_110 - crr_american_put(100,110,1,Periods[i],0.2,0.05)
  for (j in seq(1,length(lamda))) {
    diff_kr_strike_price_110[i,j] = real_value_strike_price_110 - kr_american_put(100,110,1,Periods[i],lamda[j],0.2,0.05)
  }
}

# Declare column and row names
dimnames(diff_kr_strike_price_110) = list(as.character(Periods), as.character(lamda))
dimnames(diff_crr_strike_price_110) = list(as.character(Periods), "CRR_Strike_Price_110")

# Create a data frame that contains diff_crr_strike_price_110 and diff_kr_strike_price_110
# and name the columns after the values of λ.
mapping_strike_price_110 = data.frame(diff_kr_strike_price_110, diff_crr_strike_price_110)

colnames(mapping_strike_price_110) = c(lamda_labels, "CRR_Strike_Price_110")
mapping_strike_price_110$Periods = Periods

# One plot per λ, CRR always included for comparison
for (j in seq_along(lamda)) {
  lam_col = lamda_labels[j]
  
  p = ggplot(mapping_strike_price_110) +
    geom_line(aes(x = Periods, y = .data[[lam_col]], col = "KR")) +
    geom_line(aes(x = Periods, y = CRR_Strike_Price_110, col = "CRR")) +
    scale_color_manual(values = c('KR' = 'cornflowerblue', 'CRR' = 'deeppink4')) +
    labs(color = '') +
    xlab("Number of Iterations") +
    ylab("Distance from the 'real value'") +
    theme_classic() +
    theme(legend.position = "right") 

  print(p)
  ggsave(filename = paste0("distance_from_bs_strike_price_110_lambda_", lamda[j], "_american_put.png"), plot = p, width = 7, height = 5, dpi = 300)
}


##################################   Strike Price = 100    #########################################

# Matrices initialized with zeros, which for every combination of numbers of periods 
# and λ contain:

# 1. The distance between the binomial model and the "real value"
diff_crr_strike_price_100 = matrix(0, length(Periods), 1)

# 2. The distance between the trinomial model and the "real value"
diff_kr_strike_price_100 = matrix(0, length(Periods), length(lamda))

# Black-Scholes/"real" value for Strike Price = 100
real_value_strike_price_100 = kr_american_put(100,100,1,1000,1.22474,0.2,0.05)

# Calculate 1 and 2
for (i in seq(1,length(Periods))) {
  diff_crr_strike_price_100[i] = real_value_strike_price_100 - crr_american_put(100,100,1,Periods[i],0.2,0.05)
  for (j in seq(1,length(lamda))) {
    diff_kr_strike_price_100[i,j] = real_value_strike_price_100 - kr_american_put(100,100,1,Periods[i],lamda[j],0.2,0.05)
  }
}

# Declare column and row names
dimnames(diff_kr_strike_price_100) = list(as.character(Periods), as.character(lamda))
dimnames(diff_crr_strike_price_100) = list(as.character(Periods), "CRR_Strike_Price_100")

# Create a data frame that contains diff_crr_strike_price_100 and diff_kr_strike_price_100 and name the columns
# after the values of λ.
mapping_strike_price_100 = data.frame(diff_kr_strike_price_100, diff_crr_strike_price_100)

colnames(mapping_strike_price_100) = c(lamda_labels, "CRR_Strike_Price_100")
mapping_strike_price_100$Periods = Periods

# One plot per λ, CRR always included for comparison
for (j in seq_along(lamda)) {
  lam_col = lamda_labels[j]
  
  p = ggplot(mapping_strike_price_100) +
    geom_line(aes(x = Periods, y = .data[[lam_col]], col = "KR")) +
    geom_line(aes(x = Periods, y = CRR_Strike_Price_100, col = "CRR")) +
    scale_color_manual(values = c('KR' = 'cornflowerblue', 'CRR' = 'deeppink4')) +
    labs(color = '') +
    xlab("Number of Iterations") +
    ylab("Distance from the 'real value'") +
    theme_classic() +
    theme(legend.position = "right") 

  print(p)
  ggsave(filename = paste0("distance_from_bs_strike_price_100_lambda_", lamda[j], "_american_put.png"), plot = p, width = 7, height = 5, dpi = 300)
}


############################ Odd-Even Steps ##############################################

# In this subsection we are going to study the odd-even steps of the CRR model

# Extract the even steps
even_steps = Periods[Periods %% 2 == 0]
# Extract the odd steps
odd_steps  = Periods[Periods %% 2 == 1]

# Assign the values for the cases: 1. ATM, 2. ITM and 3. OTM
values = c(ATM = 100, ITM = 110, OTM = 90)
# Assign the titles for the cases: 1. ATM, 2. ITM and 3. OTM
titles  = c(ATM = "Even-Odd Steps European Put (ATM)",
            ITM = "Even-Odd Steps European Put (ITM)",
            OTM = "Even-Odd Steps European Put (OTM)")

# Create an empty list
all_kinds_of_steps = list()

for (nm in names(values)) {
  # Strike price
  strike_price_even_odd_steps = values[[nm]]
  
  # Black-Scholes formula for every strike price 
  real_value_odd_even_steps = kr_american_put(100,strike_price_even_odd_steps,1,1000,1.22474,0.2,0.05)

  # Difference of the CRR from the "real" value for even steps
  diff_crr_even_steps = sapply(even_steps, function(n) real_value_odd_even_steps - crr_american_put(100, strike_price_even_odd_steps, 1, n, 0.2, 0.05))
  names(diff_crr_even_steps) = as.character(even_steps)
  
  # Difference of the CRR from the "real" value for odd steps
  diff_crr_odd_steps = sapply(odd_steps, function(n) real_value_odd_even_steps - crr_american_put(100, strike_price_even_odd_steps, 1, n, 0.2, 0.05))
  names(diff_crr_odd_steps) = as.character(odd_steps)
  
  # All steps together
  diff_crr_all_steps = sapply(Periods, function(n) real_value_odd_even_steps - crr_american_put(100, strike_price_even_odd_steps, 1, n, 0.2, 0.05))
  names(diff_crr_all_steps) = as.character(Periods)

  # Save the above quantities in a list
  all_kinds_of_steps[[nm]] = list(evens = diff_crr_even_steps, odds = diff_crr_odd_steps, all = diff_crr_all_steps)
  
  # Plot the results
  p = ggplot() +
    geom_line(aes(even_steps, diff_crr_even_steps), col = "cornflowerblue") +
    geom_line(aes(odd_steps, diff_crr_odd_steps), col = "deeppink4") +
    geom_line(aes(Periods, diff_crr_all_steps), col = "darkolivegreen4") +
    xlab("Number of Iterations") +
    ylab("Distance from the 'real value'") +
    theme_classic() +
    ggtitle(titles[[nm]])
  
  print(p)
  ggsave(paste0("distance_form_bs_crr_even-odd_steps_", nm, "_american_put.png"), plot = p, width = 7, height = 5, dpi = 300)
}

#########################################################################################
#########################################################################################

# In this section we are going to study the behaviour of the models
# for different values of the volatility factor (σ):

volatility = seq(0.01, 0.5, 0.01)

# We change the number of periods the lattice models should run, while this time we
# fix the parameter λ for simplicity reasons. 
# Furthermore we fix the values:
# ===============================================================
# current_price = 100
# strike_price = 100 (ATM)
# maturity = 1
# risk_free_rate = 0.05                            
# ===============================================================


################################### Volatility ##################################

# Matrices initialized with zeros, which for every combination of numbers of periods 
# and strike price contain the values of:

# 1. Black-Scholes formula
real_value_volatility = matrix(0, 1, length(volatility))

# 2. Cox-Ross-Rubinstein binomial model
crr_volatility = matrix(0, length(Periods), length(volatility))

# 3. Kamrad-Ritcken trinomial model
kr_volatility = matrix(0, length(Periods), length(volatility))

# 4. The distance between the binomial model and the "real value"
diff_crr_volatility = matrix(0, length(Periods), length(volatility))

# 5. The distance between the trinomial model and the "real value"
diff_kr_volatility = matrix(0, length(Periods), length(volatility))

# Calculate 1
for (j in seq(1,length(volatility))) {
  real_value_volatility[j] = kr_american_put(100,100,1,1000,1.22474,volatility[j],0.05)
}

# Calculate 2-5
for (i in seq(1,length(Periods))) {
  for (j in seq(1,length(volatility))) {
    crr_volatility[i,j] = crr_american_put(100,100,1,Periods[i],volatility[j],0.05)
    kr_volatility[i,j] = kr_american_put(100,100,1,Periods[i],1.22474,volatility[j],0.05)
    diff_crr_volatility[i,j] = real_value_volatility[j] - crr_american_put(100,100,1,Periods[i],volatility[j],0.05)
    diff_kr_volatility[i,j] = real_value_volatility[j] - kr_american_put(100,100,1,Periods[i],1.22474,volatility[j],0.05)
  }
}

named = set_dimnames(list(CRRVolatility = crr_volatility, KRVolatility = kr_volatility, DiffCRRVolatility = diff_crr_volatility, DiffKRVolatility = diff_kr_volatility),
                       rownames_val = Periods, colnames_val = volatility)
crr_volatility = named$CRRVolatility; kr_volatility = named$KRVolatility; diff_crr_volatility = named$DiffCRRVolatility; diff_kr_volatility = named$DiffKRVolatility

# real_value_volatility has no Periods dimension, so it's separate
colnames(real_value_volatility) = as.character(volatility)   
# ==================================================================================================

# Save the values of the quantities we calculated previously in a .xlsx file, which contains
# five different sheets that correspond to each matrix.
data_list = list(CRR_Volatility = crr_volatility, KR_Volatility = kr_volatility, Real_Value_Volatility = real_value_volatility, Diff_CRR_Volatility = diff_crr_volatility, Diff_KR_Volatility = diff_kr_volatility)

write.xlsx(data_list, "results_volatility_american_put.xlsx", colNames = TRUE, rowNames = TRUE)

# Data frame with distances of both tree-based models from Black-Scholes,
# at the maximum period count, across strike prices
mapping_volatility <- data.frame(
  Volatility = volatility,
  Diff_CRR_Volatility = diff_crr_volatility[nrow(diff_crr_volatility), ],
  Diff_KR_Volatility = diff_kr_volatility[nrow(diff_kr_volatility), ]
)

p <- ggplot(mapping_volatility) +
  geom_line(aes(x = Volatility, y = Diff_KR_Volatility, col = "KR")) +
  geom_line(aes(x = Volatility, y = Diff_CRR_Volatility, col = "CRR")) +
  scale_color_manual(values = c('KR' = 'cornflowerblue', 'CRR' = 'deeppink4')) +
  labs(color = '') +
  xlab("Volatility") +
  ylab("Distance from the 'real value'") +
  theme_classic() +
  theme(legend.position = "right")

print(p)
ggsave("distance_from_bs_volatility_american_put.png", plot = p, width = 7, height = 5, dpi = 300)


#########################################################################################
#########################################################################################

# In this section we are going to study the behaviour of the models
# for different values of the risk free rate:

risk_free_rate = seq(0, 0.15, 0.01)

# We change the number of number of iterations.
# We fix the parameter λ for simplicity reasons 
# and the following values:
# ===============================================================
# current_price = 100
# strike_price = 100 (ATM)
# maturity = 1
# volatility = 0.2                         
# ===============================================================


################################### Risk Free Rate ##################################

# Matrices initialized with zeros, which for every combination of numbers of periods 
# and strike price contain the values of:

# 1. Black-Scholes formula
real_value_risk_free_rate = matrix(0, 1, length(risk_free_rate))

# 2. Cox-Ross-Rubinstein binomial model
crr_risk_free_rate = matrix(0, length(Periods), length(risk_free_rate))

# 3. Kamrad-Ritcken trinomial model
kr_risk_free_rate = matrix(0, length(Periods), length(risk_free_rate))

# 4. The distance between the binomial model and the "real value"
diff_crr_risk_free_rate = matrix(0, length(Periods), length(risk_free_rate))

# 5. The distance between the trinomial model and the "real value"
diff_kr_risk_free_rate = matrix(0, length(Periods), length(risk_free_rate))

# Calculate 1
for (j in seq(1,length(risk_free_rate))) {
  real_value_risk_free_rate[j] = kr_american_put(100,100,1,1000,1.22474,0.2,risk_free_rate[j])
}

# Calculate 2-5
for (i in seq(1,length(Periods))) {
  for (j in seq(1,length(risk_free_rate))) {
    crr_risk_free_rate[i,j] = crr_american_put(100,100,1,Periods[i],0.2,risk_free_rate[j])
    kr_risk_free_rate[i,j] = kr_american_put(100,100,1,Periods[i],1.22474,0.2,risk_free_rate[j])
    diff_crr_risk_free_rate[i,j] = real_value_risk_free_rate[j] - crr_american_put(100,100,1,Periods[i],0.2,risk_free_rate[j])
    diff_kr_risk_free_rate[i,j] = real_value_risk_free_rate[j] - kr_american_put(100,100,1,Periods[i],1.22474,0.2,risk_free_rate[j])
  }
}

named = set_dimnames(list(CRRRiskFreeRate = crr_risk_free_rate, KRRiskFreeRate = kr_risk_free_rate, DiffCRRRiskFreeRate = diff_crr_risk_free_rate, DiffKRRiskFreeRate = diff_kr_risk_free_rate),
                       rownames_val = Periods, colnames_val = risk_free_rate)
crr_risk_free_rate = named$CRRRiskFreeRate; kr_risk_free_rate = named$KRRiskFreeRate; diff_crr_risk_free_rate = named$DiffCRRRiskFreeRate; diff_kr_risk_free_rate = named$DiffKRRiskFreeRate

# real_value_risk_free_rate has no Periods dimension, so it's separate
colnames(real_value_risk_free_rate) = as.character(risk_free_rate)   
# ==================================================================================================

# Save the values of the quantities we calculated previously in a .xlsx file, which contains
# five different sheets that correspond to each matrix.
data_list = list(CRR_Risk_Free_Rate = crr_risk_free_rate, KR_Risk_Free_Rate = kr_risk_free_rate, Real_Value_Risk_Free_Rate = real_value_risk_free_rate, Diff_CRR_Risk_Free_Rate = diff_crr_risk_free_rate, Diff_KR_Risk_Free_Rate = diff_kr_risk_free_rate)

write.xlsx(data_list, "results_risk_free_rate_american_put.xlsx", colNames = TRUE, rowNames = TRUE)

# Data frame with distances of both tree-based models from Black-Scholes,
# at the maximum period count, across strike prices
mapping_risk_free_rate <- data.frame(
  Risk_Free_Rate = risk_free_rate,
  Diff_CRR_Risk_Free_Rate = diff_crr_risk_free_rate[nrow(diff_crr_risk_free_rate), ],
  Diff_KR_Risk_Free_Rate = diff_kr_risk_free_rate[nrow(diff_kr_risk_free_rate), ]
)

p <- ggplot(mapping_risk_free_rate) +
  geom_line(aes(x = Risk_Free_Rate, y = Diff_KR_Risk_Free_Rate, col = "KR")) +
  geom_line(aes(x = Risk_Free_Rate, y = Diff_CRR_Risk_Free_Rate, col = "CRR")) +
  scale_color_manual(values = c('KR' = 'cornflowerblue', 'CRR' = 'deeppink4')) +
  labs(color = '') +
  xlab("Risk free rate") +
  ylab("Distance from the 'real value'") +
  theme_classic() +
  theme(legend.position = "right")

print(p)
ggsave("distance_from_bs_risk_free_rate_american_put.png", plot = p, width = 7, height = 5, dpi = 300)


#########################################################################################
#########################################################################################

# In this section we are going to study the behaviour of the models
# for different maturities:

maturity = seq(0.01, 1, 0.01)

# We change the number of number of iterations.
# We fix the parameter λ for simplicity reasons 
# and the following values:
# ===============================================================
# current_price = 100
# strike_price = 100 (ATM)
# risk_free_rate = 0.05
# volatility = 0.2                         
# ===============================================================


################################### Maturity ##################################

# Matrices initialized with zeros, which for every combination of numbers of periods 
# and strike price (Black-Scholes has no periods) contain the values of:

# 1. Black-Scholes formula
real_value_maturity = matrix(0, 1, length(maturity))

# 2. Cox-Ross-Rubinstein binomial model
crr_maturity = matrix(0, length(Periods), length(maturity))

# 3. Kamrad-Ritcken trinomial model
kr_maturity = matrix(0, length(Periods), length(maturity))

# 4. The distance between the binomial model and the "real value"
diff_crr_maturity = matrix(0, length(Periods), length(maturity))

# 5. The distance between the trinomial model and the "real value"
diff_kr_maturity = matrix(0, length(Periods), length(maturity))

# Calculate 1
for (j in seq(1,length(maturity))) {
  real_value_maturity[j] = kr_american_put(100,100,maturity[j],1000,1.22474,0.2,0.05)
}

# Calculate 2-5
for (i in seq(1,length(Periods))) {
  for (j in seq(1,length(maturity))) {
    crr_maturity[i,j] = crr_american_put(100,100,maturity[j],Periods[i],0.2,0.05)
    kr_maturity[i,j] = kr_american_put(100,100,maturity[j],Periods[i],1.22474,0.2,0.05)
    diff_crr_maturity[i,j] = real_value_maturity[j] - crr_american_put(100,100,maturity[j],Periods[i],0.2,0.05)
    diff_kr_maturity[i,j] = real_value_maturity[j] - kr_american_put(100,100,maturity[j],Periods[i],1.22474,0.2,0.05)
  }
}

named = set_dimnames(list(CRRMaturity = crr_maturity, KRMaturity = kr_maturity, DiffCRRMaturity = diff_crr_maturity, DiffKRMaturity = diff_kr_maturity),
                       rownames_val = Periods, colnames_val = maturity)
crr_maturity = named$CRRMaturity; kr_maturity = named$KRMaturity; diff_crr_maturity = named$DiffCRRMaturity; diff_kr_maturity = named$DiffKRMaturity

# real_value_maturity has no Periods dimension, so it's separate
colnames(real_value_maturity) = as.character(maturity)   
# ==================================================================================================

# Save the values of the quantities we calculated previously in a .xlsx file, which contains
# five different sheets that correspond to each matrix.
data_list = list(CRR_Maturity = crr_maturity, KR_Maturity = kr_maturity, Real_Ralue_Maturity = real_value_maturity, Diff_CRR_Maturity = diff_crr_maturity, Diff_KR_Maturity = diff_kr_maturity)

write.xlsx(data_list, "results_maturity_american_put.xlsx", colNames = TRUE, rowNames = TRUE)

# Data frame with distances of both tree-based models from Black-Scholes,
# at the maximum period count, across strike prices
mapping_maturity <- data.frame(
  Maturity = maturity,
  Diff_CRR_Maturity = diff_crr_maturity[nrow(diff_crr_maturity), ],
  Diff_KR_Maturity = diff_kr_maturity[nrow(diff_kr_maturity), ]
)

p <- ggplot(mapping_maturity) +
  geom_line(aes(x = Maturity, y = Diff_KR_Maturity, col = "KR")) +
  geom_line(aes(x = Maturity, y = Diff_CRR_Maturity, col = "CRR")) +
  scale_color_manual(values = c('KR' = 'cornflowerblue', 'CRR' = 'deeppink4')) +
  labs(color = '') +
  xlab("Maturity") +
  ylab("Distance from the 'real value'") +
  theme_classic() +
  theme(legend.position = "right")

print(p)
ggsave("distance_from_bs_maturity_american_put.png", plot = p, width = 7, height = 5, dpi = 300)