# It is known that put-call parity (call price - put price
# equals current price minus the discounted strike price) 
# holds for the Black-Scholes formula and the Cox-Ross-Rubinstein 
# binomial tree. Here, we check if the put-call parity tends to hold 
# for the Kamrad-Ritchken tree as well, as the number of iterations increases.

library(ggplot2)

# Load the functions we are going to employ
source("black_scholes_european_call.R")
source("black_scholes_european_put.R")
source("cox_ross_rubinstein_european_call.R")
source("cox_ross_rubinstein_european_put.R")
source("kamrad_ritchken_european_call.R")
source("kamrad_ritchken_european_put.R")

# Create a sequence for different numbers of iterations
periods = seq(5, 300)

# Matrices initialized with zeros, which will hold each tree's put-call difference
kr_put_call_parity = matrix(0, length(periods), 1)
crr_put_call_parity = matrix(0, length(periods), 1)

# Black-Scholes put-call parity
bs_put_call_parity = matrix(bs_european_call(100, 100, 1, 0.2, 0.05) - bs_european_put(100, 100, 1, 0.2, 0.05),
                             length(periods), 1)

# Calculate the difference call price - put price for the binomial and trinomial tree
for (i in seq_along(periods)){
  kr_put_call_parity[i] = kr_european_call(100, 100, 1, periods[i], 1.22474, 0.2, 0.05) -
                           kr_european_put(100, 100, 1, periods[i], 1.22474, 0.2, 0.05)

  crr_put_call_parity[i] = crr_european_call(100, 100, 1, periods[i], 0.2, 0.05) -
                            crr_european_put(100, 100, 1, periods[i], 0.2, 0.05)
}

# Combine all three series into a single data frame for plotting
put_call_mapping = data.frame(kr_put_call_parity, crr_put_call_parity, bs_put_call_parity)
rownames(put_call_mapping) = as.character(periods)
colnames(put_call_mapping) = c("KR", "CRR", "BS")

# Plot the the difference call price - put price for each case
p = ggplot(put_call_mapping) +
  geom_line(mapping = aes(periods, KR, color = 'KR'), linewidth = 1) +
  geom_line(mapping = aes(periods, CRR, color = 'CRR'), linewidth = 1) +
  geom_line(mapping = aes(periods, BS, color = 'Black Scholes'), linewidth = 1.7, alpha = 0.5, linetype = "dashed") +
  scale_color_manual(values = c('KR' = 'deeppink4', 'CRR' = 'darkolivegreen4', 'Black Scholes' = 'steelblue4')) +
  labs(color = '') +
  xlab("Number of Iterations") +
  ylab("Put-Call Parity") +
  theme_classic() +
  theme(legend.position = "right") 

print(p)
ggsave("put_call_parity.png", plot = p, width = 7, height = 5, dpi = 300)
