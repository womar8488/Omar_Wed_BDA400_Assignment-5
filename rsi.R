# Relative Strength Index (RSI) function
rsi <- function(data, period) {
  n <- length(data)
  
  # 1. Calculate price differences between consecutive data points
  diff_values <- diff(data)
  
  # 2. Initialize vectors to store gains and losses
  gains <- numeric(length(diff_values))
  losses <- numeric(length(diff_values))
  
  # 3. Calculate gains and losses
  for (i in 1:length(diff_values)) {
    if (diff_values[i] >= 0) {
      gains[i] <- diff_values[i]
      losses[i] <- 0
    } else {
      gains[i] <- 0
      losses[i] <- abs(diff_values[i])
    }
  }
  
  # 4. Calculate initial average gains and losses for the first 'period' elements
  avg_gain <- mean(gains[1:period])
  avg_loss <- mean(losses[1:period])
  
  # 5. Initialize RSI vector with NA values
  rsi_values <- rep(NA, n)
  
  # Calculate initial RSI at index (period + 1)
  if (avg_loss == 0) {
    rsi_values[period + 1] <- 100
  } else {
    rs <- avg_gain / avg_loss
    rsi_values[period + 1] <- 100 - (100 / (1 + rs))
  }
  
  # 6. Calculate subsequent RSI values using Wilder's smoothing method
  if ((period + 2) <= n) {
    for (i in (period + 2):n) {
      avg_gain <- (avg_gain * (period - 1) + gains[i - 1]) / period
      avg_loss <- (avg_loss * (period - 1) + losses[i - 1]) / period
      
      if (avg_loss == 0) {
        rsi_values[i] <- 100
      } else {
        rs <- avg_gain / avg_loss
        rsi_values[i] <- 100 - (100 / (1 + rs))
      }
    }
  }
  
  # 7. Return the RSI values
  return(rsi_values)
}
# Test dataset from assignment
data <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62)

# Calculate RSI with period = 5
rsi_result <- rsi(data, period = 5)

# Print the resulting RSI values
print(rsi_result)