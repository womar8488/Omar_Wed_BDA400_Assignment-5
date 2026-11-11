# Stochastic RSI (StochRSI) function
stoch_rsi <- function(data, period, k_period, d_period) {
  # 1. Calculate the RSI
  rsi_values <- rsi(data, period)
  
  # 2. Calculate the StochRSI (normalized RSI values, ignoring NAs)
  min_rsi <- min(rsi_values, na.rm = TRUE)
  max_rsi <- max(rsi_values, na.rm = TRUE)
  k_values <- (rsi_values - min_rsi) / (max_rsi - min_rsi)
  
  # 3. Calculate the %K line (SMA of StochRSI values)
  k_line <- sma(k_values, k_period)
  
  # 4. Calculate the %D line (SMA of %K line)
  d_line <- sma(k_line, d_period)
  
  # 5. Return the %K and %D lines as a list
  result <- list(
    k_line = k_line,
    d_line = d_line
  )
  
  return(result)
}
# Test dataset
data <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62)
stoch_rsi_result <- stoch_rsi(data, period = 5, k_period = 3, d_period = 3)
print(stoch_rsi_result)