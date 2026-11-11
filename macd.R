# Moving Average Convergence Divergence (MACD) function
macd <- function(data, short_period, long_period, signal_period) {
  # 1. Calculate the short-term and long-term exponential moving averages (EMA)
  short_ema <- ema(data, short_period)
  long_ema <- ema(data, long_period)
  
  # 2. Calculate the MACD line
  macd_line <- short_ema - long_ema
  
  # 3. Calculate the signal line (EMA of the MACD line)
  signal_line <- ema(macd_line, signal_period)
  
  # 4. Calculate the histogram (the difference between the MACD line and the signal line)
  histogram <- macd_line - signal_line
  
  # 5. Return the MACD line, signal line, and histogram as a list
  result <- list(
    macd_line = macd_line,
    signal_line = signal_line,
    histogram = histogram
  )
  
  return(result)
}
# Sample data from assignment
data <- c(100, 105, 110, 115, 120, 125, 130)

# Calculate MACD
macd_result <- macd(data, short_period = 3, long_period = 5, signal_period = 2)

# Print results
print(macd_result)