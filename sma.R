sma <- function(data, period) {
  # 1. Check if the length of data is less than the specified period
  if (length(data) < period) {
    stop("Error: Data length is shorter than the specified period.")
  }
  
  # 2. Initialize a vector to store the SMA values
  n <- length(data)
  sma_values <- numeric(n - period + 1)
  
  # 3. Calculate SMA for each window of period data points
  for (i in 1:(n - period + 1)) {
    window_data <- data[i:(i + period - 1)]
    sma_values[i] <- sum(window_data) / period
  }
  
  # 4. Return the calculated values
  return(sma_values)
}
# Sample data from assignment
data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)

# Calculate SMA with period of 3
sma_result <- sma(data, period = 3)

# Print results
print(sma_result)
