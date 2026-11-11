# Exponential Moving Average (EMA) function
ema <- function(data, period) {
  # 1. Calculate the multiplier for EMA
  multiplier <- 2 / (period + 1)
  
  # 2. Initialize an empty numeric vector to store EMA values
  n <- length(data)
  ema_values <- numeric(n)
  
  # 3. Loop through the data array
  for (i in 1:n) {
    if (i == 1) {
      # 4. Calculate EMA for the first data point (uses the first price)
      ema_values[i] <- data[i]
    } else {
      # 5. Calculate EMA for subsequent data points
      ema_values[i] <- (data[i] - ema_values[i - 1]) * multiplier + ema_values[i - 1]
    }
  }
  
  # 6. Return the calculated values
  return(ema_values)
}
data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
ema_result <- ema(data, period = 3)
print(ema_result)