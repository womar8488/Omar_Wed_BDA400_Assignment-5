# Standard Deviation (stdev) function
stdev <- function(data) {
  # 1. Calculate the mean of the data
  mean_value <- mean(data)
  
  # 2. Calculate the differences between the data points and the mean
  diff_values <- data - mean_value
  
  # 3. Calculate the squared differences
  squared_diff <- diff_values^2
  
  # 4. Calculate the variance (mean of squared differences)
  variance <- sum(squared_diff) / length(data)
  
  # 5. Calculate the standard deviation (square root of the variance)
  standard_deviation <- sqrt(variance)
  
  # 6. Return the standard deviation value
  return(standard_deviation)
}
data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
stdev_result <- stdev(data)
print(stdev_result)