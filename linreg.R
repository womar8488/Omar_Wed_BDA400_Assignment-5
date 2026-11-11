# Linear Regression (linreg) function
linreg <- function(regressionSource, regressionLength, regressionOffset) {
  n <- length(regressionSource)
  
  # 1. Check if regressionLength is greater than the number of elements in regressionSource
  if (regressionLength > n) {
    stop("regressionLength cannot be greater than the number of elements in regressionSource")
  }
  
  # 2. Check if regressionOffset is greater than or equal to regressionLength
  if (regressionOffset >= regressionLength) {
    stop("regressionOffset must be less than regressionLength")
  }
  
  # 3. Calculate start and end indices for the subset window
  start_index <- max(1, n - regressionLength + regressionOffset)
  end_index <- min(n, n - regressionOffset)
  
  # 4. Extract the relevant portion of regressionSource
  source_subset <- regressionSource[start_index:end_index]
  
  # 5. Calculate the index values for the regression points
  index_values <- 1:length(source_subset)
  
  # 6. Calculate means
  mean_index <- mean(index_values)
  mean_source <- mean(source_subset)
  
  # 7. Calculate the numerator and denominator for the linear regression formula
  numerator <- sum((index_values - mean_index) * (source_subset - mean_source))
  denominator <- sum((index_values - mean_index)^2)
  
  # 8. Calculate the slope and intercept of the linear regression line
  slope <- numerator / denominator
  intercept <- mean_source - slope * mean_index
  
  # 9. Calculate the predicted values for the source subset
  predicted_values <- slope * index_values + intercept
  
  # 10. Return slope, intercept, and predicted values as a list
  result <- list(
    slope = slope,
    intercept = intercept,
    predicted_values = predicted_values
  )
  
  return(result)
}
data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
linreg_result <- linreg(regressionSource = data, regressionLength = 5, regressionOffset = 0)
print(linreg_result)