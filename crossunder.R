# Crossunder function
crossunder <- function(arr1, arr2) {
  n1 <- length(arr1)
  n2 <- length(arr2)
  
  # 1. Check if the length of both arrays is the same
  if (n1 != n2) {
    stop("Both arrays should have the same length")
  }
  
  # 2. Initialize a character vector to store the crossunder signals
  crossunder_signals <- rep("False", n1)
  crossunder_signals[1] <- "None"
  
  # 3. Check for crossunder signals at each data point starting from index 2
  if (n1 >= 2) {
    for (i in 2:n1) {
      if (arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1]) {
        crossunder_signals[i] <- "True"
      } else {
        crossunder_signals[i] <- "False"
      }
    }
  }
  
  # 4. Return the crossunder signals
  return(crossunder_signals)
}
# Sample data for two arrays
arr1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
arr2 <- c(18, 20, 22, 18, 15, 12, 16, 17, 13)

# Calculate crossunder signals between arr1 and arr2
crossunder_signals <- crossunder(arr1, arr2)

# Print results
print(crossunder_signals)