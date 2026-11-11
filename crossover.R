# Crossover function
crossover <- function(arr1, arr2) {
  n1 <- length(arr1)
  n2 <- length(arr2)
  
  # 1. Check if the length of both arrays is the same
  if (n1 != n2) {
    stop("Both arrays should have the same length")
  }
  
  # 2. Initialize a character vector to store the crossover signals with "None"
  crossover_signals <- rep("None", n1)
  
  # 3. Check for crossovers at each data point starting from index 2
  if (n1 >= 2) {
    for (i in 2:n1) {
      if (arr1[i] > arr2[i] && arr1[i - 1] <= arr2[i - 1]) {
        crossover_signals[i] <- "Up"
      } else if (arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1]) {
        crossover_signals[i] <- "Down"
      } else {
        crossover_signals[i] <- "None"
      }
    }
  }
  
  # 4. Return the crossover signals
  return(crossover_signals)
}
# Sample data for two arrays
arr1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
arr2 <- c(18, 20, 22, 18, 15, 12, 16, 17, 13)

# Calculate crossover signals between arr1 and arr2
crossover_signals <- crossover(arr1, arr2)

# Print results
print(crossover_signals)