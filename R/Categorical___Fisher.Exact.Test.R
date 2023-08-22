Categorical___Fisher.Exact = function(data_matrix) {
  # Check if the input is a 2x2 matrix
  if (dim(data_matrix)[1] != 2 | dim(data_matrix)[2] != 2) {
    stop("Please provide a 2x2 matrix or table.")
  }

  # Conduct Fisher's exact test
  result = fisher.test(data_matrix)

  # Return the result
  return(result)
}

# Example usage:
# Let's say you have the following data:
#             Disease    No Disease
# Treatment      5          15
# No Treatment   1          20
data_matrix <- matrix(c(5, 1, 15, 20), nrow=2)
colnames(data_matrix) <- c("Disease", "No Disease")
rownames(data_matrix) <- c("Treatment", "No Treatment")

# Conduct the test
Categorical___Fisher.Exact(data_matrix)
