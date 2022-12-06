# Set working directory
setwd('Documents/Code_Advent/advent_code_6/')

# Read input and seperate the string into a vector - each character as an element
input <- as.vector(read.delim('input', header = F))[[1]]

# Seperate string into vector
input_v <- strsplit(input, '')[[1]]

# Find unique chunks of 4
# Use for loop to go through each chunk in a sliding window pattern
results <- c()
for (n in 1:(length(input_v) - 3)) {
  
  if (length(unique(input_v[n:(n + 3)])) == 4) {
    results[n] <- n + 3
  } else {
    results[n] <- NA
  }
}

# Answer 1 - Return the first result that is not NA
na.omit(results)[1]

# PART 2 

# Find unique chunks of 14
# Use for loop to go through each chunk in a sliding window pattern
results <- c()
for (n in 1:(length(input_v) - 13)) {
  
  if (length(unique(input_v[n:(n + 13)])) == 14) {
    results[n] <- n + 13
  } else {
    results[n] <- NA
  }
}

# Answer 1 - Return the first result that is not NA
na.omit(results)[1]


