# Set working directory 
setwd('Documents/Code_Advent/advent_code_4/')

# Load input
input <- as.vector(read.delim('input', header = F))[[1]]

# Create empty vector for results to go into
results1 <- c() 

# for loop to iterate through rows of input
for (n in 1:length(input)) {
  
  # Extract the first range from the nth pair and create a vector containing numbers in the range
  range1 <- strsplit(input[n], ',')[[1]][1]
  start1 <- as.numeric(strsplit(range1, '-')[[1]][1])
  end1 <- as.numeric(strsplit(range1, '-')[[1]][2])
  vector1 <- c(start1:end1)
  
  # Extract the second range from the nth pair and create a vector containing numbers in the range
  range2 <- strsplit(input[n], ',')[[1]][2]
  start2 <- as.numeric(strsplit(range2, '-')[[1]][1])
  end2 <- as.numeric(strsplit(range2, '-')[[1]][2])
  vector2 <- c(start2:end2)
  
  # if length of intersect is same as length of min length of two vectors
  # then one range is contained within another therefore put 1 in results 1
  if (length(intersect(vector1, vector2)) == min(length(vector1), length(vector2))) {
    results1[n] <- 1
    
    # if not put 0 in results1
  } else {
    results1[n] <- 0
  }
}

# Answer 1 - find sum of results
sum(results1)

# PART 2 

# Find the number of pairs that any overlap whether it is complete or not
# Create empty vector for the results to go in
results2 <- c()

# for loop to iterate through rows of input
for (n in 1:length(input)) {
  
  # Extract the first range from the nth pair and create a vector containing numbers in the range
  range1 <- strsplit(input[n], ',')[[1]][1]
  start1 <- as.numeric(strsplit(range1, '-')[[1]][1])
  end1 <- as.numeric(strsplit(range1, '-')[[1]][2])
  vector1 <- c(start1:end1)
  
  # Extract the second range from the nth pair and create a vector containing numbers in the range
  range2 <- strsplit(input[n], ',')[[1]][2]
  start2 <- as.numeric(strsplit(range2, '-')[[1]][1])
  end2 <- as.numeric(strsplit(range2, '-')[[1]][2])
  vector2 <- c(start2:end2)
  
  # if length of intersect is same as length of min length of two vectors
  # then one range is contained within another therefore put 1 in results 1
  if (length(intersect(vector1, vector2)) > 0) {
    results2[n] <- 1
    
    # if not put 0 in results1
  } else {
    results2[n] <- 0
  }
}

# Answer 2 - find sum of results2
sum(results2)



