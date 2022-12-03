# Set working directory
setwd('Documents/Code_Advent/advent_code_1/')

# Read in file 
# Make sure to leave in blanks
input <- read.delim(file = 'input.txt',
                    header = F,
                    blank.lines.skip = F)

# Convert input to vector
input <- as.vector(input$V1)

# Make a blank results vector
results <- c()

# Make count object equal to 0
count <- 0

# For loop, go through each element in input vector
# if count is 0 make the element the count
# if count is greater and there is no NA - add to count
# if count is greater than 0 and there is an NA, put count in results and reset count
for (x in 1:length(input)) {
  
  if (count > 0 & is.na(input[x]) == F) {
    count <- count + input[x]
    
  } else if (count > 0 & is.na(input[x]) == T) {
    results[x] <- count
    count <- 0
    
  } else if (count == 0) {
    count <- input[x]
    
  }
  
}

# Get all the counts 
final <- na.omit(results)

# Answer 1 - Find max value
max(final)

# Answer 2 - Find top 3 values and add together
sum(sort(final, decreasing = T)[1:3])






  
  
  
  
