# Set working directory
setwd('Documents/Code_Advent/advent_code_3/')

# Create vectors of letters and corresponding numbers
kvp <- data.frame(letters = c(letters, LETTERS),
                  nums = c(1:52))

# Read input
input <- read.delim('input', header = F)

# Make empty vetcor for values to go in
all_values <- c()

# For loop to iterate through rows of input
for (x in 1:nrow(input)) {
  
  # Extract first half of string and put in vector
  compartment1 <- 
    substring(input$V1[x], 1, nchar(input$V1[x]) / 2)
  vector1 <- strsplit(compartment1, '')[[1]]
  
  # Extract second half of string and put in vector
  compartment2 <-
    substring(input$V1[x], (nchar(input$V1[x]) / 2) + 1, nchar(input$V1[x]))
  vector2 <- strsplit(compartment2, '')[[1]]
  
  # Find overlap
  item <- intersect(vector1, vector2)
  
  # Lookup
  value <- kvp$nums[kvp$letters == item]
  
  # Store
  all_values[x] <- value
  
}

# Answer 1 - find sum of all values 
sum(all_values)

# PART 2

# Find common item between each three lines

# Vector to put results in
group_values <- c(1:(nrow(input)/3))

# for loop to iterate through groups
for (n in 1:(nrow(input)/3)) {
  
  # Extract string of first elf and put in vector
  elf1 <- unique(strsplit(input$V1[(3*n)-2], '')[[1]])
  
  # Extract string of second elf and put in vector
  elf2 <- unique(strsplit(input$V1[(3*n)-1], '')[[1]])
  
  # Extract string of second elf and put in vector
  elf3 <- unique(strsplit(input$V1[3*n], '')[[1]])
  
  # Find common item
  intersect1 <- intersect(elf1, elf2)
  intersect2 <- intersect(intersect1, elf3)

  # Lookup
  value <- kvp$nums[kvp$letters == intersect2]
  
  # Store
  group_values[n] <- value
  
}

# Answer 2 - find sum of all values
sum(group_values)

