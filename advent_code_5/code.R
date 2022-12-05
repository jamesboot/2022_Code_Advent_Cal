# Set working directory
setwd('Documents/Code_Advent/advent_code_5/')

# Read in input
# Get instructions first
instr <- read.delim('input', skip = 9, header = F, sep = '')

# Rename the headers of the instr dataframe
colnames(instr) <- c('move', 'NUMBER', 'from', 'STACK1', 'to', 'STACK2')

# Get the stacks
# Top is 1, second down is 2 etc...
row_list <- list()

# For loop to get all the rows out
for (x in 1:8) {
  
  row_list[[paste0('row',x)]] <- gsub('    ', ' NA', as.vector(read.delim('input', header = F, sep = '\t', nrows = 8))[[1]][x])
  
  row_list[[paste0('row',x)]] <- strsplit(row_list[[paste0('row',x)]], ' ')
  
  
}

# Make final list of stacks
stack_list <- list()

# Extract the crate names from the row_list to create stacks 
for (x in 1:9) {
  stack_list[[x]] <- c(
    row_list[[1]][[1]][x],
    row_list[[2]][[1]][x],
    row_list[[3]][[1]][x],
    row_list[[4]][[1]][x],
    row_list[[5]][[1]][x],
    row_list[[6]][[1]][x],
    row_list[[7]][[1]][x],
    row_list[[8]][[1]][x]
  )
  
}

# Remove the NA values 
for (x in 1:9) {
  stack_list[[x]] <- stack_list[[x]][stack_list[[x]] != 'NA']
}

# Move the stacks according to the instructions
for (x in 1:nrow(instr)) {
  
  n_to_move <- instr$NUMBER[x]
  
  while (n_to_move >= 1) {
    
    stack_list[[instr$STACK2[x]]] <- c(stack_list[[instr$STACK1[x]]][1], stack_list[[instr$STACK2[x]]])
    stack_list[[instr$STACK1[x]]] <- stack_list[[instr$STACK1[x]]][-1]
    n_to_move <- n_to_move - 1
    
  }
}

# Answer 1 - find crates on top of each stack
for (x in 1:9) {
  print(stack_list[[x]][1])
}

# PART 2

# Multiple crates beign moved retains their order - not one at a time

# Extract the crate names from the row_list to create stacks 
for (x in 1:9) {
  stack_list[[x]] <- c(
    row_list[[1]][[1]][x],
    row_list[[2]][[1]][x],
    row_list[[3]][[1]][x],
    row_list[[4]][[1]][x],
    row_list[[5]][[1]][x],
    row_list[[6]][[1]][x],
    row_list[[7]][[1]][x],
    row_list[[8]][[1]][x]
  )
  
}

# Remove the NA values 
for (x in 1:9) {
  stack_list[[x]] <- stack_list[[x]][stack_list[[x]] != 'NA']
}


# Move the stacks according to the instructions
for (x in 1:nrow(instr)) {
  
  n_to_move <- instr$NUMBER[x]
  
  stack_list[[instr$STACK2[x]]] <-
    c(stack_list[[instr$STACK1[x]]][1:n_to_move], stack_list[[instr$STACK2[x]]])
  stack_list[[instr$STACK1[x]]] <-
    stack_list[[instr$STACK1[x]]][-c(1:n_to_move)]
  
}

# Answer 2
for (x in 1:9) {
  print(stack_list[[x]][1])
}
