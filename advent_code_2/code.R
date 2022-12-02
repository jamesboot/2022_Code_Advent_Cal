# Set working directory 
setwd('Documents/Code_Advent/advent_code_2/')

# Load input
input <- read.delim('input',
                    header = F,
                    sep = '')

# Make col 3 with the merge of cols 1 and 3
input$V3 <- paste0(input$V1, input$V2)

# Retrieve all the possible combinations
combos <- unique(input$V3)

# Opponent:
# A for Rock (1), B for Paper (2), and C for Scissors (3)
# Player:
# X for Rock, Y for Paper, and Z for Scissors
# Lose = 0, Draw = 3, Win = 6

# Work out scores for each combination
scores <- c(6, 8, 2, 3, 5, 7, 1, 9, 4)

# Make key values pair list of combos and points they would score
kvp <- data.frame(combination = combos,
                  score = scores)

# Now for loop to go through each row and assign score
for (x in 1:nrow(kvp)) {
  input$score[input$V3 == kvp$combination[x]] <- kvp$score[x]
}

# Calculate sum of scores
sum(input$score)

# PART 2
# X means you need to lose, 
# Y means you need to end the round in a draw,
# Z means you need to win

# Iterate through rows using for loop
for (x in 1:nrow(input)) {
  
  # If the outcome is lose - score is 0
  if (input$V2[x] == 'X') {
    score <- 0
    
    # If we lose and if they use rock - we need scissors = 3 points
    if (input$V1[x] == 'A') {
      score <- score + 3
      
      # If we lose and if they use paper - we need rock = 1 points
    } else if (input$V1[x] == 'B') {
      score <- score + 1
      
      # If we loose and if they use paper - we need rock = 1 points
    } else if (input$V1[x] == 'C') {
      score <- score + 2
    }
    
    # Return the score
    input$score2[x] <- score
    
    # If the outcome is draw - score is 3
  } else if (input$V2[x] == 'Y') {
    score <- 3
    
    # If we draw and if they use rock - we need rock = 1 points
    if (input$V1[x] == 'A') {
      score <- score + 1
      
      # If we draw and if they use paper - we need paper = 2 points
    } else if (input$V1[x] == 'B') {
      score <- score + 2
      
      # If we draw and if they use scissors - we need scissors = 3 points
    } else if (input$V1[x] == 'C') {
      score <- score + 3
    }
    
    # Return the score
    input$score2[x] <- score
    
    # If the outcome is win - score is 6
  } else if (input$V2[x] == 'Z') {
    score <- 6
    
    # If we win and if they use rock - we need paper - 2 points
    if (input$V1[x] == 'A') {
      score <- score + 2
      
      # If we win and if they use paper - we need scissors - 3 points
    } else if (input$V1[x] == 'B') {
      score <- score + 3
      
      # If we win and if they use scissors - we need rock - 1 points
    } else if (input$V1[x] == 'C') {
      score <- score + 1
      
    }
    
    # Return the score
    input$score2[x] <- score
  }
}

# Calculate sum of new scores
sum(input$score2)





