source(here::here("common functions.R"))

day <- 2

# Data input
input <- get_input(day)

a <- table(input)

#Pt 1
score <- 1 * grepl("X", input) + 
  2 * grepl("Y", input) + 
  3 * grepl("Z", input) + 
  0 * grepl("A Z|B X|C Y",input) +
  3 * grepl("A X|B Y|C Z",input) + 
  6 * grepl("A Y|B Z|C X",input)

sum(score)
  
score2 <- 0 * grepl("X", input) + 
  3 * grepl("Y", input) + 
  6 * grepl("Z", input) + 
  3 * grepl("A X|B Z|C Y",input) +
  2 * grepl("A Z|B Y|C X",input) + 
  1 * grepl("A Y|B X|C Z",input)

sum(score2)