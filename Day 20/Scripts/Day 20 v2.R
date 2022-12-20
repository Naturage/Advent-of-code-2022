# Optimising.
# Starting time: 14.11
# names swapped to separate vector, output into global env via <<-. 10.52
# which swapped for match. 9.7
# swapped the logic to only move positions about and refer to same original number list. 6.03 (depends on run - up to 6.7)

source(here::here("common functions.R"))

ptm <- proc.time()
# Data input
# input <- c(1, 2, -3, 3, -2, 0, 4)
input <- get_input(20) %>% as.numeric
len <- length(input)

mix_one <- function(pos, id, input){
  where_currently <- match(id,pos)
  moving_how_much <- input[id] %% (len-1) #just always move right
  if(where_currently > 1){
    pos <- pos[c(where_currently:len,1:(where_currently-1))]
  }
  pos <- append(pos[-1], pos[1], after = moving_how_much)
  return(pos)
}

# P1 
p1 <- input
pos <- 1:len

for (i in 1:len){
  pos <- pos %>% mix_one(i,p1)
}

p1 <- p1[pos]
(ans <- sum(p1[((1:3 * 1000) + match(0,p1) - 1) %% len + 1]))

# P2
decryption_key <- 811589153
p2 <- input * decryption_key
pos <- 1:len

for (n in 1:10){
  for (i in 1:len){
    pos <- pos %>% mix_one(i,p2)
  }
}

options(digits = 22)
p2 <- p2[pos]
(ans <- sum(p2[((1:3 * 1000) + match(0,p2) - 1) %% len + 1]))

ptm - proc.time()

