source(here::here("common functions.R"))

# Data input
input <- get_input(20) %>% as.numeric
len <- length(input)
# note the entries are not unique and you can't just look their current position up on the go. Create ID.
names(input) <- 1:len

mix_one <- function(vector, id){
  where_currently <- which(names(vector)==id)
  moving_how_much <- vector[[where_currently]] %% (len-1) #just always move right
  if(where_currently > 1){
    vector <- c(vector[where_currently:len],vector[1:(where_currently-1)]) #align to start at our number
  }
  if (moving_how_much > 0){
    vector <- c(vector[2:(moving_how_much+1)], vector[1], vector[(moving_how_much+2):len])
  }
  return(vector)
}

# P1 
p1 <- input

for (i in 1:len){
  p1 <- p1 %>% mix_one(i)
}

where_0 <- which(p1 == 0)
(ans <- sum(p1[(where_0+1000 - 1) %% len + 1],p1[(where_0+2000 - 1) %% len + 1],p1[(where_0+3000 - 1) %% len + 1]))

# P2
decryption_key <- 811589153
p2 <- input * decryption_key

for (n in 1:10){
  for (i in 1:len){
    p2 <- p2 %>% mix_one(i)
  }
}

options(digits = 22)
where_0 <- which(p2 == 0)
(ans <- sum(p2[(where_0+1000 - 1) %% len + 1],p2[(where_0+2000 - 1) %% len + 1],p2[(where_0+3000 - 1) %% len + 1]))