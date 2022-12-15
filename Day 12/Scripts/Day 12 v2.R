# V2, sped up as much as was possible.
# Changes:
# Fixed egregious bug in common functions which always downloaded the input. Sorry AoC hosts!
# Moved neighbours calculation into main loop - faster to check multiple if conditions at once.
# Precalculate nrow and ncol.
# got rid of array indexing in favour of single number + modulo (my heart aches for this one)
# got rid a separate matrix that tracks what's due to check - we can just use entries where ttr = current distance.
# to go further, removed which() statement that did the above and instead populate the list for next check one by one.
# End result - code that runs in 4.2ms.

source(here::here("common functions.R"))

# Data input
input <- read_rds(here(paste0("day ",12), "data","input.rds")) %>%
  str_split("", simplify = TRUE)

R <- nrow(input)
C <- ncol(input)

altitude_lookup <- c(1:26,1,26)
names(altitude_lookup) <- c(letters,"S","E")

altitudes <- altitude_lookup[input] %>%
  matrix(nrow = R)

ttr <- matrix(rep(-1,length(input)), nrow = nrow(input))
ttr[which(input  == "E")] <- 0

start_pos <- which(input  == "S")
current_distance <- 1
entries_to_check <- which(input  == "E")
next_check <- c()

while(ttr[start_pos] == -1){
  for (n in entries_to_check){
    x <- n %%  R + 1
    y <- n %/% R + 1
    alt <- altitudes[x,y] - 1
    if((x != 1 && ttr[[x-1,y  ]] == -1) && (altitudes[[x-1,y]] >= alt)){
      ttr[[x-1,y  ]] <- current_distance
      next_check <- c(next_check, (y-1) * R + (x-2))
    }
    if((x != R && ttr[[x+1,y  ]] == -1) && (altitudes[[x+1,y]] >= alt)){
      ttr[[x+1,y  ]] <- current_distance
      next_check <- c(next_check, (y-1) * R + (x))
    }
    if((y != 1 && ttr[[x  ,y-1]] == -1) && (altitudes[[x,y-1]] >= alt)){
      ttr[[x  ,y-1]] <- current_distance
      next_check <- c(next_check, (y-2) * R + (x-1))
    }
    if((y != C && ttr[[x  ,y+1]] == -1) && (altitudes[[x,y+1]] >= alt)){
      ttr[[x  ,y+1]] <- current_distance
      next_check <- c(next_check, (y) * R + (x-1))
    }
  }
  current_distance <- current_distance + 1
  entries_to_check <- next_check
  next_check <- c()
}

# P1
(ttr[start_pos])

# P2
(min(ttr[which(input %in% c("S","a"))] %>% .[. > 0]))
