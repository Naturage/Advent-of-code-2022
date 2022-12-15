#goal: speed up substantially from current 165ms.
# Changes:
# Put function into one place to tidy the code. Actually slowed it down.
# Within sandfall, move return into if-else staements instead of assigning path and then outputting it. Negligible gain.
# On realising that height coordinate always increases by 1, swapped lists for vectors. 
# Note we need to shift the whole map down by 1 for this to work - 1 indexed matrices! Down to 140ms.
# Minimal change - tidied up input as it produces a nx1/1xn rectangle regardless of if statement.

source(here::here("common functions.R"))
# Data input

input <- readRDS(here("day 14","data","input.RDS")) %>%
  str_split(" -> ")

map <- matrix(rep(0,1000*200), nrow = 200)

for (line in input){
  tmp <- sapply(line,str_split,pattern = ",") %>% 
    lapply(as.numeric)
  
  # Note the coordinate swap and the +1 to get 0,500 within!
  for (i in 1:(length(tmp)-1)){
    map[tmp[[i]][2]:tmp[[i+1]][2] + 1,tmp[[i]][1]:tmp[[i+1]][1]] <- 1
  }
}

map_bkup <- map

sandfall <- function(path){
  x <- length(path)
  y <- path[x]
  if (map[[x+1,y]] == 0) {
    return(c(path,y))
  } else if (map[[x+1,y-1]] == 0) {
    return(c(path,y-1))
  } else if (map[[x+1,y+1]] == 0) {
    return(c(path,y+1))
  } else {
    map[x,y] <<- 2
    return(path[-length(path)])
  }
}

# Pt 1

map <- map_bkup
sand_trajectory <- c(500)

while(length(sand_trajectory) != nrow(map)){
  sand_trajectory <- sandfall(sand_trajectory)
}

(ans <- sum(map == 2))

# Pt 2
map <- map_bkup

maxrow <- max(which(map == 1, arr.ind = TRUE)[,1])

map[maxrow+2,] <- 1

sand_trajectory <- c(500)

while(length(sand_trajectory) != 0){
  sand_trajectory <- sandfall(sand_trajectory)
}

(ans <- sum(map == 2))