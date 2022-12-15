source(here::here("common functions.R"))

# Data input

input <- get_input(14) %>%
  str_split(" -> ")

map <- matrix(rep(0,1000*200), nrow = 200)

for (line in input){
  tmp <- sapply(line,str_split,pattern = ",") %>% 
    lapply(as.numeric)
  
  # Note the coordinate swap!
  for (i in 1:(length(tmp)-1)){
    if (tmp[[i]][1] == tmp[[i+1]][1]){
      map[tmp[[i]][2]:tmp[[i+1]][2],tmp[[i]][1]] <- 1
    } else {
      map[tmp[[i]][2],tmp[[i]][1]:tmp[[i+1]][1]] <- 1
    }
  }
}

map_bkup <- map

# Pt 1

map <- map_bkup
sand_trajectory <- list(c(0,500))

while(TRUE){
  current_spot <- sand_trajectory[[length(sand_trajectory)]]
  if (current_spot[1] == nrow(map)){
    break
  } else if (map[[current_spot[1]+1,current_spot[2]]] == 0){
    sand_trajectory <- append(sand_trajectory,list(c(current_spot[1]+1,current_spot[2])))
  } else if (map[[current_spot[1]+1,current_spot[2]-1]] == 0){
    sand_trajectory <- append(sand_trajectory,list(c(current_spot[1]+1,current_spot[2]-1)))
  } else if (map[[current_spot[1]+1,current_spot[2]+1]] == 0){
    sand_trajectory <- append(sand_trajectory,list(c(current_spot[1]+1,current_spot[2]+1)))
  } else {
    map[current_spot[1],current_spot[2]] <- 2
    sand_trajectory[[length(sand_trajectory)]] <- NULL
  }
}

(ans <- sum(map == 2))

# Pt 2
map <- map_bkup

maxrow <- max(which(map == 1, arr.ind = TRUE)[,1])
map[maxrow+2,] <- 1

sand_trajectory <- list(c(0,500))

while(TRUE){
  if (length(sand_trajectory) == 0){
    break
  } else {
    current_spot <- sand_trajectory[[length(sand_trajectory)]]
    if (map[[current_spot[1]+1,current_spot[2]]] == 0){
      sand_trajectory <- append(sand_trajectory,list(c(current_spot[1]+1,current_spot[2])))
    } else if (map[[current_spot[1]+1,current_spot[2]-1]] == 0){
      sand_trajectory <- append(sand_trajectory,list(c(current_spot[1]+1,current_spot[2]-1)))
    } else if (map[[current_spot[1]+1,current_spot[2]+1]] == 0){
      sand_trajectory <- append(sand_trajectory,list(c(current_spot[1]+1,current_spot[2]+1)))
    } else {
      map[current_spot[1],current_spot[2]] <- 2
      sand_trajectory[[length(sand_trajectory)]] <- NULL
    }
  }
}

(ans <- sum(map == 2)+1) # as origin is outside the array

