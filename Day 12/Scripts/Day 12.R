source(here::here("common functions.R"))

# Data input
input <- get_input(12) %>%
  str_split("", simplify = TRUE)

altitude_lookup <- c(1:26,1,26)
names(altitude_lookup) <- c(letters,"S","E")

altitudes <- altitude_lookup[input] %>%
  matrix(nrow = nrow(input))

neighbours <- function(x,y){
  out <- list()
  if (x > 1)           {out <- append(out, list(c(x-1,y)))}
  if (x < nrow(input)) {out <- append(out, list(c(x+1,y)))}
  if (y > 1)           {out <- append(out, list(c(x,y-1)))}
  if (y < ncol(input)) {out <- append(out, list(c(x,y+1)))}
  return(out)
}

# P1
needs_checking <- matrix(rep(0,length(input)), nrow = nrow(input))
needs_checking[which(input  == "S", arr.ind = TRUE)] <- 1

ttr <- matrix(rep(-1,length(input)), nrow = nrow(input))
ttr[which(input  == "S", arr.ind = TRUE)] <- 0

current_distance <- 1
while(ttr[which(input  == "E", arr.ind = TRUE)] == -1){
  entries_to_check <- which(needs_checking == 1, arr.ind = TRUE)
  for (n in 1:nrow(entries_to_check)){
    x <- entries_to_check[n,1]
    y <- entries_to_check[n,2]
    alt <- altitudes[x,y]
    for(near in neighbours(x,y)){
      if((altitudes[[near[1],near[2]]] <= alt + 1) & (ttr[[near[1],near[2]]] == -1)){
        ttr[[near[1],near[2]]] <- current_distance
        needs_checking[[near[1],near[2]]] <- 1
      }
    }
    needs_checking[x,y] <- 0
  }
  current_distance <- current_distance + 1
}
(ttr[which(input  == "E", arr.ind = TRUE)])

# P2
needs_checking <- matrix(rep(0,length(input)), nrow = nrow(input))
needs_checking[which(input  == "E", arr.ind = TRUE)] <- 1

ttr <- matrix(rep(-1,length(input)), nrow = nrow(input))
ttr[which(input  == "E", arr.ind = TRUE)] <- 0

current_distance <- 1
while(min(ttr) == -1){
  entries_to_check <- which(needs_checking == 1, arr.ind = TRUE)
  # The are places you can't reach a peak from.
  if (nrow(entries_to_check) == 0){
    break
  } else{
    for (n in 1:nrow(entries_to_check)){
      x <- entries_to_check[n,1]
      y <- entries_to_check[n,2]
      alt <- altitudes[x,y]
      for(near in neighbours(x,y)){
        if((altitudes[[near[1],near[2]]] >= alt - 1) & (ttr[[near[1],near[2]]] == -1)){
          ttr[[near[1],near[2]]] <- current_distance
          needs_checking[[near[1],near[2]]] <- 1
        }
      }
      needs_checking[x,y] <- 0
    }
    current_distance <- current_distance + 1
  }
}

(min(ttr[which(input %in% c("S","a"), arr.ind = TRUE)] %>% .[. > 0]))