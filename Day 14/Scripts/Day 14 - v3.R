# Further change - doing p2 a different, hopefully more efficient way.
# Leaving v2 unchanged as it's still quite elegant and this won't be.
# P2 is now done by line, asserting that any place sand could be is filled;
# so if there was sand above, the three diagonal spots below will be either sand or stone.
# Runtime - 48ms.
# Amusingly, there's no good reason to reset the map - any sand of P1 is subset of P2 
# and doing it this way we can use it directly.
# That cuts it down to 46ms.
# Cut down initial matrix to 400x200 as it can't go wider than distance down.


source(here::here("common functions.R"))
f <- function(){
  for (x in 1:1000){
    # Data input
    
    input <- readRDS(here("day 14","data","input.RDS")) %>%
      str_split(" -> ")
    
    map <- matrix(rep(0,400*200), nrow = 200)
    
    for (line in input){
      tmp <- sapply(line,str_split,pattern = ",") %>% 
        lapply(as.numeric)
      
      # Note the coordinate swap and the +1 to get 0,500 within!
      for (i in 1:(length(tmp)-1)){
        map[tmp[[i]][2]:tmp[[i+1]][2] + 1,tmp[[i]][1]:tmp[[i+1]][1] - 300] <- 1
      }
    }
    
    sandfall <- function(path){
      x <- length(path)+1
      y <- path[x-1]
      if (map[[x,y]] == 0) {
        return(c(path,y))
      } else if (map[[x,y-1]] == 0) {
        return(c(path,y-1))
      } else if (map[[x,y+1]] == 0) {
        return(c(path,y+1))
      } else {
        map[x-1,y] <<- 2
        return(path[-length(path)])
      }
    }
    
    # Pt 1
    
    #map <- map_bkup
    sand_trajectory <- c(200)
    
    while(length(sand_trajectory) != nrow(map)){
      sand_trajectory <- sandfall(sand_trajectory)
    }
    
    (ans <- sum(map == 2))
    
    # Pt 2
    
    maxrow <- max(which(map == 1, arr.ind = TRUE)[,1])
    
    map[1,200] <- 2
    
    for (i in 2:(maxrow + 1)){
      map[i,which((map[i-1,] == 2) & (map[i,] == 0))] <- 2
      sand_above <- which(map[i-1,] == 2)
      diagonal <- unique(c(sand_above - 1, sand_above, sand_above + 1))
      new_sand <- intersect(diagonal, which(map[i,] == 0))
      map[i,new_sand] <- 2
    }
    
    (ans <- sum(map == 2))
  }
}

system.time(f())