source(here::here("common functions.R"))

# Data input
input <- get_input(9) %>%
  tibble(raw = .) %>%
  separate(raw, into = c("direction","distance"),sep = " ", convert = TRUE)

dir_vectors <- list(c(1,0),c(-1,0),c(0,1),c(0,-1))
names(dir_vectors) <- c("R","L","U","D")

tail_catchup <- function(front, back, update_list){
  if (max(abs(front-back)) > 1){
    back <- back + sign(front-back)
    if (update_list) {
      tail_locs <<- append(tail_locs, list(back))
    }
  }
  return(back)
}

# P1
knot <- rep(list(c(0,0)),2) # is head, 2 is tail
tail_locs <- list(c(0,0))

for (i in 1:nrow(input)){
  direction <- input$direction[i]
  distance <- input$distance[i]
  for (j in 1:distance){
    knot[[1]] <- knot[[1]] + dir_vectors[[direction]]
    knot[[2]] <- tail_catchup(knot[[1]],knot[[2]], update_list = TRUE)
  }
}

(length(unique(tail_locs)))

# P2
knot <- rep(list(c(0,0)),10) # head is knot 1, tail is knot 10.
tail_locs <- list(c(0,0))

for (i in 1:nrow(input)){
  direction <- input$direction[i]
  distance <- input$distance[i]
  for (j in 1:distance){
    knot[[1]] <- knot[[1]] + dir_vectors[[direction]]
    for (k in 2:10){
      knot[[k]] <- tail_catchup(knot[[k-1]],knot[[k]], update_list = (k==10))
    }
  }
}

(length(unique(tail_locs)))