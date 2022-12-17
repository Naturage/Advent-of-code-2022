source(here::here("common functions.R"))

# Data input
move_lr <- list(">" =  1, "<" = -1)

#input <- read_lines(here("day 17","data","input test.txt")) %>% 
input <- get_input(17) %>% 
  str_split("") %>% 
  unlist() %>% 
  sapply(function(x)move_lr[[x]])
names(input) <- NULL

setup_playspace <- function(rows){
  playspace <<- matrix(0, nrow = rows, ncol = 9)
  playspace[nrow(playspace),] <<- 9
  playspace[,1] <<- 9
  playspace[,9] <<- 9
  
  touchdown <<- TRUE
  shape_counter <<- 0
  move_counter <<- 1
}

spawn_shape <- function(shape_counter,playspace){
  if (shape_counter == 1){
    x <- nrow(playspace) - 4
  } else {
    x <- min(which(playspace == 1, arr.ind = TRUE)[,1]) - 4
  }
  if (shape_counter %% 5 == 1){
    shape <- list(c(x,4),c(x,5),c(x,6),c(x,7))
  } else if (shape_counter %% 5 == 2){
    shape <- list(c(x - 2,5),c(x-1,4),c(x-1,5),c(x-1,6),c(x,5))
  } else if (shape_counter %% 5 == 3){
    shape <- list(c(x,4),c(x,5),c(x,6),c(x-1,6),c(x-2,6))
  } else if (shape_counter %% 5 == 4){
    shape <- list(c(x,4),c(x-1,4),c(x-2,4),c(x-3,4))
  } else if (shape_counter %% 5 == 0){
    shape <- list(c(x,4),c(x,5),c(x-1,4),c(x-1,5))
  }
  return(shape)
}

move_shape <- function(playspace, shape, move_counter){
  touchdown <<- FALSE
  
  direction <- input[(move_counter-1) %% length(input) + 1]
  test_position <- lapply(shape, function(x){x + c(0,direction)})
  if (all(test_position %>% sapply(function(x){playspace[x[1],x[2]]}) == 0)){
    shape <- test_position
  }
  test_position <- lapply(shape, function(x){x + c(1,0)})
  if (all(test_position %>% sapply(function(x){playspace[x[1],x[2]]}) == 0)){
    shape <- test_position
  } else {
    touchdown <<- TRUE
  }
  return(shape)
}

land_shape <- function(playspace, shape){
  for (point in shape){
    playspace[point[1],point[2]] <- 1
  }
  return(playspace)
}

#P1

setup_playspace(5000)

while(shape_counter < 2023){
  if (touchdown){
    shape_counter <- shape_counter + 1
    shape <- spawn_shape(shape_counter,playspace)
  }
  shape <- move_shape(playspace, shape, move_counter)
  move_counter <- move_counter + 1
  if (touchdown){
    playspace <- land_shape(playspace, shape)
  }
}

(ans <- max(which(playspace == 1, arr.ind = TRUE)[,1]) - min(which(playspace == 1, arr.ind = TRUE)[,1]) + 1)

# P2

report_state <- function(shape_counter,playspace,shape){
  max_height <- min(which(playspace == 1, arr.ind = TRUE)[,1])
  shape_location <- lapply(shape, function(x){x - c(max_height,0)})
  terrain <- playspace[max_height:(max_height+20),]
  output <- list(max_height = max_height,
                 shape_location = shape_location,
                 terrain = terrain,
                 shape_counter = shape_counter)
  return(output)
}

setup_playspace(10000)
states <- list()

while(length(states) < 2){
  if (touchdown){
    shape_counter <- shape_counter + 1
    shape <- spawn_shape(shape_counter,playspace)
  }
  shape <- move_shape(playspace, shape, move_counter)
  move_counter <- move_counter + 1
  if (touchdown){
    playspace <- land_shape(playspace, shape)
  }
  if(move_counter %% length(input) == 0){
    states <- append(states, list(report_state(shape_counter,playspace,shape)))
  }
}

# Turns out, the problem setters were gracious today.
confirm_equality <- all(states[[1]]$shape_location %>% unlist == states[[2]]$shape_location %>% unlist) &
  all(states[[1]]$terrain == states[[2]]$terrain) &
  (states[[1]]$shape_counter %% 5 == states[[2]]$shape_counter %% 5)

shapes_added_per_cycle <- states[[2]]$shape_counter - states[[1]]$shape_counter
height_added_per_cycle <- states[[1]]$max_height - states[[2]]$max_height

magic_number <- 1000000000000

cycles <- magic_number %/% shapes_added_per_cycle
leftovers <- magic_number %% shapes_added_per_cycle

setup_playspace(5000)

while(shape_counter <= leftovers){
  if (touchdown){
    shape_counter <- shape_counter + 1
    shape <- spawn_shape(shape_counter,playspace)
  }
  shape <- move_shape(playspace, shape, move_counter)
  move_counter <- move_counter + 1
  if (touchdown){
    playspace <- land_shape(playspace, shape)
  }
}

options(digits = 22)

(ans <- max(which(playspace == 1, arr.ind = TRUE)[,1]) - min(which(playspace == 1, arr.ind = TRUE)[,1]) + 1 +
    cycles * height_added_per_cycle)