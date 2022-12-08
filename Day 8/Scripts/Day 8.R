source(here::here("common functions.R"))

# Data input
input <- get_input(8) %>%
  str_split("", simplify = TRUE) %>%
  as.numeric() %>%
  matrix(nrow = sqrt(length(.)))

visible <- matrix(rep(0,length(input)),
  nrow = nrow(input))

# Pt 1
for (i in 1:nrow(input)){
  vis_left <- input[i,]
  visible_left_indices <- c(1,which(vis_left > lag(cummax(vis_left),1)))
  visible[i,visible_left_indices] <- 1
  
  vis_right <- rev(input[i,])
  visible_right_indices <- ncol(input) + 1 - c(1,which(vis_right > lag(cummax(vis_right),1)))
  visible[i,visible_right_indices] <- 1
  
  vis_down <- input[,i]
  visible_down_indices <- c(1,which(vis_down > lag(cummax(vis_down),1)))
  visible[visible_down_indices,i] <- 1
  
  vis_up <- rev(input[,i])
  visible_up_indices <- nrow(input) + 1 - c(1,which(vis_up > lag(cummax(vis_up),1)))
  visible[visible_up_indices,i] <- 1
}

(sum(visible))

# Pt 2
scenic_score <- 0

visible_trees <- function(tree_vec, height){
  if (length(tree_vec) == 0){
    out <- 0
  } else {
    if (max(tree_vec) >= height){
      tree_vec <- tree_vec[1:min(which(tree_vec >= height))] 
    }
    out <- length(tree_vec)
  }
  return(out)
}

for (i in 1:nrow(input)){
  for (j in 1:ncol(input)){
    height <- input[i,j]
    trees_left  <- rev(input[row(input) == i & col(input) < j])
    trees_right <-     input[row(input) == i & col(input) > j]
    trees_up    <- rev(input[row(input) < i & col(input) == j])
    trees_down  <-     input[row(input) > i & col(input) == j]
    near_trees <- list(trees_left,trees_right,trees_up,trees_down)
    scenic_tmp <- sapply(near_trees,visible_trees, height = height) %>% prod
    scenic_score <- max(scenic_score, scenic_tmp)
    }
}

(scenic_score)