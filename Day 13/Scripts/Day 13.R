source(here::here("common functions.R"))

# Data input
lister <- function(line){
  out <- line %>% 
    str_replace_all("\\[","list\\(") %>%
    str_replace_all("\\]","\\)") %>% 
    parse(text = .) %>%
    eval()
} 

# input <- read_lines(here("day 13","data","input example.txt")) %>%
input <- get_input(13) %>%
  .[which(. != "")] %>%
  lapply(lister)

grand_comparison <- function(x,y){
  # empty list is smaller than anything.
  if (length(x) == 0 && length(y) == 0){return(NA)}
  else if (length(x) == 0) {return(TRUE)}
  else if (length(y) == 0) {return(FALSE)}
  # if x[[1]] and y[[1]] are both integers, compare directly.
  # smaller one is... smaller. If equal, discard and continue.
  if (is.numeric(x[[1]]) && is.numeric(y[[1]])){
    # print(paste0("both int, ",x[[1]]," ",y[[1]]))
    if (x[[1]] < y[[1]]) {return(TRUE)}
    else if (x[[1]] > y[[1]]) {return(FALSE)}
    else {
      x[[1]] <- NULL
      y[[1]] <- NULL
      return(grand_comparison(x,y))
    }
  }
  # if x[[1]] and y[[1]] are both lists, unlist them and compare.
  # if equal, discard the whole list.
  if (is.list(x[[1]]) && is.list(y[[1]])){
    # print(paste0("both list"))
    # dput(x[[1]])
    # dput(y[[1]])
    outcome <- grand_comparison(x[[1]],y[[1]])
    if (is.na(outcome)){
      x[[1]] <- NULL
      y[[1]] <- NULL
      return(grand_comparison(x,y))
    } else {
      return(outcome)
    }
  }
  # finally, if one's a list and the other isn't, put int into a list and retry.
  if (is.numeric(x[[1]]) && is.list(y[[1]])){
    # print(paste0("x wasn't numeric, converting ",x[[1]]))
    x[[1]] <- list(x[[1]])
    return(grand_comparison(x,y))
  }
  if (is.list(x[[1]]) && is.numeric(y[[1]])){
    # print(paste0("y wasn't numeric, converting ",y[[1]]))
    y[[1]] <- list(y[[1]])
    return(grand_comparison(x,y))
  }
}

# Pt 1
ans <- 0
for (i in 1:(length(input)/2)){
  ans <- ans + i * grand_comparison(input[[2*i - 1]],input[[2*i]])
}
(ans)

# Pt 2
# God forbid we're not sorting this proper. Just need 2 locations.
loc_1 <- sapply(input, grand_comparison, y = list(list(2))) %>% sum(.,1)
loc_2 <- sapply(input, grand_comparison, y = list(list(6))) %>% sum(.,2) # as 1 more is the upper one
(ans <- loc_1 * loc_2)