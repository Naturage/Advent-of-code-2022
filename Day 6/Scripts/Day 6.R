source(here::here("common functions.R"))

# Data input
input <- get_input(6)

input <- input %>% str_split("") %>% unlist()

# Pt 1
i <- 1
while (input[i:(i+3)] %>% unique %>% length != 4){
  i <- i+1
}
(ans <- i+3)

# Pt 2
i <- 1
while (input[i:(i+13)] %>% unique %>% length != 14){
  i <- i+1
}
(ans <- i+13)