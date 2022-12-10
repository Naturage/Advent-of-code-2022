source(here::here("common functions.R"))

# Data input
input <- get_input(10)

# Note we're after "during X cycle", i.e. after X-1 cycles. Since we index from 1 that's trivial.
register <- c(1)

for (task in input){
  if (task == "noop"){
    register <- c(register, register[length(register)])
  } else {
    incr <- str_remove(task, "addx ") %>% as.numeric()
    register <- c(register, register[length(register)], register[length(register)]+incr)
  }
}

# Pt 1
times <- 0:5 * 40 + 20
(ans <- sum(register[times] * times))

#Pt 2
drawn_pixel <- rep(0:39, 6)
register <- register[1:240]
sprite_visible <- (abs(register-drawn_pixel) <= 1) %>% ifelse(.,"#",".")
sapply(0:5, function(x){paste0(sprite_visible[(40*x+1):(40*x+40)], collapse = "")}) %>% 
  paste(collapse = "\n") %>% cat()