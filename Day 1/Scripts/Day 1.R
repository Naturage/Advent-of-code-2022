source(here::here("common functions.R"))

day <- 1

# Data input
input <- get_input(day) %>% as.numeric()
input <- c(0,input)

cumul_input <- rep(0, length(input))
for (i in 2:length(input)){
  cumul_input[i] <- ifelse(is.na(input[i]),0,cumul_input[i-1]+input[i])
}

calories_carried <- cumul_input[which(cumul_input == 0) - 1]

# Pt 1
max(cumul_input)

# Pt 2
sort(calories_carried, decreasing = TRUE)[1:3] %>% sum()
