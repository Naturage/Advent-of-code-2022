source(here::here("common functions.R"))

day <- 3

# Data input
input <- get_input(day)

#Pt 1
sack_1 <- substr(input, 1, nchar(input)/2)
sack_2 <- substr(input, 1+nchar(input)/2, nchar(input))

find_common <- function(bag_1, bag_2){
  out <- intersect(
    str_split(bag_1,"") %>% unlist,
    str_split(bag_2,"") %>% unlist)
  return(out)
}

mistakes <- mapply(find_common, sack_1, sack_2)

asciis <- utf8ToInt(paste0(mistakes,collapse = ""))
(asciis %>% sapply(function(x){ifelse(x <= 90, x - 38, x - 96)}) %>% sum())

#Pt 2
trio_sack_1 <- input[which(1:length(input) %% 3 == 1)]
trio_sack_2 <- input[which(1:length(input) %% 3 == 2)]
trio_sack_3 <- input[which(1:length(input) %% 3 == 0)]

find_common_p2 <- function(bag_1, bag_2, bag_3){
  out <- intersect(
    str_split(bag_1,"") %>% unlist,
    str_split(bag_2,"") %>% unlist) %>%
    intersect(str_split(bag_3,"") %>% unlist)
  return(out)
}

badges <- mapply(find_common_p2, trio_sack_1, trio_sack_2, trio_sack_3)

asciis_badges <- utf8ToInt(paste0(badges,collapse = ""))
(asciis_badges %>% sapply(function(x){ifelse(x <= 90, x - 38, x - 96)}) %>% sum())