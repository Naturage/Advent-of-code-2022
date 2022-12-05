source(here::here("common functions.R"))

# Data input
input <- get_input(5)

# Parsing the stacks
input_stacks <- input[1:(which(input == "")-2)]

no_stacks <- input[which(input == "")-1] %>% 
  str_match_all("\\d+") %>% 
  unlist() %>% 
  as.numeric() %>% 
  max

stacks <- list()

for(i in 1:no_stacks){
  stacks[[i]] <- input_stacks %>% str_sub(start = 4*i-2, end = 4*i-2) %>% .[which(. != " ")]
}

stacks_backup <- stacks

# Parsing the orders
input_orders <- input[-(1:(which(input == "")))]

parsed_orders <- tibble(raw = input_orders) %>%
  mutate(raw = str_replace_all(raw, "move |from |to ","")) %>%
  separate(raw, c("amount","from","to"), sep = " ") %>%
  mutate(across(everything(), as.numeric))

# Pt 1
stacks <- stacks_backup

for (i in 1:nrow(parsed_orders)){
  order <- parsed_orders %>% slice(i)
  amount <- order$amount
  from <- order$from
  to <- order$to
  
  for (j in 1:amount){
    hoisted <- stacks[[from]][1]
    stacks[[from]] <- stacks[[from]][-1]
    stacks[[to]] <- c(hoisted,stacks[[to]])
  }
}

(ans <- sapply(stacks, function(x){x[1]}) %>% paste0(collapse = ""))

# Pt 2
stacks <- stacks_backup

for (i in 1:nrow(parsed_orders)){
  order <- parsed_orders %>% slice(i)
  amount <- order$amount
  from <- order$from
  to <- order$to
  
  hoisted <- stacks[[from]][1:amount]
  stacks[[from]] <- stacks[[from]][-1:-amount]
  stacks[[to]] <- c(hoisted,stacks[[to]])
}

(ans <- sapply(stacks, function(x){x[1]}) %>% paste0(collapse = ""))