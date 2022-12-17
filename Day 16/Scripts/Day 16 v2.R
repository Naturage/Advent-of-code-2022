source(here::here("common functions.R"))

# Data input

input <- get_input(16) %>%
  #input <- read_lines(here("day 16","data","input test.txt")) %>%
  str_replace_all("Valve |has flow rate=|;|,|tunnel(s)? lead(s)? to valve(s)? ","") %>%
  str_split(" ")

flow_rates <- lapply(input, function(line){tibble(valve = line[1], 
                                                  amount = line[2] %>% as.numeric)}) %>% 
  bind_rows() %>%
  filter(amount > 0)

path_list <- lapply(input, function(line){tibble(from = line[1], 
                                                 to = line[-1:-2])}) %>% 
  bind_rows()

scenario_list <- list(list(
  curr_pos = "AA",
  valves_open = c(),
  steam_per_turn = 0,
  steam_till_now = 0,
  keep = TRUE
))

for(minute in 1:30){
  
  next_scenario <- c()
  
  for (scenario in scenario_list){
    next_scenario <- append(next_scenario, 
                            path_list %>% 
                              filter(from == scenario$curr_pos) %>% 
                              pull(to) %>%
                              lapply(function(x){list(
                                curr_pos = x,
                                valves_open = scenario$valves_open,
                                steam_per_turn = scenario$steam_per_turn,
                                steam_till_now = scenario$steam_till_now,
                                keep = TRUE)}))
    
    if (!(scenario$curr_pos %in% scenario$valves_open) & 
        (scenario$curr_pos %in% flow_rates$valve)){
      next_scenario <- append(next_scenario,
                              flow_rates %>%
                                filter(valve == scenario$curr_pos) %>%
                                pull(amount) %>%
                                lapply(function(x){list(
                                  curr_pos = scenario$curr_pos,
                                  valves_open = c(scenario$valves_open, scenario$curr_pos),
                                  steam_per_turn = scenario$steam_per_turn + x,
                                  steam_till_now = scenario$steam_till_now - x,
                                  keep = TRUE)}))
    }
  }
  
  next_scenario <- next_scenario %>%
    lapply(function(x){
      if(!is.null(x$valves_open)){
        x$valves_open <- sort(x$valves_open)
      }
      return(x)
    })
  
  for (i in 1:(length(next_scenario)-1)){
    for (j in (i+1):length(next_scenario)){
      i_in_j <- (next_scenario[[i]]$curr_pos           ==   next_scenario[[j]]$curr_pos     &&
                   (all(next_scenario[[i]]$valves_open   %in% next_scenario[[j]]$valves_open) ||
                      is.null(next_scenario[[i]]$valves_open))                                &&
                   next_scenario[[i]]$steam_till_now     <=   next_scenario[[j]]$steam_till_now)
      
      j_in_i <- (next_scenario[[j]]$curr_pos           ==   next_scenario[[i]]$curr_pos     &&
                   (all(next_scenario[[j]]$valves_open   %in% next_scenario[[i]]$valves_open) ||
                      is.null(next_scenario[[j]]$valves_open))                                 &&
                   next_scenario[[j]]$steam_till_now     <=   next_scenario[[i]]$steam_till_now)
      
      if      (i_in_j & j_in_i){next_scenario[[i]]$keep <- FALSE} 
      else if (i_in_j)         {next_scenario[[i]]$keep <- FALSE} 
      else if (j_in_i)         {next_scenario[[j]]$keep <- FALSE}
    }
  }
  
  scenario_list <- next_scenario[next_scenario %>% sapply(function(x){x$keep})] %>%
    lapply(function(x){
      x$steam_till_now <- x$steam_till_now + x$steam_per_turn
      return(x)
    })
  
  print(paste0("time = ",minute,", scenarios = ",length(next_scenario),", keeping = ",length(scenario_list)))
  
  print(paste0("max release = ",max(scenario_list %>% sapply(function(x){x$steam_per_turn})),
               ", max to date = ",max(scenario_list %>% sapply(function(x){x$steam_till_now}))))
}

(ans <- max(scenario_list %>% sapply(function(x){x$steam_till_now})))

a <- scenario_list[scenario_list %>% sapply(function(x){x$curr_pos}) == "ZB"]