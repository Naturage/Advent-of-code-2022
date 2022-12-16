source(here::here("common functions.R"))
library(dtplyr) # speed up dplyr via using data.table backend

# Data input
input <- get_input(16) %>%
  str_replace_all("Valve |has flow rate=|;|,|tunnel(s)? lead(s)? to valve(s)? ","") %>%
  str_split(" ")

flow_rates <- lapply(input, function(line){tibble(valve = line[1], 
                                                  amount = line[2] %>% as.numeric)}) %>% 
  bind_rows() %>%
  filter(amount > 0)

path_list <- lapply(input, function(line){tibble(from = line[1], 
                                                 to = line[-1:-2])}) %>% 
  bind_rows()

current_position <- tibble(
  curr_pos = "AA",
  valves_open = "",
  steam_per_turn = 0,
  steam_till_now = 0
)

for (i in 1:30){
  current_position <- rbind(
    current_position %>%
      left_join(path_list, by = c("curr_pos" = "from")) %>%
      mutate(curr_pos = to, to = NULL),
    current_position %>% 
      filter(!str_detect(valves_open,curr_pos)) %>% 
      mutate(valves_open = paste0(valves_open," ",curr_pos)) %>%
      inner_join(flow_rates, by = c("curr_pos" = "valve")) %>%
      mutate(steam_per_turn = steam_per_turn + amount, 
             steam_till_now = steam_till_now - amount, # will re-add in a moment
             amount = NULL)
  ) %>%
    mutate(steam_till_now = steam_till_now + steam_per_turn) %>%
    lazy_dt() %>%
    group_by(curr_pos, valves_open, steam_per_turn) %>%
    summarise(steam_till_now = max(steam_till_now), .groups = "drop") %>%
    collect()
  
  if (i > 20){
    too_low <- max(current_position$steam_till_now) - (31-i)*100 # 100 is eyeballed.
    current_position <- current_position %>% filter(steam_till_now > too_low)
  }
  
  print(paste0(i," ",nrow(current_position)))
}

(ans <- max(current_position$steam_till_now))

# Pt 2

current_position_p2 <- tibble(
  curr_pos_1 = "AA",
  curr_pos_2 = "AA",
  valves_open = "",
  steam_per_turn = 0,
  steam_till_now = 0
)

for (i in 1:26){
  current_position_p2 <- rbind(
    current_position_p2 %>%
      left_join(path_list, by = c("curr_pos_1" = "from")) %>%
      mutate(curr_pos_1 = to, to = NULL),
    current_position_p2 %>% 
      filter(!str_detect(valves_open,curr_pos_1)) %>% 
      mutate(valves_open = paste0(valves_open," ",curr_pos_1)) %>%
      inner_join(flow_rates, by = c("curr_pos_1" = "valve")) %>%
      mutate(steam_per_turn = steam_per_turn + amount, 
             steam_till_now = steam_till_now - amount, # will re-add in a moment
             amount = NULL)
  ) 
  
  current_position_p2 <- rbind(
    current_position_p2 %>%
      left_join(path_list, by = c("curr_pos_2" = "from")) %>%
      mutate(curr_pos_2 = to, to = NULL),
    current_position_p2 %>% 
      filter(!str_detect(valves_open,curr_pos_2)) %>% 
      mutate(valves_open = paste0(valves_open," ",curr_pos_2)) %>%
      inner_join(flow_rates, by = c("curr_pos_2" = "valve")) %>%
      mutate(steam_per_turn = steam_per_turn + amount, 
             steam_till_now = steam_till_now - amount, # will re-add in a moment
             amount = NULL)
  ) 
  
  current_position_p2 <- current_position_p2 %>%
    mutate(tmp        = if_else(curr_pos_1 > curr_pos_2, curr_pos_2, curr_pos_1),
           curr_pos_2 = if_else(curr_pos_1 > curr_pos_2, curr_pos_1, curr_pos_2),
           curr_pos_1 = tmp,
           tmp = NULL) 
  
  current_position_p2 <- current_position_p2 %>%
    mutate(steam_till_now = steam_till_now + steam_per_turn) %>%
    lazy_dt() %>%
    group_by(curr_pos_1, curr_pos_2, valves_open, steam_per_turn) %>%
    summarise(steam_till_now = max(steam_till_now), .groups = "drop") %>%
    collect()
  
  if (i > 10){
    too_low <- max(current_position_p2$steam_till_now) - (27-i)*50 # 100 is eyeballed.
    current_position_p2 <- current_position_p2 %>% filter(steam_till_now > too_low)
  }
  
  # I hate this, but it works.
  #TODO: get the sorting of valves open - either by efficient string sorting, or by using 0-1 cols for open instead.
  #TODO: figure out a way to cull scenarios that are worse in every way
  # Something along the lines of "group by curr pos 1 & 2, if valves opena subset and steam till now lower, yeet"
  current_position_p2 <- current_position_p2 %>% arrange(desc(steam_till_now)) %>%
    slice(1:max(100,nrow(current_position_p2) * 0.5))

  print(paste0(i," ",nrow(current_position_p2)))
}

(ans <- max(current_position_p2$steam_till_now))
