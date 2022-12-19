source(here::here("common functions.R"))

ptm <- proc.time()

# Data input
input <- get_input(19)

input_clean <- tibble(blueprint_id = str_match(input,"Blueprint (\\d+):")[,2],
       ore_rob_ore = str_match(input,"ore robot costs (\\d+) ore")[,2],
       clay_rob_ore = str_match(input,"clay robot costs (\\d+) ore")[,2],
       obs_rob_ore = str_match(input,"obsidian robot costs ((\\d+) ore)?( and)?( (\\d+) clay)?")[,3],
       obs_rob_clay = str_match(input,"obsidian robot costs ((\\d+) ore)?( and)?( (\\d+) clay)?")[,6],
       geo_rob_ore = str_match(input,"geode robot costs ((\\d+) ore)?( and)?( (\\d+) obsidian)?")[,3],
       geo_rob_obs = str_match(input,"geode robot costs ((\\d+) ore)?( and)?( (\\d+) obsidian)?")[,6]) %>%
  mutate(across(everything(), as.numeric))

ans <- 0

for (i in 1:nrow(input_clean)){
  blueprint <- input_clean %>% slice(i)
  max_ore_robots <- max(blueprint$clay_rob_ore,blueprint$obs_rob_ore,blueprint$geo_rob_ore)
  
  geo_max <- 0
  
  scenarios <- tibble(
    time        = 0,
    ore         = 0,  clay        = 0,  obs         = 0,  geo         = 0,
    ore_robots  = 1,  clay_robots = 0,  obs_robots  = 0,  geo_robots  = 0)
  
  while(nrow(scenarios > 0)){
    
    # build order: ore robot
    # prereq: less ore robots than can use in a turn.
    scenarios_ore <- scenarios %>%
      filter(ore_robots < max_ore_robots) %>%
      mutate(eta = ifelse(ore > blueprint$ore_rob_ore, 1, ceiling((blueprint$ore_rob_ore - ore)/ore_robots) + 1)) %>%
      mutate(time = time + eta,
             ore  = ore  + eta * ore_robots - blueprint$ore_rob_ore,
             clay = clay + eta * clay_robots,
             obs  = obs  + eta * obs_robots ,
             geo  = geo  + eta * geo_robots ,
             ore_robots = ore_robots + 1) %>%
      select(-eta) %>%
      filter(time <= 24)
    
    geo_max <- max(geo_max, scenarios_ore$geo)
    scenarios_ore <- scenarios_ore %>% filter(time < 24)
    
    # build order: clay robot
    scenarios_clay <- scenarios %>%
      filter(clay_robots < blueprint$obs_rob_clay) %>%
      mutate(eta = ifelse(ore > blueprint$clay_rob_ore, 1, ceiling((blueprint$clay_rob_ore - ore)/ore_robots) + 1)) %>%
      mutate(time = time + eta,
             ore  = ore  + eta * ore_robots - blueprint$clay_rob_ore,
             clay = clay + eta * clay_robots,
             obs  = obs  + eta * obs_robots ,
             geo  = geo  + eta * geo_robots ,
             clay_robots = clay_robots + 1) %>%
      select(-eta) %>%
      filter(time <= 24)
    
    geo_max <- max(geo_max, scenarios_clay$geo)
    scenarios_clay <- scenarios_clay %>% filter(time < 24)
    
    # build order: obsidian robot
    # prereq: clay robot
    scenarios_obs <- scenarios %>%
      filter(clay_robots > 0) %>%
      mutate(eta1 = ifelse(ore  > blueprint$obs_rob_ore , 1, ceiling((blueprint$obs_rob_ore  - ore )/ore_robots ) + 1),
             eta2 = ifelse(clay > blueprint$obs_rob_clay, 1, ceiling((blueprint$obs_rob_clay - clay)/clay_robots) + 1),
             eta = pmax(eta1, eta2)) %>%
      mutate(time = time + eta,
             ore  = ore  + eta * ore_robots  - blueprint$obs_rob_ore,
             clay = clay + eta * clay_robots - blueprint$obs_rob_clay,
             obs  = obs  + eta * obs_robots ,
             geo  = geo  + eta * geo_robots ,
             obs_robots = obs_robots + 1) %>%
      select(-eta1, -eta2, -eta) %>%
      filter(time <= 24)
    
    geo_max <- max(geo_max, scenarios_obs$geo)
    scenarios_obs <- scenarios_obs %>% filter(time < 24)
    
    # build order: geode robot
    # prereq: obsidian robot
    scenarios_geo <- scenarios %>%
      filter(obs_robots > 0) %>%
      mutate(eta1 = ifelse(ore > blueprint$geo_rob_ore, 1, ceiling((blueprint$geo_rob_ore - ore)/ore_robots) + 1),
             eta2 = ifelse(obs > blueprint$geo_rob_obs, 1, ceiling((blueprint$geo_rob_obs - obs)/obs_robots) + 1),
             eta = pmax(eta1, eta2)) %>%
      mutate(time = time + eta,
             ore  = ore  + eta * ore_robots  - blueprint$geo_rob_ore,
             clay = clay + eta * clay_robots,
             obs  = obs  + eta * obs_robots  - blueprint$geo_rob_obs,
             geo  = geo  + eta * geo_robots ,
             geo_robots = geo_robots + 1) %>%
      select(-eta1, -eta2, -eta) %>%
      filter(time <= 24)
    
    geo_max <- max(geo_max, scenarios_geo$geo)
    scenarios_geo <- scenarios_geo %>% filter(time < 24)
    
    # built order: idle till t=24.
    # prereq: geode robot
    scenarios_idle <- scenarios %>%
      filter(geo_robots > 0) %>%
      mutate(eta = 24 - time) %>%
      mutate(time = time + eta,
             ore  = ore  + eta * ore_robots ,
             clay = clay + eta * clay_robots,
             obs  = obs  + eta * obs_robots ,
             geo  = geo  + eta * geo_robots ) %>%
      select(-eta)
    
    geo_max <- max(geo_max, scenarios_idle$geo)
    
    scenarios <- bind_rows(scenarios_ore, scenarios_clay, scenarios_obs, scenarios_geo)
    
    # a couple explicit dead end filters: 
    # any geode robot needs 7+ obsidian, so time >= 17 with 0 obsidian robots is a no-go. 
    # any obsidian robot needs 5+ clay, so time >= 12 with 0 clay robots is a no-go. 
    # any scenario where making a geode robot a turn won't get you to the goal anymore is a no-go.
    scenarios <- scenarios %>%
      filter(time < 24) %>%
      filter(!(time >= 17 & obs_robots == 0)) %>%
      filter(!(time >= 12 & clay_robots == 0)) %>%
      filter((24-time) * geo_robots + (24-time)*(25-time)/2 + geo > geo_max)
    
    #print(paste0(i,", goal - ",geo_max, ", scen - ",nrow(scenarios), ", min time ",min(scenarios$time)))
  }
  print(paste(i,geo_max))
  ans <- ans + i* geo_max
}

(ans)

# P2 - just copypaste

ans <- 1

for (i in 1:3){
  blueprint <- input_clean %>% slice(i)
  max_ore_robots <- max(blueprint$clay_rob_ore,blueprint$obs_rob_ore,blueprint$geo_rob_ore)
  
  geo_max <- 0
  
  scenarios <- tibble(
    time        = 0,
    ore         = 0,  clay        = 0,  obs         = 0,  geo         = 0,
    ore_robots  = 1,  clay_robots = 0,  obs_robots  = 0,  geo_robots  = 0)
  
  while(nrow(scenarios > 0)){
    
    # build order: ore robot
    # prereq: less ore robots than max ore needed a minute.
    scenarios_ore <- scenarios %>%
      filter(ore_robots < max_ore_robots) %>%
      mutate(eta = ifelse(ore > blueprint$ore_rob_ore, 1, ceiling((blueprint$ore_rob_ore - ore)/ore_robots) + 1)) %>%
      mutate(time = time + eta,
             ore  = ore  + eta * ore_robots - blueprint$ore_rob_ore,
             clay = clay + eta * clay_robots,
             obs  = obs  + eta * obs_robots ,
             geo  = geo  + eta * geo_robots ,
             ore_robots = ore_robots + 1) %>%
      select(-eta) %>%
      filter(time <= 32)
    
    geo_max <- max(geo_max, scenarios_ore$geo)
    scenarios_ore <- scenarios_ore %>% filter(time < 32)
    
    # build order: clay robot
    scenarios_clay <- scenarios %>%
      filter(clay_robots < blueprint$obs_rob_clay) %>%
      mutate(eta = ifelse(ore > blueprint$clay_rob_ore, 1, ceiling((blueprint$clay_rob_ore - ore)/ore_robots) + 1)) %>%
      mutate(time = time + eta,
             ore  = ore  + eta * ore_robots - blueprint$clay_rob_ore,
             clay = clay + eta * clay_robots,
             obs  = obs  + eta * obs_robots ,
             geo  = geo  + eta * geo_robots ,
             clay_robots = clay_robots + 1) %>%
      select(-eta) %>%
      filter(time <= 32)
    
    geo_max <- max(geo_max, scenarios_clay$geo)
    scenarios_clay <- scenarios_clay %>% filter(time < 32)
    
    # build order: obsidian robot
    # prereq: clay robot
    scenarios_obs <- scenarios %>%
      filter(clay_robots > 0) %>%
      mutate(eta1 = ifelse(ore  > blueprint$obs_rob_ore , 1, ceiling((blueprint$obs_rob_ore  - ore )/ore_robots ) + 1),
             eta2 = ifelse(clay > blueprint$obs_rob_clay, 1, ceiling((blueprint$obs_rob_clay - clay)/clay_robots) + 1),
             eta = pmax(eta1, eta2)) %>%
      mutate(time = time + eta,
             ore  = ore  + eta * ore_robots  - blueprint$obs_rob_ore,
             clay = clay + eta * clay_robots - blueprint$obs_rob_clay,
             obs  = obs  + eta * obs_robots ,
             geo  = geo  + eta * geo_robots ,
             obs_robots = obs_robots + 1) %>%
      select(-eta1, -eta2, -eta) %>%
      filter(time <= 32)
    
    geo_max <- max(geo_max, scenarios_obs$geo)
    scenarios_obs <- scenarios_obs %>% filter(time < 32)
    
    # build order: geode robot
    # prereq: obsidian robot
    scenarios_geo <- scenarios %>%
      filter(obs_robots > 0) %>%
      mutate(eta1 = ifelse(ore > blueprint$geo_rob_ore, 1, ceiling((blueprint$geo_rob_ore - ore)/ore_robots) + 1),
             eta2 = ifelse(obs > blueprint$geo_rob_obs, 1, ceiling((blueprint$geo_rob_obs - obs)/obs_robots) + 1),
             eta = pmax(eta1, eta2)) %>%
      mutate(time = time + eta,
             ore  = ore  + eta * ore_robots  - blueprint$geo_rob_ore,
             clay = clay + eta * clay_robots,
             obs  = obs  + eta * obs_robots  - blueprint$geo_rob_obs,
             geo  = geo  + eta * geo_robots ,
             geo_robots = geo_robots + 1) %>%
      select(-eta1, -eta2, -eta) %>%
      filter(time <= 32)
    
    geo_max <- max(geo_max, scenarios_geo$geo)
    scenarios_geo <- scenarios_geo %>% filter(time < 32)
    
    # built order: idle till t=24.
    # prereq: geode robot
    scenarios_idle <- scenarios %>%
      filter(geo_robots > 0) %>%
      mutate(eta = 32 - time) %>%
      mutate(time = time + eta,
             ore  = ore  + eta * ore_robots ,
             clay = clay + eta * clay_robots,
             obs  = obs  + eta * obs_robots ,
             geo  = geo  + eta * geo_robots ) %>%
      select(-eta)
    
    geo_max <- max(geo_max, scenarios_idle$geo)
    
    scenarios <- bind_rows(scenarios_ore, scenarios_clay, scenarios_obs, scenarios_geo)
    
    # a couple explicit dead end filters: 
    # any geode robot needs 7+ obsidian, so time >= 17 with 0 obsidian robots is a no-go. 
    # any obsidian robot needs 5+ clay, so time >= 12 with 0 clay robots is a no-go. 
    # any scenario where making a geode robot a turn won't get you to the goal anymore is a no-go.
    scenarios <- scenarios %>%
      filter(time < 32) %>%
      filter(!(time >= 20 & obs_robots == 0)) %>%
      filter(!(time >= 10 & clay_robots == 0)) %>%
      filter((32-time) * geo_robots + (32-time)*(33-time)/2 + geo > geo_max)
    
    #print(paste0(i,", goal - ",geo_max, ", scen - ",nrow(scenarios), ", min time ",min(scenarios$time)))
  }
  print(paste(i,geo_max))
  ans <- ans * geo_max
}

(ans)

ptm - proc.time()