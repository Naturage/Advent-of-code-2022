source(here::here("common functions.R"))

# Data input

#input <- read_lines(here("day 15","data","input test.txt"))
input <- get_input(15)

input_clean <- input %>% 
  str_extract_all("(-)?\\d+") %>%
  tibble(sensor_x = sapply(.,function(x){x[1]}),
         sensor_y = sapply(.,function(x){x[2]}),
         beacon_x = sapply(.,function(x){x[3]}),
         beacon_y = sapply(.,function(x){x[4]})) %>%
  select(-1) %>%
  mutate(across(everything(),as.numeric))

# Pt 1
#magic_row <- 10
magic_row <- 2000000

p1_info <- input_clean %>% 
  mutate(distance_to_beacon = abs(sensor_x - beacon_x)+ 
                              abs(sensor_y - beacon_y),
         distance_on_line = distance_to_beacon - abs(sensor_y - magic_row)) %>%
  filter(distance_on_line > 0) %>%
  mutate(exclusion_2m_min = sensor_x - distance_on_line,
         exclusion_2m_max = sensor_x + distance_on_line) %>%
  select(exclusion_2m_min,exclusion_2m_max) %>% 
  arrange(exclusion_2m_min)

for (i in 1:(nrow(p1_info)-1)){
  i1 <- p1_info$exclusion_2m_min[i]
  i2 <- p1_info$exclusion_2m_max[i]
  j1 <- p1_info$exclusion_2m_min[i+1]
  j2 <- p1_info$exclusion_2m_max[i+1]
  # i1 <= i2 by definition.
  if (i2 >= j1 - 1){
    p1_info$exclusion_2m_min[i+1] <- i1
    p1_info$exclusion_2m_max[i+1] <- max(i2,j2)
    p1_info$exclusion_2m_min[i] <- NA
    p1_info$exclusion_2m_max[i] <- NA
  }
}

(ans <- p1_info %>% 
    filter(!is.na(exclusion_2m_min)) %>% 
    mutate(diff = exclusion_2m_max - exclusion_2m_min + 1) %>%
    summarise(ans = sum(diff)) %>%
    pull(ans) - 
    input_clean %>% 
      distinct(beacon_x,beacon_y) %>% 
      filter(beacon_y == magic_row) %>% 
      nrow())

# P2
# TODO: magic.
# t = x+y, u=x-y.
# Initial condition becomes t+u between 0 and 8M, t-u between 0 and 8M.
p2_info <- input_clean %>% 
  mutate(distance_to_beacon = abs(sensor_x - beacon_x)+ 
                              abs(sensor_y - beacon_y)) %>%
  mutate(t_min  = sensor_x + sensor_y - distance_to_beacon,
         t_max  = sensor_x + sensor_y + distance_to_beacon,
         u_min = sensor_x - sensor_y - distance_to_beacon,
         u_max = sensor_x - sensor_y + distance_to_beacon)

# Given there's a single point that works, it has to be dist+1 from at least 2 points (I think 3, but assume 2).
# Find every intersection of empty rhombi.

poss_t <- c()
poss_u <- c()

for (i in 1:(nrow(p2_info)-1)){
  for (j in (i+1):nrow(p2_info)){
    t_min_1 <- p2_info$t_min[i] - 1
    t_min_2 <- p2_info$t_min[j] - 1
    t_max_1 <- p2_info$t_max[i] + 1
    t_max_2 <- p2_info$t_max[j] + 1
    u_min_1 <- p2_info$u_min[i] - 1
    u_min_2 <- p2_info$u_min[j] - 1
    u_max_1 <- p2_info$u_max[i] + 1
    u_max_2 <- p2_info$u_max[j] + 1
    duplirows <- c(t_min_1,t_min_2,t_max_1,t_max_2) %>% .[duplicated(.)]
    if (length(duplirows) > 0){
      print(paste0("t possibly ",duplirows))
      poss_t <- c(poss_t, duplirows)
    }
    duplicols <- c(u_min_1,u_min_2,u_max_1,u_max_2) %>% .[duplicated(.)]
    if (length(duplicols) > 0){
      print(paste0("u possibly ",duplicols))
      poss_u <- c(poss_u, duplicols)
    }
  }
}

poss_points <- expand.grid(t = unique(poss_t), u = unique(poss_u)) %>%
  filter(t %% 2 == u %% 2) %>% # integer x,y
  mutate(x = (t+u)/2,
         y = (t-u)/2) %>%
  filter(x >= 0 & y >= 0 & x <= 4000000 & y <= 4000000) %>%
  select(secret_x = x,secret_y = y)

options(digits=22)

(ans <- p2_info %>% 
  select(sensor_x, sensor_y, distance_to_beacon) %>%
  left_join(poss_points, by = character()) %>%
  mutate(distance_to_secret = abs(sensor_x - secret_x) + 
                              abs(sensor_y - secret_y)) %>%
  mutate(far_enough = (distance_to_secret > distance_to_beacon)) %>%
  group_by(secret_x, secret_y) %>%
  summarise(sensors_evaded = sum(far_enough), .groups = "drop") %>%
  filter(sensors_evaded == nrow(p2_info)) %>%
  mutate(ans = 4000000 * secret_x + secret_y) %>%
  pull(ans))