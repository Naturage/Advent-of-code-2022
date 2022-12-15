source(here::here("common functions.R"))

# Data input

input <- get_input(15)

input_clean <- input %>% 
  str_extract_all("\\d+") %>%
  tibble(sensor_x = sapply(.,function(x){x[1]}),
         sensor_y = sapply(.,function(x){x[2]}),
         beacon_x = sapply(.,function(x){x[3]}),
         beacon_y = sapply(.,function(x){x[4]})) %>%
  select(-1) %>%
  mutate(across(everything(),as.numeric))

# Pt 1
magic_row <- 2000000

p1_info <- input_clean %>% 
  mutate(distance_to_beacon = abs(sensor_x - beacon_x)+ 
                              abs(sensor_y - beacon_y),
         distance_on_line = distance_to_beacon - abs(sensor_x - magic_row)) %>%
  filter(distance_on_line > 0) %>%
  mutate(exclusion_2m_min = sensor_x - distance_on_line,
         exclusion_2m_max = sensor_x + distance_on_line) %>%
  select(exclusion_2m_min,exclusion_2m_max)

# and now, need to somehow dedupe.
for(i in 1:(nrow(p1_info)-1)){
  
  for(j in (i+1):nrow(p1_info)){
    testing <- cbind(p1_info %>% slice(i),
                     p1_info %>% slice(j))
  }
}