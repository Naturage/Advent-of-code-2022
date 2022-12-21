source(here::here("common functions.R"))

# Data input
input <- get_input(18) %>% 
  sapply(str_split,pattern = ",") %>% 
  sapply(as.numeric) %>%
  unname %>% 
  t %>%
  tibble(x = .[,1], y = .[,2], z = .[,3]) %>%
  select(x,y,z)

nearby <- tibble(move_x = c(1,-1,0,0,0,0),
                 move_y = c(0,0,1,-1,0,0),
                 move_z = c(0,0,0,0,1,-1))

# P1
open_side <- input %>% 
  left_join(nearby, by = character()) %>%
  mutate(x = x + move_x,
         y = y + move_y,
         z = z + move_z) %>%
  anti_join(input, by = c("x","y","z"))

(ans <- nrow(open_side))

# P2
# Poor man's idea - if you're outside, within 20 (or more?) steps you should be able to reach a coordinate of 20+ in any direction
# Definitely not optimal but can't be bothered to think.

input %>% summarise(max(x),min(x),max(y),min(y),max(z),min(z))

nearby2 <- tibble(move_x = c(1,-1,0,0,0,0,0),
                  move_y = c(0,0,1,-1,0,0,0),
                  move_z = c(0,0,0,0,1,-1,0))

outside <- expand_grid(x = c(-1:21),y=c(-1:21),z=c(-1:21)) %>%
  filter(x %in% c(1:21) & y %in% c(1:21) & z %in% c(1:21))
outside_size <- nrow(outside)

while (TRUE){
  outside <- outside %>% 
    left_join(nearby2, by = character()) %>%
    mutate(x = x + move_x,
           y = y + move_y,
           z = z + move_z) %>%
    select(-starts_with("move")) %>%
    distinct(across(everything())) %>%
    anti_join(input, by = c("x","y","z")) %>%
    filter(x %in% -1:21 & y %in% -1:21 & z %in% -1:21)
  
  if (outside_size == nrow(outside)){
    break
  } else {
    outside_size <- nrow(outside)
    print(outside_size)
  }
}

open_outside_side <- open_side %>%
  semi_join(outside, by = c("x","y","z"))

(ans <- nrow(open_outside_side))