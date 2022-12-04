source(here::here("common functions.R"))

# Data input
input <- get_input(4)

parsed_input <- tibble(raw = input) %>% 
  separate(raw, c("elf_1_start","elf_1_end","elf_2_start","elf_2_end"), sep = "-|,") %>%
  mutate(across(everything(), as.numeric))

#Pt 1
(parsed_input %>% 
  mutate(within = (((elf_1_start >= elf_2_start) & (elf_1_end <= elf_2_end))|
                     ((elf_1_start <= elf_2_start) & (elf_1_end >= elf_2_end)))) %>%
  summarise(ans = sum(within)) %>%
  pull(ans))

#Pt 2
(parsed_input %>% 
    mutate(overlap = !((elf_1_start > elf_2_end)|(elf_2_start > elf_1_end))) %>%
    summarise(ans = sum(overlap)) %>%
    pull(ans))