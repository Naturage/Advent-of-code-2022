source(here::here("common functions.R"))

# Data input
input <- get_input(7)

# input <- read_lines(here("day 7","data","input example.txt"))
# Parsing row by row.
input_parsed <- tibble(parent = "", 
                       type = "D", 
                       filename = "-/", 
                       size = NA)

parent = ""
for (line in input){
  if(grepl("^\\$ cd",line)){
    parent_tmp <- str_match(line,"^\\$ cd ([a-zA-z/\\.]+)")[,2]
    if ((parent_tmp == "..")){
      parent <- str_replace(parent,"-[a-zA-Z]+$","")
    } else {
      parent <- paste0(parent,"-",parent_tmp)
    }
  } else if (grepl("^\\$ ls",line)){
    # do nothing
  } else if (grepl("^dir",line)){
    filename <- str_match(line,"^dir ([a-zA-z\\.]+)")[,2] %>%
      paste0(parent,"-",.)
    input_parsed <- add_row(input_parsed,
                            parent = parent,
                            type = "D",
                            filename = filename,
                            size = NA)
  } else{
    size <- str_match(line,"^(\\d+) ([a-zA-z\\.]+)")[,2] %>% as.numeric
    filename <- str_match(line,"^(\\d+) ([a-zA-z\\.]+)")[,3]
    input_parsed <- add_row(input_parsed,
                            parent = parent,
                            type = "F",
                            filename = filename,
                            size = size)
  }
} 

# Find directory sizes
while (is.na(input_parsed %>% filter(filename == "-/") %>% pull(size))){
  print(input_parsed %>% filter(is.na(size)) %>% nrow())
  sizes <- input_parsed %>% 
    group_by(parent) %>%
    summarise(dir_size = sum(size)) %>%
    filter(!is.na(dir_size)) %>%
    mutate(type = "D")
  
  input_parsed <- input_parsed %>%
    left_join(sizes, by = c("filename" = "parent","type")) %>%
    mutate(size = coalesce(size,dir_size)) %>%
    select(-dir_size)
  
}

# Pt 1
(sizes %>% filter(dir_size<100000) %>% summarise(ans = sum(dir_size)) %>% pull(ans))

# Pt2
(sizes %>% filter(dir_size>max(dir_size)-40000000) %>% summarise(ans = min(dir_size)) %>% pull(ans))