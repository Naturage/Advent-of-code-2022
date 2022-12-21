source(here::here("common functions.R"))

# Data input
input <- get_input(21) %>%
  tibble(raw = .) %>% 
  separate(raw, into = c("var", "rest"), sep = ": ") %>%
  mutate(final_value = str_extract(rest,"\\d+") %>% as.numeric) %>%
  separate(rest, into = c("op_var1","op","op_var2"), sep = " ")

# P1
worked_on <- input
finalised <- tibble()

while (nrow(worked_on > 0)){
  finalised <- worked_on %>% 
    filter(!is.na(final_value)) %>%
    select(var, final_value) %>%
    bind_rows(finalised)
  
  worked_on <- worked_on %>% 
    anti_join(finalised, by = "var") %>% 
    select(var,op_var1,op,op_var2) %>%
    left_join(finalised %>% 
              rename(op_var1_value = final_value), 
              by = c("op_var1" = "var")) %>%
    left_join(finalised %>% 
              rename(op_var2_value = final_value), 
              by = c("op_var2" = "var")) %>%
    mutate(final_value = case_when(
      op == "+" ~ op_var1_value + op_var2_value,
      op == "-" ~ op_var1_value - op_var2_value,
      op == "*" ~ op_var1_value * op_var2_value,
      op == "/" ~ op_var1_value / op_var2_value))
}

options(digits = 22)
(ans <- finalised %>% filter(var == "root") %>% pull(final_value))

# P2
input %>% filter(var == "humn")

worked_on <- input %>% 
  mutate(op = ifelse(var == "root","-",op)) %>%
  filter(var != "humn")

finalised <- tibble()

while (nrow(worked_on) >72){ #checked by hand this is where it stops.
  finalised <- worked_on %>% 
    filter(!is.na(final_value)) %>%
    select(var, final_value) %>%
    bind_rows(finalised)
  
  worked_on <- worked_on %>% 
    anti_join(finalised, by = "var") %>% 
    select(var,op_var1,op,op_var2) %>%
    left_join(finalised %>% 
                rename(op_var1_value = final_value), 
              by = c("op_var1" = "var")) %>%
    left_join(finalised %>% 
                rename(op_var2_value = final_value), 
              by = c("op_var2" = "var")) %>%
    mutate(final_value = case_when(
      op == "+" ~ op_var1_value + op_var2_value,
      op == "-" ~ op_var1_value - op_var2_value,
      op == "*" ~ op_var1_value * op_var2_value,
      op == "/" ~ op_var1_value / op_var2_value))
}

# flip the table around.
worked_on_2 <- worked_on %>%
  filter(var != "root") %>%
  mutate(var_new = ifelse(is.na(op_var1_value),op_var1,op_var2),
         op_var1_new = 
           case_when(
             op %in% c("+","*") ~ var,
             is.na(op_var1_value) ~ var,
             TRUE ~ op_var2
           ),
         op_var2_new = 
           case_when(
             op %in% c("+","*") & is.na(op_var1_value) ~ op_var2,
             op %in% c("+","*") ~ op_var1,
             is.na(op_var1_value) ~ op_var2,
             TRUE ~ var
           ),
         op_new = case_when(
           op == "+" ~ "-",
           op == "*" ~ "/",
           op == "-" & is.na(op_var1_value) ~ "+",
           op == "-" ~ "-",
           op == "/" & is.na(op_var1_value) ~ "*",
           op == "/" ~ "/"
         )) %>%
  select(var = var_new,
         op_var1 = op_var1_new,
         op_var2 = op_var2_new,
         op = op_new)

finalised <- finalised %>%
  add_row(worked_on %>% 
            filter(var == "root") %>% 
            mutate(final_value = coalesce(op_var1_value,op_var2_value),
                   var = ifelse(is.na(op_var1_value),op_var1,op_var2)) %>%
            select(var,final_value))

worked_on <- worked_on_2 %>% mutate(final_value = NA)

while (nrow(worked_on) > 0){
  finalised <- worked_on %>% 
    filter(!is.na(final_value)) %>%
    select(var, final_value) %>%
    bind_rows(finalised)
  
  worked_on <- worked_on %>% 
    anti_join(finalised, by = "var") %>% 
    select(var,op_var1,op,op_var2) %>%
    left_join(finalised %>% 
                rename(op_var1_value = final_value), 
              by = c("op_var1" = "var")) %>%
    left_join(finalised %>% 
                rename(op_var2_value = final_value), 
              by = c("op_var2" = "var")) %>%
    mutate(final_value = case_when(
      op == "+" ~ op_var1_value + op_var2_value,
      op == "-" ~ op_var1_value - op_var2_value,
      op == "*" ~ op_var1_value * op_var2_value,
      op == "/" ~ op_var1_value / op_var2_value))
}
