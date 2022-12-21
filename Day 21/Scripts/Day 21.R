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
worked_on <- input %>% 
  filter(!var %in% c("humn","root"))

finalised <- tibble()

last_size <- nrow(worked_on)+1

while (nrow(worked_on) < last_size){
  
  last_size <- nrow(worked_on)
  
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
             TRUE ~ op_var1
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

equality <- input %>% filter(var == "root")
eq_value <- finalised %>% filter(var == equality$op_var1 | var == equality$op_var2) %>% pull(final_value)
extra_value <- tibble(var = c(equality$op_var1, equality$op_var2),
                      final_value = c(eq_value,eq_value)) %>%
  anti_join(finalised, by = "var")

finalised <- finalised %>%
  add_row(extra_value)

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

(ans <- finalised %>% filter(var == "humn") %>% pull(final_value))