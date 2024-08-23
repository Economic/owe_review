data <- tar_read(owe_data) %>% 
  mutate(inverse_var = owe_se^-2) %>% 
  filter(!is.na(inverse_var)) %>% 
  arrange(desc(inverse_var)) %>% 
  mutate(id = row_number())

filter_median <- function(i) {
  data <- data %>% 
    filter(id > i) 
  
  if (nrow(data) > 0) {
    data %>% 
      summarize(
        excl_w_median = MetricsWeighted::weighted_median(owe_b, w = inverse_var),
        excl_median = median(owe_b)
      ) %>% 
        mutate(id = i)
  }
    
}

map(1:nrow(data), filter_median) %>% 
  list_rbind() %>% 
  full_join(data) %>% 
  select(study, owe_b, owe_se, inverse_var, matches("excl")) %>% 
  print(n=Inf)