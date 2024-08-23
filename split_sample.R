create_stats_df <- function(data, time_period) {
  obs_count <- nrow(data)
  
  data %>% 
    summarize(mean = mean(owe_b), median = median(owe_b)) %>% 
    mutate(period = time_period) %>% 
    pivot_longer(-period) %>% 
    mutate(study_count = obs_count)
}

split_sample <- function(i) {
  data <- owe_data %>% 
    filter(published == 1)
  
  before_data <- data %>% 
    filter(year <= i) %>% 
    create_stats_df("before")
  
  after_data <- data %>% 
    filter(year > i) %>% 
    create_stats_df("after")
  
  before_data %>% 
    bind_rows(after_data) %>% 
    mutate(year_cut = i) 
}

owe_data %>% 
  pull(year) %>% 
  unique() %>% 
  map(split_sample) %>% 
  list_rbind() %>% 
  filter(name == "median") %>% 
  ggplot(aes(x = year_cut, y = value, color = period)) + 
  geom_step() + 
  geom_point() + 
  geom_text(aes(label = study_count), nudge_y = -0.03, nudge_x = -0.2)



