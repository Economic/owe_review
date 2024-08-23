grab_owe_data <- function(csv, download_date) {
  read_csv(csv, show_col_types = FALSE) %>% 
    mutate(download_date = download_date) 
}

summary_df_row <- function(data, filter_string, weighted = FALSE) {
  filtered_data <- data %>% 
    filter(eval(rlang::parse_expr(filter_string))) 
  
  if (weighted) {
    filtered_data %>% 
      filter(!is.na(owe_se)) %>% 
      mutate(weight = 1 / (owe_se^2)) %>%
      summarize(
        mean = weighted.mean(owe_b, w = weight),
        median = MetricsWeighted::weighted_median(owe_b, w = weight),
        count = n()
      )
  }
  
  else {
    filtered_data %>% 
      summarize(
        mean = mean(owe_b),
        median = median(owe_b),
        count = n()
      )
  }

  
  # filtered_data %>% 
  #   summarize(
  #     mean = mean(owe_b),
  #     median = median(owe_b),
  #     large_negative = mean(owe_magnitude == "Large negative"),
  #     medium_negative = mean(owe_magnitude == "Medium negative"),
  #     small_negative = mean(owe_magnitude == "Small negative"),
  #     zero_positive = mean(owe_magnitude == "Positive"),
  #     count = n(),
  #   )
  
  # filtered_data %>% 
  #   filter(!is.na(owe_se)) %>% 
  #   mutate(weight = 1 / (owe_se^2)) %>%
  #   summarize(
  #     weighted_mean = weighted.mean(owe_b, w = weight),
  #     weighted_median = MetricsWeighted::weighted_median(owe_b, w = weight),
  #     weighted_count = n()
  #   ) %>% 
  #   bind_cols(unweighted)
  
}

summary_df <- function(data) {
  
  data <- data %>% 
    mutate(all = 1)
  
  groups <- c(
    "All studies" = "all == 1",
    "United States-only" = "country == 'US'",
    "Other countries" = "country != 'US'",
    "Overall / broad group" = "overall == 1",
    "Narrow group" = "overall != 1",
    "Restaurants or retail" = "restaurants_retail == 1",
    "Teenagers" = "teens == 1",
    "Published before 2010" = "year < 2010",
    "Published since 2010" = "year >= 2010",
    "Authors reported OWE" = "owe_reported == 1"
  )
  
  weighted_estimate <- summary_df_row(data, "all == 1", weighted = TRUE) %>% 
    mutate(category = "Precision-weighted")
  
  map(groups, ~ summary_df_row(data, .x)) %>% 
    list_rbind(names_to = "category") %>% 
    bind_rows(weighted_estimate)
  
}
