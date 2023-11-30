grab_owe_data <- function(csv, download_date) {
  read_csv(csv) %>% 
    mutate(download_date = download_date) 
}

summary_df_row <- function(data, filter_string) {
  data %>% 
    filter(eval(rlang::parse_expr(filter_string))) %>% 
    summarize(
      mean = mean(owe_b),
      median = median(owe_b),
      large_negative = mean(owe_magnitude == "Large negative"),
      medium_negative = mean(owe_magnitude == "Medium negative"),
      small_negative = mean(owe_magnitude == "Small negative"),
      zero_positive = mean(owe_magnitude == "Positive"),
      count = n()
    )
}

summary_df <- function(data) {
  
  data <- data %>% 
    mutate(all = 1)
  
  groups <- c(
    "All studies" = "all == 1",
    "United States-only" = "country == 'US'",
    "Other countries" = "country != 'US'",
    "Broad group / high-recall" = "overall == 1",
    "Restaurants or retail" = "restaurants_retail == 1",
    "Teenagers" = "teens == 1",
    "Non-teenagers" = "teens != 1",
    "Published before 2010" = "year < 2010",
    "Published since 2010" = "year >= 2010",
    "Authors reported OWE" = "owe_reported == 1"
  )
  
  map(groups, ~ summary_df_row(data, .x)) %>% 
    list_rbind(names_to = "category")
  
}
