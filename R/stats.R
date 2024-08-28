create_paper_stats_csv = function(data, ns_dz_matches, file_name) {

  median_owe_all = data |> 
    summarize(value = median(owe_b)) |> 
    mutate(value = label_number(accuracy = 0.01)(value)) |> 
    mutate(name = "Median OWE (all studies)")
  
  median_owe = data |> 
    filter(published == 1) |> 
    summarize(value = median(owe_b)) |> 
    mutate(value = label_number(accuracy = 0.01)(value)) |> 
    mutate(name = "Median OWE (published studies)")
  
  mean_owe = data |> 
    filter(published == 1) |> 
    summarize(value = mean(owe_b)) |> 
    mutate(value = label_number(accuracy = 0.01)(value)) |> 
    mutate(name = "Mean OWE (published studies)")
  
  mean_owe_all = data |> 
    summarize(value = mean(owe_b)) |> 
    mutate(value = label_number(accuracy = 0.01)(value)) |> 
    mutate(name = "Mean OWE (all studies)")
  
  median_broad = data |> 
    filter(published == 1, overall == 1) |> 
    summarize(value = median(owe_b)) |> 
    mutate(value = label_number(accuracy = 0.01)(value)) |> 
    mutate(name = "Median OWE, broad group")
  
  median_narrow = data |> 
    filter(published == 1, overall != 1) |> 
    summarize(value = median(owe_b)) |> 
    mutate(value = label_number(accuracy = 0.01)(value)) |> 
    mutate(name = "Median OWE, narrow group")
  
  median_us = data |> 
    filter(published == 1, country == "US") |> 
    summarize(value = median(owe_b)) |> 
    mutate(value = label_number(accuracy = 0.01)(value)) |> 
    mutate(name = "Median OWE, United States")
  
  mean_us = data |> 
    filter(published == 1, country == "US") |> 
    summarize(value = mean(owe_b)) |> 
    mutate(value = label_number(accuracy = 0.01)(value)) |> 
    mutate(name = "Mean OWE, United States")
  
  count_studies_all = data |> 
    count() |> 
    mutate(value = as.character(n)) |> 
    mutate(name = "Number of studies, all studies")
  
  count_studies_published = data |> 
    filter(published == 1) |> 
    count() |> 
    mutate(value = as.character(n)) |> 
    mutate(name = "Number of studies, published studies")
  
  share_studies_more_pos = data |> 
    filter(published == 1) |> 
    mutate(more_positive = owe_b >= -0.4) |> 
    summarize(value = mean(more_positive)) |> 
    mutate(value = label_percent(accuracty = 1)(value)) |> 
    mutate(name = "Share of studies with OWE >= -0.4")
  
  number_journals = data |> 
    filter(published == 1) |> 
    distinct(journal) |> 
    count() |> 
    mutate(value = as.character(n)) |> 
    mutate(name = "Number of journals in repository")
  
  journals_with_at_least_4 = data |> 
    filter(published == 1) |> 
    count(journal) |> 
    filter(n >= 4) |> 
    arrange(desc(n)) |> 
    mutate(name = paste0("Journal with at least 4 studies #", row_number())) |> 
    mutate(value = paste0(journal, " (", n, ")"))
  
  averaged_estimates_all = data |> 
    filter(averaged == 1) |> 
    count() |> 
    mutate(value = as.character(n)) |> 
    mutate(name = "Studies with average of multiple estimates, all")
  
  averaged_estimates_published = data |> 
    filter(published == 1, averaged == 1) |> 
    count() |> 
    mutate(value = as.character(n)) |> 
    mutate(name = "Studies with average of multiple estimates, published")
  
  owe_reported_all = data |> 
    filter(owe_reported == 1) |> 
    count() |> 
    mutate(value = as.character(n)) |> 
    mutate(name = "Studies reporting OWE, all")
  
  owe_reported_published = data |> 
    filter(published == 1, owe_reported == 1) |> 
    count() |> 
    mutate(value = as.character(n)) |> 
    mutate(name = "Studies reporting OWE, published")
  
  no_owe_se_all = data |> 
    filter(is.na(owe_se)) |> 
    count() |> 
    mutate(value = as.character(n)) |> 
    mutate(name = "Studies without SE, all")
  
  no_owe_se_published = data |> 
    filter(published == 1, is.na(owe_se)) |> 
    count() |> 
    mutate(value = as.character(n)) |> 
    mutate(name = "Studies without SE, published")
  
  published_since_2010 = data |> 
    filter(published == 1) |> 
    mutate(since_2010 = year >= 2010) |> 
    summarize(
      "Published since 2010, count" = as.character(sum(since_2010)),
      "Published since 2010, share" = label_percent(accuarcy = 1)(mean(since_2010))
    ) |> 
    pivot_longer(everything())
  
  number_studies_narrow = data |> 
    filter(published == 1, overall != 1) |> 
    count() |> 
    mutate(value = as.character(n)) |> 
    mutate(name = "Number of studies, narrow groups")
  
  number_studies_broad = data |> 
    filter(published == 1, overall == 1) |> 
    count() |> 
    mutate(value = as.character(n)) |> 
    mutate(name = "Number of studies, broad groups")
  
  broad_owe_2020_share = data %>% 
    filter(published == 1, overall == 1) %>% 
    mutate(after_2020 = year >= 2020) %>% 
    summarize(value = mean(after_2020)) %>% 
    mutate(
      value = label_percent(accuarcy = 1)(value),
      name = "Share of broad studies published since 2020"
    )
  
  narrow_owe_2020_share = data %>% 
    filter(published == 1, overall != 1) %>% 
    mutate(after_2020 = year >= 2020) %>% 
    summarize(value = mean(after_2020)) %>% 
    mutate(
      value = label_percent(accuarcy = 1)(value),
      name = "Share of narrow studies published since 2020"
    )
  
  owe_2010_broad_v_narrow = data %>% 
    filter(published == 1, year >= 2010) %>% 
    summarize(value = median(owe_b), .by = overall) %>% 
    mutate(value = label_number(accuracy = 0.01)(value)) %>% 
    mutate(name = case_when(
      overall == 1 ~ "Median OWE in year >= 2010, broad group",
      overall != 1 ~ "Median OWE in year >= 2010, narrow group"
    )) 
  
  number_studies_2010_broad_v_narrow = data %>% 
    filter(published == 1, year >= 2010) %>% 
    count(overall) %>% 
    mutate(value = as.character(n)) %>% 
    mutate(name = case_when(
      overall == 1 ~ "Number of published studies in year >= 2010, broad group",
      overall != 1 ~ "Number of published studies in year >= 2010, narrow group"
    )) 
  
  number_studies_rr = data |> 
    filter(published == 1, restaurants_retail == 1) |> 
    count() |> 
    mutate(value = as.character(n)) |> 
    mutate(name = "Number of studies, restaurants/retail")
  
  number_studies_teen = data |> 
    filter(published == 1, teens == 1) |> 
    count() |> 
    mutate(value = as.character(n)) |> 
    mutate(name = "Number of studies, teenagers")
  
  median_teen = data |> 
    filter(published == 1, teens == 1) |> 
    summarize(value = median(owe_b)) |> 
    mutate(value = label_number(accuracy = 0.01)(value)) |> 
    mutate(name = "Median OWE, teens")
  
  mean_teen = data |> 
    filter(published == 1, teens == 1) |> 
    summarize(value = mean(owe_b)) |> 
    mutate(value = label_number(accuracy = 0.01)(value)) |> 
    mutate(name = "Mean OWE, teens")
  
  median_rr = data |> 
    filter(published == 1, restaurants_retail == 1) |> 
    summarize(value = median(owe_b)) |> 
    mutate(value = label_number(accuracy = 0.01)(value)) |> 
    mutate(name = "Median OWE, restaurants/retail")
  
  mean_rr = data |> 
    filter(published == 1, restaurants_retail == 1) |> 
    summarize(value = mean(owe_b)) |> 
    mutate(value = label_number(accuracy = 0.01)(value)) |> 
    mutate(name = "Mean OWE, restaurants/retail")
  
  negative_owe_share = data %>% 
    filter(published == 1) %>% 
    summarize(value = mean(owe_b < 0)) %>% 
    mutate(value = label_percent(accuracy = 1)(value)) %>% 
    mutate(name = "Share of published studies with negative OWE")
  
  negative_owe_share_us_2021 = data %>% 
    filter(published == 1, country == "US", year <= 2021) %>% 
    summarize(value = mean(owe_b < 0)) %>% 
    mutate(value = label_percent(accuracy = 1)(value)) %>% 
    mutate(name = "Share of published US studies in 2021 or before with negative OWE")
  
  size_by_period = data %>% 
    filter(published == 1) %>% 
    mutate(period = if_else(year >= 2010, "Post-2010", "Pre-2010")) %>% 
    mutate(size = if_else(
      owe_magnitude %in% c("Large negative", "Medium negative"), 
      "large/medium negative",
      "positive or small negative"
    )) %>% 
    count(period, size) %>% 
    mutate(value = as.character(n)) %>% 
    mutate(name = paste("Number of studies", period, size))
  
  decade_data = data %>% 
    mutate(decade = case_when(
      year >= 1990 & year < 2000 ~ "1992 - 1999",
      year >= 2000 & year < 2010 ~ "2000 - 2009",
      year >= 2010 & year < 2020 ~ "2010 - 2019",
      year >= 2020 ~ "2020 - 2024"
    )) 
  
  number_studies_by_decade = decade_data %>% 
    filter(published == 1) %>% 
    count(decade) %>% 
    mutate(
      name = paste("Number of studies,", decade),
      value = as.character(n)
    )
  
  mean_median_by_decade = decade_data %>% 
    filter(published == 1) %>% 
    summarize(
      Median = median(owe_b),
      Mean = mean(owe_b),
      .by = decade
    ) %>% 
    pivot_longer(-decade) %>% 
    mutate(
      name = paste(name, "OWE,", decade),
      value = label_number(accuracy = 0.01)(value)
    ) 
  
  data_2000_2020 = decade_data |> 
    filter(published == 1) |> 
    filter(decade %in% c("2000 - 2009", "2020 - 2024"))
  
  model_lm = lm_robust(owe_b ~ decade - 1, data = data_2000_2020, se_type = "HC1")
  model_rq = rq(owe_b ~ decade - 1, data = data_2000_2020, tau = 0.5)
  model_rq_vcov = summary(model_rq, se = "nid", covariance = T)$cov
  null_hypothesis = "`decade2000 - 2009` = `decade2020 - 2024`"
  data_2000_2020_df = nrow(data_2000_2020) - 2
  model_rq_test = hypotheses(model_rq, null_hypothesis, df = data_2000_2020_df, vcov = model_rq_vcov)
  model_lm_test = hypotheses(model_lm, null_hypothesis, df = data_2000_2020_df)
  
  mean_median_reg_tests = tribble(
    ~name, ~value,
    "Difference of means  , 2000-2009 and 2020-2024, p-value", model_lm_test$p,
    "Difference of medians, 2000-2009 and 2020-2024, p-value", model_rq_test$p
  ) |> 
    mutate(value = label_number(accuracy = 0.001)(value))
    
  number_studies_ns_dz_overlap = ns_dz_matches %>% 
    filter(!is.na(dz_study)) %>% 
    count() %>% 
    mutate(value = as.character(n)) %>% 
    mutate(name = "Number of studies, NS DZ intersection")
  
  median_owe_ns_dz_overlap = ns_dz_matches %>% 
    filter(!is.na(dz_study)) %>% 
    inner_join(data, by = join_by(dz_study == study_id)) %>% 
    summarize(value = median(owe_b)) %>% 
    mutate(value = label_number(accuracy = 0.01)(value)) %>% 
    mutate(name = "Median OWE, NS DZ intersection")
  
  results = bind_rows(
      median_owe, mean_owe, median_broad,
      count_studies_all, count_studies_published,
      share_studies_more_pos,
      number_journals, journals_with_at_least_4,
      averaged_estimates_all, averaged_estimates_published,
      owe_reported_all, owe_reported_published,
      published_since_2010,
      no_owe_se_all, no_owe_se_published,
      median_owe_all, mean_owe_all,
      median_us, mean_us,
      number_studies_narrow, number_studies_broad,
      median_narrow,
      broad_owe_2020_share, narrow_owe_2020_share,
      owe_2010_broad_v_narrow, number_studies_2010_broad_v_narrow,
      number_studies_rr, number_studies_teen,
      median_teen, mean_teen, median_rr, mean_rr,
      size_by_period,
      number_studies_by_decade, mean_median_by_decade, mean_median_reg_tests,
      negative_owe_share, negative_owe_share_us_2021,
      number_studies_ns_dz_overlap,
      median_owe_ns_dz_overlap
    ) |> 
    select(name, value)
  
  write_csv(results, file_name)
  
  file_name
}
