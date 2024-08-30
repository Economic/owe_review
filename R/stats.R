create_paper_stats_csv = function(data, ns_dz_data, file_name) {

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
    mutate(name = "Median OWE, published, broad group")
  
  median_narrow = data |> 
    filter(published == 1, overall != 1) |> 
    summarize(value = median(owe_b)) |> 
    mutate(value = label_number(accuracy = 0.01)(value)) |> 
    mutate(name = "Median OWE, published, narrow group")
  
  median_us = data |> 
    filter(published == 1, country == "US") |> 
    summarize(value = median(owe_b)) |> 
    mutate(value = label_number(accuracy = 0.01)(value)) |> 
    mutate(name = "Median OWE, published, United States")
  
  mean_us = data |> 
    filter(published == 1, country == "US") |> 
    summarize(value = mean(owe_b)) |> 
    mutate(value = label_number(accuracy = 0.01)(value)) |> 
    mutate(name = "Mean OWE, published, United States")
  
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
    mutate(name = "Share of published studies with OWE >= -0.4")
  
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
    mutate(name = "Number of published studies, narrow groups")
  
  number_studies_broad = data |> 
    filter(published == 1, overall == 1) |> 
    count() |> 
    mutate(value = as.character(n)) |> 
    mutate(name = "Number of published studies, broad groups")
  
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
      overall == 1 ~ "Median OWE in year >= 2010, published, broad group",
      overall != 1 ~ "Median OWE in year >= 2010, published, narrow group"
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
    mutate(name = "Number of studies, published, restaurants/retail")
  
  number_studies_teen = data |> 
    filter(published == 1, teens == 1) |> 
    count() |> 
    mutate(value = as.character(n)) |> 
    mutate(name = "Number of studies, published, teenagers")
  
  median_teen = data |> 
    filter(published == 1, teens == 1) |> 
    summarize(value = median(owe_b)) |> 
    mutate(value = label_number(accuracy = 0.01)(value)) |> 
    mutate(name = "Median OWE, published, teens")
  
  mean_teen = data |> 
    filter(published == 1, teens == 1) |> 
    summarize(value = mean(owe_b)) |> 
    mutate(value = label_number(accuracy = 0.01)(value)) |> 
    mutate(name = "Mean OWE, published, teens")
  
  median_rr = data |> 
    filter(published == 1, restaurants_retail == 1) |> 
    summarize(value = median(owe_b)) |> 
    mutate(value = label_number(accuracy = 0.01)(value)) |> 
    mutate(name = "Median OWE, published, restaurants/retail")
  
  mean_rr = data |> 
    filter(published == 1, restaurants_retail == 1) |> 
    summarize(value = mean(owe_b)) |> 
    mutate(value = label_number(accuracy = 0.01)(value)) |> 
    mutate(name = "Mean OWE, published, restaurants/retail")
  
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
    mutate(name = paste("Number of published studies", period, size))
  
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
      name = paste("Number of published studies,", decade),
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
      name = paste(name, "OWE, published,", decade),
      value = label_number(accuracy = 0.01)(value)
    ) 
  
  data_compare_decades = data |> 
    filter(
      published == 1,
      year %in% c(2000:2009, 2020:2024)
    ) |> 
    mutate(post = if_else(year >= 2020, 1, 0))
  
  model_lm = feols(owe_b ~ post, data = data_compare_decades)
  model_rq = rq(owe_b ~ post, data = data_compare_decades, tau = 0.5)
  rq_p = tidy(model_rq, se.type = "nid", conf.int = T) |> 
    filter(term == "post") |> 
    pull(p.value)
  lm_p = tidy(model_lm, se = "HC1", conf.int = T) |> 
    filter(term == "post") |> 
    pull(p.value)
  
  mean_median_reg_tests = tribble(
    ~name, ~value,
    "Null: equality of means  , 2000-2009 and 2020-2024, p-value", lm_p,
    "Null: equality of medians, 2000-2009 and 2020-2024, p-value", rq_p
  ) |> 
    mutate(value = label_number(accuracy = 0.001)(value))

  data_compare_2010 = data |> 
    filter(published == 1) |> 
    mutate(
      medium_large = if_else(owe_b < -0.4, 1, 0),
      post = if_else(year >= 2010, 1, 0)
    )
  
  medium_large_2010_p = feols(medium_large ~ post, data = data_compare_2010) |> 
    broom::tidy(se = "HC1", conf.int = T) |> 
    filter(term == "post") |> 
    pull(p.value)
  
  medium_large_2010_test = tibble(
    name = "Null: equality of medium/large negative share, before and after 2010, p-value ",
    value = medium_large_2010_p
  ) |> 
    mutate(value = label_number(accuracy = 0.001)(value))
  
  ns_positive_count_share = ns_dz_data |> 
    filter(!is.na(ns_study_id)) |> 
    summarize(
      `NS studies, total number` = as.character(n()),
      `NS studies, number positive` = as.character(sum(ns_pos)),
      `NS studies, share positive` = label_percent(accuracy=1)(mean(ns_pos))
    ) |> 
    pivot_longer(everything())
  
  dz_positive_count_share = ns_dz_data |> 
    filter(!is.na(dz_study_id)) |> 
    filter(dz_published == 1, dz_country == "US") |> 
    summarize(
      `DZ studies US published, total number` = as.character(n()),
      `DZ studies US published, number positive` = as.character(sum(dz_pos)),
      `DZ studies US published, share positive` = label_percent(accuracy=1)(mean(dz_pos))
    ) |> 
    pivot_longer(everything())
  
  dz_ns_superset_count = ns_dz_data |> 
    filter(!is.na(ns_elast) | (dz_published == 1 & dz_country == "US")) |> 
    count() |> 
    mutate(value = as.character(n)) |> 
    mutate(name = "DZ NS superset, number of studies")
    
  number_studies_ns_dz_overlap = ns_dz_data %>% 
    filter(!is.na(dz_study_id), !is.na(ns_study_id)) %>% 
    count() %>% 
    mutate(value = as.character(n)) %>% 
    mutate(name = "Number of studies, NS DZ intersection")
  
  median_owe_ns_dz_overlap = ns_dz_data %>% 
    filter(!is.na(dz_study_id), !is.na(ns_study_id)) %>% 
    summarize(value = median(dz_owe)) %>% 
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
      medium_large_2010_test,
      negative_owe_share, negative_owe_share_us_2021,
      ns_positive_count_share, dz_positive_count_share,
      dz_ns_superset_count,
      number_studies_ns_dz_overlap,
      median_owe_ns_dz_overlap
    ) |> 
    select(name, value)
  
  write_csv(results, file_name)
  
  file_name
}
