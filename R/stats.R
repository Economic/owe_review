create_paper_stats_csv = function(data, file_name) {
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
      median_narrow
    ) |> 
    select(name, value)
  
  write_csv(results, file_name)
  
  file_name
}