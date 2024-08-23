create_paper_stats_csv = function(data, file_name) {
  
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
  
  median_broad = data |> 
    filter(published == 1) |> 
    summarize(value = median(owe_b)) |> 
    mutate(value = label_number(accuracy = 0.01)(value)) |> 
    mutate(name = "Median OWE, broad group")
  
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
  
  results = bind_rows(
      median_owe, mean_owe, median_broad,
      count_studies_all, count_studies_published,
      number_journals, journals_with_at_least_4
    ) |> 
    select(name, value)
  
  write_csv(results, file_name)
  
  file_name
}