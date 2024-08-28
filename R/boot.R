central_tendencies <- function(data, i) {
  median = median(data$owe_b[i])
  mean = mean(data$owe_b[i])
  
  c(median, mean)
}

bootstrap_by_decade = function(data, seed) {
  data |> 
    filter(published == 1) |> 
    mutate(decade = case_when(
      year >= 1990 & year < 2000 ~ "1992 - 1999",
      year >= 2000 & year < 2010 ~ "2000 - 2009",
      year >= 2010 & year < 2020 ~ "2010 - 2019",
      year >= 2020 ~ "2020 - 2024"
    )) |>
    nest_by(decade) |> 
    mutate(
      results = list(boot(
        data, 
        statistic = central_tendencies, 
        R = 1000000, 
        parallel = "multicore", 
        ncpus = 4
      )),
      tidy_boot = list(broom::tidy(results))
    ) |> 
    unnest(tidy_boot) |> 
    mutate(name = case_when(row_number() == 1 ~ "median", row_number() == 2 ~ "mean")) |> 
    select(decade, boot_statistic = statistic, boot_se = std.error, name) |> 
    ungroup()
}

