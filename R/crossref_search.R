function() {
  issns = read_csv("initial_journals.csv") |>
    select(name = journal_short, value = issn) |>
    deframe()

  retrieve_works_cr = function(issn, from, until) {
    cr_works(
      filter = list(
        issn = issn,
        from_pub_date = from,
        until_pub_date = until
      ),
      query = "minimum wage wages wage's living tipped",
      sample = 100
    ) |>
      pluck("data")
  }

  from_date = "2019-01"
  until_date = "2025-09"

  results_cr = map(
    issns,
    ~ retrieve_works_cr(.x, from = from_date, until = until_date)
  ) |>
    list_rbind(names_to = "journal")

  write_csv(results_cr, "~/Downloads/crossref_results_20190101_20250931.csv")

  read_csv("~/Downloads/crossref_results_20190101_20250931.csv") |>
    mutate(across(
      title | abstract,
      ~ str_detect(
        str_to_lower(str_replace_all(.x, "-", " ")),
        "(minimum|living|tipped) wage"
      ),
      .names = "{.col}_match"
    )) |>
    filter(title_match == 1 | abstract_match == 1) |>
    mutate(across(published.print | published.online, ~ str_sub(.x, 1, 4))) |>
    mutate(
      year = if_else(
        !is.na(published.print),
        published.print,
        published.online
      ),
      doi = paste0("https://doi.org/", doi)
    ) |>
    select(
      journal_id = journal,
      journal = container.title,
      year,
      doi,
      abstract,
      title
    ) |>
    write_csv("~/Downloads/crossref_results_20190101_20250931_filtered.csv")
}
