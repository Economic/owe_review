function() {
  from_date = "2019-01-01"
  until_date = "2025-09-30"

  results_oa |> write_csv("~/Downloads/openalex_results_20190101_20250931.csv")

  results_cr_prep = read_csv(
    "~/Downloads/crossref_results_20190101_20250931_filtered.csv"
  ) |>
    select(
      journal_id,
      doi,
      cr_journal = journal,
      cr_year = year,
      cr_title = title
    )

  results_oa_prep = read_csv(
    "~/Downloads/openalex_results_20190101_20250931.csv"
  ) |>
    select(
      journal_id = journal,
      doi,
      oa_journal = source_display_name,
      oa_year = publication_year,
      oa_title = title
    )

  results_cr_prep |>
    full_join(results_oa_prep) |>
    filter(is.na(oa_title))
}
