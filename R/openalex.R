create_initial_issns = function(initial_journals) {
  issns = initial_journals |>
    select(name = journal_short, value = issn) |>
    deframe()
}

retrieve_works_oa_ = function(issn, from, to) {
  mw_query = '"minimum wage" OR "minimum wages" OR "minimum wage\'s"'
  lw_query = '"living wage" OR "living wages" OR "living wage\'s"'
  tw_query = '"tipped wage" OR "tipped wages" OR "tipped wage\'s"'

  search_query = paste(mw_query, lw_query, tw_query, sep = " OR ")

  oa_fetch(
    entity = "works",
    from_publication_date = from,
    to_publication_date = to,
    primary_location.source.issn = issn,
    search = search_query
  )
}

retrieve_works_oa = function(issns, from, to) {
  map(
    issns,
    ~ retrieve_works_oa_(.x, from = from, to = to)
  ) |>
    list_rbind(names_to = "journal")
}

retrieve_early_works = function(issns, from, to) {
  # do this separately for two periods
  # just to avoid any API limits

  # first batch:
  early_early = retrieve_works_oa(issns, from = from, to = "2015-12-31")

  # second batch
  early_late = retrieve_works_oa(issns, from = "2016-01-01", to = to)

  bind_rows(early_late, early_early)
}

make_works_csv = function(data, file) {
  data |>
    select(
      journal,
      year = publication_year,
      title,
      abstract,
      doi,
      relevance_score
    ) |>
    arrange(desc(relevance_score)) |>
    write_csv(file)

  file
}


function() {
  archived_test = tar_read(archived_owe_data) |>
    filter(year >= 1992, year <= 2018, published == 1) |>
    select(
      owe_study_id = study_id,
      owe_study = study,
      owe_journal = journal,
      repo_year = year,
      doi = url,
      owe_title = title
    ) |>
    mutate(doi = str_to_lower(doi)) |>
    # altered dois to match openalex
    mutate(
      doi = case_match(
        owe_study_id,
        "rb_2017_le" ~ "https://doi.org/10.1016/j.labeco.2016.11.010",

        .default = doi
      )
    )

  oa_early = tar_read(early_works)

  archived_test |>
    left_join(oa_early, by = "doi") |>
    select(
      relevance_score,
      owe_study,
      owe_journal,
      repo_year,
      owe_title,
      doi
    ) |>
    filter(is.na(relevance_score))
}
