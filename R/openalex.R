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

retrieve_early_works = function(issns) {
  early_early = retrieve_works_oa(
    issns,
    from = "2010-01-01",
    to = "2015-12-31"
  ) |>
    mutate(
      regime_from = "Jan 2010",
      regime_to = "Dec 2018"
    )
  early_late = retrieve_works_oa(
    issns,
    from = "2016-01-01",
    to = "2018-12-31"
  ) |>
    mutate(
      regime_from = "Jan 2010",
      regime_to = "Dec 2018"
    )

  # retrieve_works_oa(
  #   issns,
  #   from = "2019-01-01",
  #   to = "2025-09-31"
  # ) |>
  #   mutate(
  #     regime_from = "Jan 2019",
  #     regime_to = "Sep 2025"
  #   ) |>
  #   bind_rows(early_late, early_early)

  bind_rows(early_late, early_early)
}
