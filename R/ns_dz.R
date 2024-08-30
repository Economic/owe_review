read_ns_data = function(xls) {
  ns_elasticities = read_excel(xls, sheet = "Our_Selections") %>% 
    mutate(year = unlist(year)) %>% 
    unite(authors, matches("author"), sep = " ", na.rm = TRUE) %>% 
    unite(year_mult, year, multiple, sep = "", na.rm = TRUE, remove = FALSE) %>% 
    mutate(ns_study = paste(authors, year_mult)) %>% 
    select(ns_study, ns_elast = `Preferred estimate`, year) 
  
  ns_papers = read_excel(xls, sheet = "Paper_List", skip = 2) %>% 
    select(matches("author"), year, multiple, journal, title) %>% 
    unite(authors, matches("author"), sep = " ", na.rm = TRUE) %>% 
    unite(year_mult, year, multiple, sep = "", na.rm = TRUE, remove = FALSE) %>% 
    mutate(ns_study = paste(authors, year_mult)) %>% 
    select(ns_study, title, journal) 
  
  ns_elasticities %>% 
    left_join(ns_papers, by = "ns_study") %>% 
    select(ns_study, year, journal, title, ns_elast)
}

combine_ns_dz = function(ns_data, ns_dz_matches, owe_data) {
  ns_data %>% 
    select(ns_study, ns_year = year, ns_elast) %>% 
    full_join(ns_dz_matches, by = "ns_study") %>% 
    full_join(owe_data, by = join_by(dz_study == study_id)) %>% 
    select(
      ns_study_id = ns_study, 
      ns_year,
      ns_elast, 
      dz_study_id = dz_study, 
      dz_study_name = study,
      dz_owe = owe_b,
      dz_published = published,
      dz_country = country,
      dz_year = year
    )
  
  # seems NS excludes
  # Allegretto Nadler 2015
  # Borgshulte Cho 2020
  # Card Katz Krueger 1994
  # Dow Godøy Lowenstein Reich 2020
  # Dube Lindner 2021
  # Giuliano 2013
  # Godoey Reich 2021
  # Leung 2021
}

make_ns_dz_unique = function(data) {
  ns_studies = data |> 
    filter(!is.na(ns_study_id)) |> 
    summarize(ns_elast = median(ns_elast), .by = ns_study_id) |> 
    mutate(ns_pos = if_else(ns_elast >= 0, 1, 0))
  
  dz_studies = data |> 
    filter(!is.na(dz_owe)) |> 
    summarize(
      dz_owe = first(dz_owe), 
      ns_study_id = first(ns_study_id), 
      dz_published = first(dz_published), 
      dz_country = first(dz_country),
      .by = dz_study_id
    ) |> 
    mutate(dz_pos = if_else(dz_owe >= 0, 1, 0))
  
  dz_studies |> 
    full_join(ns_studies, by = "ns_study_id") |> 
    mutate(dz_status = case_when(
      dz_pos == 1 ~ "DZ positive",
      dz_pos == 0 ~ "DZ negative",
      .default = "DZ missing"
    )) |> 
    mutate(ns_status = case_when(
      ns_pos == 1 ~ "NS positive",
      ns_pos == 0 ~ "NS negative",
      .default = "NS missing"
    ))
}
