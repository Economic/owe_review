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

 
# tar_read(combined_ns_dz) %>% 
#   filter(is.na(ns_study_id), !is.na(dz_study_id)) %>% 
#   distinct(dz_study_id, .keep_all = T) %>% 
#   filter(dz_published == 1, dz_country == "US", dz_year <= 2021) %>%
#   summarize(
#     mean_ns_elast = mean(ns_elast), 
#     median_ns_elast = median(ns_elast), 
#     n = n(), 
#     median_dz_owe = median(dz_owe), 
#     mean_dz_owe = mean(dz_owe)
#   )