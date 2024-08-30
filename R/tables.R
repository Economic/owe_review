save_kable_tex_fragment <- function(kable_latex_object, file) {
  kable_latex_object %>% 
    str_remove_all("\\\\begin\\{t.*") %>% 
    str_remove_all("\\\\end\\{t.*") %>% 
    str_remove_all("\\\\toprule") %>% 
    str_remove_all("\\\\bottomrule") %>% 
    str_remove_all("\\\\centering") %>% 
    writeLines(file)
}

make_summary_table <- function(data, file) {
  published <- data %>% 
    filter(published == 1) %>% 
    summary_df() %>% 
    mutate(panel = "published") %>% 
    mutate(category = if_else(
      category == "All studies", 
      "All published studies", 
      category
    ))
  
  table_data <- data %>% 
    summary_df() %>% 
    filter(category == "All studies") %>% 
    mutate(panel = "overall") %>% 
    bind_rows(published) %>% 
    select(-panel) %>% 
    relocate(
      category, 
      count, 
      median, 
      mean
    )
  
  table_data %>% 
    kbl(
      booktabs = T, 
      format = "latex",
      col.names = c("", "{Number of studies}", "{Median OWE}", "{Mean OWE}"),
      linesep = ""
    ) %>% 
    add_indent(positions = 3:12) %>% 
    column_spec(1, bold = if_else(
      table_data$category %in% c("All studies", "All published studies"), 
      TRUE, 
      FALSE
    )) %>% 
    str_replace_all("\\\\\\{", "\\{") %>% 
    str_replace_all("\\\\\\}", "\\}") %>% 
    str_replace_all("Number of studies", "\\\\thead\\{Number\\\\\\\\of studies\\}") %>% 
    str_replace_all("Median OWE", "\\\\thead\\{Median\\\\\\\\OWE\\}") %>% 
    str_replace_all("Mean OWE", "\\\\thead\\{Mean\\\\\\\\OWE\\}") %>% 
    save_kable_tex_fragment(file)
  
  file
}

make_hypothetical_table <- function(file) {
  data <- tribble(
    ~sector, ~aff_share, ~mw_e, ~mw_w,
    "All workers", 0.10, 0.01, 0.03,
    "Retail", 0.15, -0.03, 0.20,
    "Restaurants", 0.40, -0.10, 0.45,
    "All workers", 0.05, -0.02, 0.00,
    "Retail", 0.10, -0.02, 0.10,
    "Restaurants", 0.30, -0.05, 0.25
  ) %>% 
    mutate(
      owe = if_else(mw_w > 0, mw_e / mw_w, NA),
      owe = scales::label_number(accuracy = 0.01)(owe),
      owe = if_else(is.na(owe), "{---}", owe),
      #aff_share = scales::label_percent(accuracy = 0.1)(aff_share)
    ) 
  
  data %>% 
    kbl(
      booktabs = T, 
      format = "latex",
      col.names = c(
        "", 
        "{Affected share}", 
        "{$\\epsilon^E$}", 
        "{$\\epsilon^W$}",
        "{OWE}"
      ),
      linesep = "",
      escape = FALSE
    ) %>% 
    pack_rows("Case A", 1, 3) %>%
    pack_rows("Case B", 4, 6) %>% 
    # fix percent signs
    str_replace_all("\\%", "\\\\%") %>% 
    save_kable_tex_fragment(file)
  
  file
}


make_country_table <- function(data, file) {
  published = data |> 
    filter(published == 1) |> 
    count(country) |> 
    rename(count_published = n)
  
  all = data |> 
    count(country) |> 
    rename(count_all = n) 
  
  table_data = all |> 
    full_join(published, by = "country") |> 
    mutate(country = case_match(
      country,
      "US" ~ "United States",
      "UK" ~ "United Kingdom",
      .default = country
    )) |> 
    arrange(desc(count_all), country) 

  
  table_data |> 
    kbl(
      booktabs = T, 
      format = "latex",
      col.names = c("", "{All studies}", "{Published studies}"),
      linesep = ""
    ) |> 
    str_replace_all("\\\\\\{", "\\{") %>% 
    str_replace_all("\\\\\\}", "\\}") %>% 
    str_replace_all("All studies", "\\\\thead\\{All studies\\}") %>% 
    str_replace_all("Published studies", "\\\\thead\\{Published studies\\}") %>% 
    save_kable_tex_fragment(file)
  
  file
}

make_dz_ns_table = function(data, file) {
  table_data = data |> 
    filter(dz_published == 1 | is.na(dz_published)) |> 
    filter(dz_country == "US" | is.na(dz_country)) |> 
    count(dz_status, ns_status) |> 
    mutate(value = as.character(n)) |> 
    pivot_wider(id_cols = dz_status, names_from = ns_status) |> 
    mutate(across(everything(), ~ replace_na(.x, ""))) |> 
    relocate(dz_status, `NS missing`, `NS negative`, `NS positive`)
  
  table_data |> 
    kbl(
      booktabs = T, 
      format = "latex",
      col.names = c("", "{NS missing}", "{NS negative}", "{NS positive}"),
      linesep = ""
    ) |> 
    str_replace_all("\\\\\\{", "\\{") %>%
    str_replace_all("\\\\\\}", "\\}") %>%
    # str_replace_all("All studies", "\\\\thead\\{All studies\\}") %>% 
    # str_replace_all("Published studies", "\\\\thead\\{Published studies\\}") %>% 
    save_kable_tex_fragment(file)
  
  file
    
}