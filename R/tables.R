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
    "Restaurants", 0.50, -0.10, 0.50,
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
  cells = data |> 
    filter(dz_published == 1 | is.na(dz_published)) |> 
    filter(dz_country == "US" | is.na(dz_country)) |> 
    count(dz_status, ns_status) |> 
    mutate(value = as.character(n)) 
  
  total_studies = cells |> 
    summarize(sum(n)) |> 
    pull() |> 
    as.character()
  
  dz_totals = cells |> 
    summarize(`DZ total` = as.character(sum(n)), .by = dz_status)
  
  ns_totals = cells |> 
    summarize(value = sum(n), .by = ns_status) |> 
    mutate(value = as.character(value)) |> 
    pivot_wider(names_from = ns_status) |> 
    mutate(across(everything(), as.character)) |> 
    mutate(
      dz_status = "NS status total",
      `DZ total` = total_studies
    )
    
  table_data = cells |> 
    pivot_wider(id_cols = dz_status, names_from = ns_status) |> 
    full_join(dz_totals, by = "dz_status") |> 
    bind_rows(ns_totals) |> 
    mutate(across(everything(), ~ replace_na(.x, ""))) |> 
    relocate(dz_status, `NS missing`, `NS negative`, `NS positive`)
  
  table_data |> 
    kbl(
      booktabs = T, 
      format = "latex",
      col.names = c("", "{NS missing}", "{NS negative}", "{NS positive}", "{DZ status total}"),
      linesep = ""
    ) |> 
    column_spec(1:5, bold = if_else(table_data$dz_status == "NS status total", TRUE, FALSE)) %>% 
    column_spec(5, bold = TRUE) %>% 
    str_replace_all("\\\\\\{", "\\{") %>%
    str_replace_all("\\\\\\}", "\\}") %>%
    str_replace_all("DZ status total", "\\\\textbf\\{DZ status total\\}") %>% 
    # str_replace_all("Published studies", "\\\\thead\\{Published studies\\}") %>% 
    save_kable_tex_fragment(file)
  
  file
    
}