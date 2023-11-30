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
    mutate(panel = "published")
  
  data %>% 
    summary_df() %>% 
    mutate(panel = "overall") %>% 
    bind_rows(published) %>% 
    select(-panel) %>% 
    relocate(category, count, median, mean) %>% 
    mutate(across(matches("_neg|_pos"), scales::label_percent(accuracy = 0.1))) %>% 
    kbl(
      booktabs = T, 
      format = "latex",
      col.names = c("", "Number of studies", "{Median OWE}", "{Mean OWE}", "{Large negative}", "{Medium negative}", "{Small negative}", "{Zero or positive}")
    ) %>% 
    pack_rows("Overall", 1, 10) %>%
    pack_rows("Published and peer-reviewed", 11, 20) %>% 
    add_header_above(c(" ", " ", " ", " ", "Distribution of studies by OWE range" = 4)) %>% 
    str_replace_all("\\\\\\{", "\\{") %>% 
    str_replace_all("\\\\\\}", "\\}") %>% 
    str_replace("Number of studies", "\\\\thead\\{Number\\\\\\\\of studies\\}") %>% 
    str_replace("Median OWE", "\\\\thead\\{Median\\\\\\\\OWE\\}") %>% 
    str_replace("Mean OWE", "\\\\thead\\{Mean\\\\\\\\OWE\\}") %>% 
    str_replace("Large negative", "\\\\thead\\{Large\\\\\\\\negative\\}") %>% 
    str_replace("Medium negative", "\\\\thead\\{Medium\\\\\\\\negative\\}") %>% 
    str_replace("Small negative", "\\\\thead\\{Small\\\\\\\\negative\\}") %>% 
    str_replace("Zero or positive", "\\\\thead\\{Zero\\\\\\\\or positive\\}") %>% 
    
    save_kable_tex_fragment(file)
  
  file
}