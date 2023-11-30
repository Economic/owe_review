make_reference_list <- function(data, file_name) {
  rows <- nrow(data)
  
  vec <- map(1:rows, ~ make_single_reference(data, .x)) %>% 
    list_c()
  
  vec <- c("\\onehalfspacing", vec)

  writeLines(as.character(vec), file(file_name), sep="\n")
  
  file_name
}

make_single_reference <- function(data, row) {
  data <- data %>% 
    filter(row_number() == row)
  
  study_name <- pull(data, study)
  authors <- pull(data, authors)
  title <- pull(data, title)
  url <- pull(data, url)
  journal <- pull(data, journal)
  owe_b <- pull(data, owe_b)
  owe_se <- pull(data, owe_se)
  source <- pull(data, source) %>% 
    str_replace_all("\\$", "\\\\$") %>% 
    str_replace_all("\\_", "\\\\_")
  
  c(
    paste0("\\subsection*{", study_name, "}"),
    "\\vspace{-0.7em}",
    "",
    paste0("\\noindent ", authors, ". ``", title, "'', \\href{", url, "}{", journal, "}."),
    "",
    "\\vspace{0.7em}",
    "",
    paste0("\\noindent {\\bf Own-wage elasticity estimate:} ", owe_b, " (", owe_se, ")"),
    "",
    "\\vspace{0.7em}",
    "",
    paste0("\\noindent {\\bf Source of estimate:} ", source),
    ""
  )
  
}

