## Load your packages, e.g. library(targets).
source("packages.R")

## Globals
citation_authors <- "Arindrajit Dube and Ben Zipperer"
citation_year <- 2023
citation_title <- "Minimum wage own-wage elasticity database"
citation_url <- "https://economic.github.io/owe"
download_date <- "30 November 2023"
owe_database <- "https://economic.github.io/owe/mw_owe_database.csv"

## Functions
lapply(list.files("R", full.names = TRUE), source)

tar_plan(
  owe_data = grab_owe_data(owe_database, download_date),
  histogram = make_histogram(owe_data),
  tar_file(histogram_file, save_plot(histogram, "docs/histogram_overall.pdf")),
  tar_file(summary_table, make_summary_table(owe_data, "docs/summary_table.tex")),
  tar_file(reference_list, make_reference_list(owe_data, "docs/study_list.tex"))
)


