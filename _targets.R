## Load your packages, e.g. library(targets).
source("packages.R")

## Functions
lapply(list.files("R", full.names = TRUE), source)

# for reproducibility of the bootstrap
tar_option_set(seed = 2533725)

tar_plan(
  
  # owe data from web
  tar_file(owe_data_csv, "mw_owe_repository.csv"),
  owe_data = read_csv(owe_data_csv, show_col_types = FALSE),
  
  # ns papers with dz study id
  tar_file(ns_dz_matches_csv, "ns_dz_match.csv"),
  
  # bootstrapped median and mean by decade
  boot_central = bootstrap_by_decade(owe_data),
  
  # histogram names
  histogram_inputs = define_histograms(),
  
  # all histograms, as list
  all_histograms = make_histograms(owe_data, histogram_inputs),
  
  # overall histogram pdf
  tar_file(histogram_overall_file, save_plot(
    all_histograms$overall, "docs/histogram_overall.pdf", w = 7, h = 4.67
  )),
  
  # broad vs narrow histograms pdf
  tar_file(histogram_broad_narrow_file, pair_histograms_pdf(
    all_histograms$broad, 
    all_histograms$narrow,
    "Overall/broad group low wage workers",
    "Narrow group of low wage workers",
    "docs/histogram_broad_narrow.pdf"
  )),
  
  # teens vs restaurants/retail histograms pdf
  tar_file(histogram_teens_rr_file, pair_histograms_pdf(
    all_histograms$teens, 
    all_histograms$rr,
    "Teenagers",
    "Restaurants or retail",
    "docs/histogram_teens_rr.pdf"
  )),

  # summary table
  tar_file(
    summary_table, 
    make_summary_table(owe_data, "docs/summary_table.tex")
  ),
  
  # list of references
  tar_file(
    reference_list, 
    make_reference_list(owe_data, "docs/study_list.tex")
  ),
  
  # range plot of all studies
  range_plot = make_range_plot(owe_data),
  tar_file(
    range_plot_file, 
    save_plot(range_plot, "docs/range_plot.pdf", h = 7.5, w = 7)
  ),
  
  # OWE reported plot
  owe_reported_plot = make_owe_reported_plot(owe_data),
  tar_file(
    owe_reported_file, 
    save_plot(owe_reported_plot, "docs/owe_reported.pdf", w = 7, h = 4.67)
  ),
  
  # Median OWE by decade bar plot
  owe_decade_plot = make_owe_decade_plot(owe_data),
  tar_file(
    owe_decade_file, 
    save_plot(owe_decade_plot, "docs/owe_decade.pdf", w = 7, h = 4.67)
  ),
  
  # hypothetical elasticities table
  tar_file(
    hypohetical_elasticities_table, 
    make_hypothetical_table("docs/hypothetical_table.tex")
  ),
  
  tar_file(
    paper_stats_csv, create_paper_stats_csv(
      owe_data,
      ns_dz_matches_csv,
      "docs/paper_stats.csv"
    )
  )
)
