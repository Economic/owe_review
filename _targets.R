## Load your packages, e.g. library(targets).
source("packages.R")

## Functions
tar_source()

# for reproducibility of the bootstrap
tar_option_set(seed = 2533725)

tar_assign({
  # owe data from web
  owe_data = "mw_owe_repository.csv" |>
    tar_file_read(read_csv(!!.x, show_col_types = FALSE))

  # ns data
  ns_data_xls = "MW_Papers_1992_2020_Final.xlsx" |>
    tar_file()
  ns_data = read_ns_data(ns_data_xls) |>
    tar_target()

  # ns_dz matches, hand-entered
  ns_dz_matches = "ns_dz_studies.csv" |>
    tar_file_read(read_csv(!!.x, show_col_types = FALSE))

  # combine ns data and owe data
  combined_ns_dz = combine_ns_dz(ns_data, ns_dz_matches, owe_data) |>
    tar_target()
  unique_ns_dz = make_ns_dz_unique(combined_ns_dz) |>
    tar_target()

  # bootstrapped median and mean by decade
  boot_central = bootstrap_by_decade(owe_data) |>
    tar_target()

  # histograms
  histogram_inputs = define_histograms() |>
    tar_target()
  all_histograms = make_histograms(owe_data, histogram_inputs) |>
    tar_target()

  # MAIN TEXT FIGURES
  # range plot of all studies
  range_plot = make_range_plot(owe_data) |> tar_target()
  range_plot_file = save_plot(
    range_plot,
    "docs/range_plot.pdf",
    h = 7.5,
    w = 7
  ) |>
    tar_file()

  # overall histogram pdf
  histogram_overall_file = save_plot(
    all_histograms$overall,
    "docs/histogram_overall.pdf",
    w = 7,
    h = 4.67
  ) |>
    tar_file()

  # Median and mean OWE by decade plot
  owe_decade_plot = make_owe_decade_plot(owe_data, boot_central) |> tar_target()
  owe_decade_file = save_plot(
    owe_decade_plot,
    "docs/owe_decade.pdf",
    w = 7,
    h = 4.67
  ) |>
    tar_file()

  # MAIN TEXT TABLES
  # summary table
  summary_table = make_summary_table(owe_data, "docs/summary_table.tex") |>
    tar_file()

  # APPENDIX FIGURES
  # OWE reported plot
  owe_reported_plot = make_owe_reported_plot(owe_data) |> tar_target()
  owe_reported_file = save_plot(
    owe_reported_plot,
    "docs/owe_reported.pdf",
    w = 7,
    h = 4.67
  ) |>
    tar_file()

  # broad vs narrow histograms pdf
  histogram_broad_narrow_file = pair_histograms_pdf(
    all_histograms$broad,
    all_histograms$narrow,
    "Overall/broad group low wage workers",
    "Narrow group of low wage workers",
    "docs/histogram_broad_narrow.pdf"
  ) |>
    tar_file()

  # teens vs restaurants/retail histograms pdf
  histogram_teens_rr_file = pair_histograms_pdf(
    all_histograms$rr,
    all_histograms$teens,
    "Restaurants or retail",
    "Teenagers",
    "docs/histogram_teens_rr.pdf"
  ) |>
    tar_file()

  # before / after 2010 pdf
  histogram_2010_file = pair_histograms_pdf(
    all_histograms$before_2010,
    all_histograms$after_2010,
    "Published between 1992 and 2009",
    "Published between 2010 and 2024",
    "docs/histogram_before_after_2010.pdf"
  ) |>
    tar_file()

  # APPENDIX TABLES
  # country table
  country_table = make_country_table(owe_data, "docs/country_table.tex") |>
    tar_file()

  # DZ NS table
  dz_ns_table = make_dz_ns_table(unique_ns_dz, "docs/dz_ns_table.tex") |>
    tar_file()

  # APPENDIX LIST OF REFERENCES
  # list of references
  reference_list = make_reference_list(owe_data, "docs/study_list.tex") |>
    tar_file()

  # misc stats for review text
  paper_stats_csv = create_paper_stats_csv(
    owe_data,
    unique_ns_dz,
    "docs/paper_stats.csv"
  ) |>
    tar_file()
})
