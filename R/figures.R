save_plot <- function(p, file, w = NULL, h = NULL) {
  if (is.null(w)) w <- 9
  if (is.null(h)) h <- 6
  
  ggsave(file, p, device = cairo_pdf, dpi = 600, width = w, height = h, units = "in")
  
  file
}

define_histograms <- function() {
  tribble(
    ~name, ~filter_string, ~outcome,
    "overall", "published == 1", "count",
    "broad", "published == 1 & overall == 1", "percent",
    "narrow", "published == 1 & overall != 1", "percent",
    "rr", "published == 1 & restaurants_retail == 1", "percent",
    "teens", "published == 1 & teens == 1", "percent",
    "before_2010", "published == 1 & year < 2010", "percent",
    "after_2010", "published == 1 & year >= 2010", "percent"
  ) 
}

make_histograms <- function(data, inputs) {
  parameters <- inputs %>% 
    mutate(data = list(data)) %>% 
    select(data, filter_string, outcome)
    
  list_names <- inputs %>% 
    pull(name)
  
  output <- parameters %>% 
    pmap(make_histogram)
  
  names(output) <- list_names
  
  output
}

pair_histograms_pdf <- function(hist_1, hist_2, title_1, title_2, file) {
  plot <- (hist_1 + ggtitle(title_1) + theme(plot.margin = margin(b = 10))) /
  (hist_2 + ggtitle(title_2) + theme(plot.margin = margin(t = 10)))
    
  save_plot(plot, file, h = 8.0, w = 7)
}

colors <- c(
  "Large negative" = "#440154FF",
  "Medium negative" = "#3B528BFF",
  "Small negative" = "#21908CFF",
  "Positive" = "#5DC863FF"
)

make_histogram <- function(data, filter_string = NULL, outcome = "percent") {
  
  data <- data %>% 
    mutate(all = 1) %>% 
    mutate(bin = case_when(
      owe_b < -1.2 ~ -1.4,
      owe_b >= -1.2 & owe_b < -0.8 ~ -1.0,
      owe_b >= -0.8 & owe_b < -0.4 ~ -0.6,
      owe_b >= -0.4 & owe_b <  0.0 ~ -0.2,
      owe_b >=  0.0 & owe_b <  0.4 ~  0.2,
      owe_b >=  0.4 & owe_b <  0.8 ~  0.6,
      owe_b >=  0.8 & owe_b <  1.2 ~  1.0,
      owe_b >=  1.2 ~ 1.4
    ))
  
  if (is.null(filter_string)) {
    filter_string <- "all == 1"
  }
  
  filtered_data <- data %>% 
    filter(eval(rlang::parse_expr(filter_string)))
  
  binned_template <- data %>% 
    count(group = owe_magnitude, bin) %>% 
    mutate(bin = bin * -1) %>% 
    select(group, bin)
  
  binned_data <- filtered_data %>% 
    count(group = owe_magnitude, bin) %>% 
    mutate(bin = bin * -1) %>% 
    full_join(binned_template, by = join_by(group, bin)) %>% 
    mutate(n = if_else(is.na(n), 0, n))
  
  legend_labels <- binned_data %>% 
    summarize(n = sum(n), .by = group) %>% 
    mutate(
      n_total = sum(n),
      n_share = scales::label_percent(accuracy = 1)(n / n_total),
      legend_label = paste0(group, ": ", n, " studies (", n_share, ")")
    ) %>% 
    select(group, legend_label) %>% 
    deframe()
  
  median_owe <- filtered_data %>% 
    summarize(median(owe_b) * -1) %>% 
    pull()
  
  median_owe_label <- paste(
    "Median OWE =", 
    scales::label_number(accuracy = 0.01)(median_owe * -1)
  ) %>% 
    str_replace("-", "\u2212")
  
  y_axis_breaks <- seq(-14, 14, 4) / 10 * -1
  y_axis_labels <- c(
    "More negative than -1.2",
    "-1.2  to -0.8",
    "-0.8  to -0.4",
    "-0.4  to  0.0",
    " 0.0  to  0.4",
    " 0.4  to  0.8",
    " 0.8  to  1.2",
    "More positive than 1.2"
  )
  
  if (outcome == "percent") {
    binned_data <- binned_data %>% 
      mutate(n = n / sum(n))
    
    max_value <- 0.44
    
    x_scale <- scale_x_continuous(
      labels = scales::label_percent(accuracy = 1),
      breaks = c(seq(0, 0.4, 0.1)), 
      minor_breaks = seq(0, 0.4, 0.1) + 0.05,
      expand = expansion(mult = c(0, 0), add = c(0.01, 0.0)),
      limits = c(0, 0.8)
    )
    
    x_axis_title <- "Share of studies"
    
    median_owe_seg_xend <- max_value * 1.10
    median_owe_label_x <- max_value * 1.15
    
  }
  
  if (outcome == "count") {
    max_value <- binned_data %>% 
      summarize(max(n)) %>% 
      pull()
    
    max_value <- 20.5
    
    x_scale <- scale_x_continuous(
      breaks = seq(0, 20, 5), 
      minor_breaks = seq(0, 15, 5) + 2.5,
      limits = c(0, 37),
      expand = expansion(mult = c(0, 0), add = c(0.5, 0))
    )
    
    x_axis_title <- "Number of studies"
    
    median_owe_seg_xend <- max_value * 1.10
    median_owe_label_x <- max_value * 1.15
    
  }
  
  binned_data %>% 
    mutate(group = factor(group, levels = c(
      "Large negative",
      "Medium negative",
      "Small negative",
      "Positive"
    ))) %>% 
    ggplot(aes(y = bin, x = n, fill = group)) +
    geom_col(orientation = "y") + 
    geom_segment(
      aes(x = 0, xend = median_owe_seg_xend, y = median_owe, yend = median_owe),
      color = "grey30",
      alpha = 0.5,
      lineend = "round", 
      linetype = "dashed"
    ) +
    annotate(
      "text", 
      x = median_owe_label_x, 
      y = median_owe, 
      label = median_owe_label,
      hjust = 0,
      size = 3.5, 
      color = "grey30",
      family="Roboto Condensed"
    ) + 
    scale_y_continuous(breaks = y_axis_breaks, labels = y_axis_labels) +
    x_scale +
    scale_fill_manual(
      labels = legend_labels,
      values = colors
    ) +
    labs(
      x = x_axis_title,
      y = NULL
    ) +
    theme_minimal() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      legend.title=element_blank(),
      legend.position = c(0.78, 0.85),
      axis.title.x = element_text(
        size = 10, 
        hjust = 0.64),
      plot.margin = margin(0,0,0,0),
      text = element_text(size=12, family="Roboto Condensed", color = "grey30"),
      plot.title = element_text(
        
        margin = margin(b = 10)
      )
    )
}

make_range_plot <- function(data) {
  truncate <- function(x) {
    case_when(x > 3 ~ 3, x < -3 ~ -3, TRUE ~ x)
  }
  
  data <- data %>% 
    mutate(group = factor(owe_magnitude, levels = c(
      "Large negative",
      "Medium negative",
      "Small negative",
      "Positive"
    ))) %>% 
    mutate(across(owe_ub|owe_lb, truncate))
  
  legend_labels <- data %>% 
    count(group) %>% 
    mutate(
      n_total = sum(n),
      n_share = scales::label_percent(accuracy = 1)(n / n_total),
      legend_label = paste0(group, ": ", n, " studies (", n_share, ")")
    ) %>% 
    select(group, legend_label) %>% 
    deframe()
  
  data %>% 
    ggplot(aes(y = reorder(study, owe_b, desc), color = group)) +
    geom_point(aes(x = owe_b), size = 2) +
    geom_segment(
      aes(x = owe_lb, xend = owe_ub, yend = study), 
      lineend = "round",
      linewidth = 1
    ) + 
    scale_color_manual(
      values = colors,
      labels = legend_labels
    ) +
    scale_x_continuous(
      breaks = seq(-3, 3, 0.5),
      minor_breaks = NULL,
      limits = c(-3, 3),
      expand = expansion(mult = c(0, 0), add = c(0.05, 0.40))
    ) + 
    theme_minimal() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      legend.title=element_blank(),
      legend.position = c(0.79, 0.83),
      legend.spacing.y = unit(0, 'cm'),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_markdown(vjust = 0.67),
      plot.margin = margin(0,0,0,0),
      text = element_text(size=9, family="Roboto Condensed", color = "grey30")
    ) +
    guides(color = guide_legend(byrow = TRUE))
}


make_owe_reported_plot <- function(owe_data) {
  color_1 <- c("#440154FF")
  color_2 <- c("#21908CFF")
  color_3 <- c("#3B528BFF")
  
  owe_data %>% 
    filter(published == 1) %>% 
    mutate(decade = case_when(
      year >= 1990 & year < 2000 ~ "1992 - 1999",
      year >= 2000 & year < 2010 ~ "2000 - 2009",
      year >= 2010 & year < 2020 ~ "2010 - 2019",
      year >= 2020 ~ "2020 - 2024"
    )) %>% 
    summarize(`Employment and wage\nelasticities only` = sum(1 - owe_reported), `OWE reported\nby authors` = sum(owe_reported), .by = decade) %>% 
    mutate(across(everything(), ~ replace_na(.x, 0))) %>% 
    pivot_longer(-decade) %>% 
    ggplot(aes(x = decade, fill = name, y = value)) + 
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c(color_2, color_3)) + 
    scale_y_continuous(breaks = seq(0, 32, 4), minor_breaks = seq(0, 30, 2)) + 
    theme_minimal(base_family = "Roboto Condensed") +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      text = element_text(size=12, color = "grey30"),
      #legend.position = "none",
      legend.title = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.key.spacing.y  = unit(0.3, "cm"),
      legend.key.width = unit(0.7, "cm")
    )
}

make_owe_decade_plot <- function(owe_data, bootstraps) {
  
  bootstraps = bootstraps |> 
    select(decade, boot_se, name) |> 
    mutate(name = case_match(
      name,
      "median" ~ "Median",
      "mean" ~ "Mean"
    ))
  
  color_1 <- c("#440154FF")
  color_2 <- c("#21908CFF")
  color_3 <- c("#3B528BFF")
  
  colors = c("Mean" = color_2, "Median" = color_3, "owe_b" = color_1)
  alpha_values = c("Mean" = 1, "Median" = 1, "owe_b" = 0.4)
  point_sizes = c("Mean" = 3.2, "Median" = 3.2, "owe_b" = 1.8)
  point_shapes = c("Mean" = 17, "Median" = 15, "owe_b" = 1)
  
  data = owe_data %>%
    filter(published == 1) |> 
    mutate(decade = case_when(
      year >= 1990 & year < 2000 ~ "1992 - 1999",
      year >= 2000 & year < 2010 ~ "2000 - 2009",
      year >= 2010 & year < 2020 ~ "2010 - 2019",
      year >= 2020 ~ "2020 - 2024"
    )) |> 
    select(study_id, decade, owe_b)
  
  decade_summaries = data |> 
    summarize(
      Median = median(owe_b), 
      Mean = mean(owe_b),
      .by = decade
    ) |> 
    pivot_longer(-decade) |> 
    inner_join(bootstraps, by = c("decade", "name")) |> 
    mutate(
      ci_95_lo = value - 1.96 * boot_se,
      ci_95_hi = value + 1.96 * boot_se
    )
  
  data |> 
    rename(value = owe_b) |> 
    mutate(name = "owe_b") |> 
    bind_rows(decade_summaries) |> 
    mutate(point_size = case_match(
      name,
     "Mean" ~ 2, 
     "Median" ~ 2, 
     "owe_b" ~ 5
    )) |> 
    mutate(decade_x = case_when(
      decade == "1992 - 1999" ~ 1,
      decade == "2000 - 2009" ~ 2,
      decade == "2010 - 2019" ~ 3,
      decade == "2020 - 2024" ~ 4
    )) |> 
    mutate(decade_x = case_match(
      name,
      "Median" ~ decade_x + 0.09,
      "Mean" ~ decade_x + 0.2,
      .default = decade_x
    )) |> 
    ggplot(aes(x = decade_x, y = value, color = name)) + 
    geom_point(aes(alpha = name, size = name, shape = name), stroke = 0.7) +
    geom_errorbar(aes(ymin = ci_95_lo, ymax = ci_95_hi)) +
    scale_color_manual(name = "", values = colors, breaks = c("Mean", "Median")) +
    scale_alpha_manual(values = alpha_values) + 
    scale_size_manual(name = "", values = point_sizes, breaks = c("Mean", "Median")) +
    scale_shape_manual(name = "", values = point_shapes, breaks = c("Mean", "Median")) +
    scale_y_continuous(limits = c(-2.5, 2), minor_breaks = NULL) + 
    scale_x_continuous(
      breaks = seq(1, 4), 
      labels = c("1992 - 1999", "2000 - 2009", "2010 - 2019", "2020 - 2024"),
      minor_breaks = NULL
    ) +
    guides(alpha = "none") + 
    theme_minimal(base_family = "Roboto Condensed") +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      text = element_text(size=12, color = "grey30"),
      panel.grid.major.x = element_blank(),
      legend.key.spacing.y  = unit(0.1, "cm")
    )
}




