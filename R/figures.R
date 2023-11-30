make_histogram <- function(data) {
  binned_data <- data %>% 
    mutate(bin = case_when(
      owe_b < -1.2 ~ -1.4,
      owe_b >= -1.2 & owe_b < -0.8 ~ -1.0,
      owe_b >= -0.8 & owe_b < -0.4 ~ -0.6,
      owe_b >= -0.4 & owe_b <  0.0 ~ -0.2,
      owe_b >=  0.0 & owe_b <  0.4 ~  0.2,
      owe_b >=  0.4 & owe_b <  0.8 ~  0.6,
      owe_b >=  0.8 & owe_b <  1.2 ~  1.0,
      owe_b >=  1.2 ~ 1.4
    )) %>% 
    count(group = owe_magnitude, bin) %>% 
    mutate(bin = bin * -1) 
  
  colors <- c(
    "Large negative" = "#440154FF",
    "Medium negative" = "#3B528BFF",
    "Small negative" = "#21908CFF",
    "Positive" = "#5DC863FF"
  )
  
  legend_labels <- binned_data %>% 
    summarize(n = sum(n), .by = group) %>% 
    mutate(
      n_total = sum(n),
      n_share = scales::label_percent(accuracy = 1)(n / n_total),
      legend_label = paste0(group, ": ", n, " studies (", n_share, ")")
    ) %>% 
    select(group, legend_label) %>% 
    deframe()
  
  median_owe <- data %>% 
    summarize(median(owe_b) * -1) %>% 
    pull()
  
  median_owe_label <- paste(
    "Median OWE =", 
    scales::label_number(accuracy = 0.01)(median_owe * -1)
  )
  
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
      aes(x = 0, xend = 25.3, y = median_owe, yend = median_owe),
      lineend = "round", linetype = "dashed"
    ) +
    annotate(
      "text", 
      x = 26, 
      y = median_owe, 
      label = median_owe_label,
      #color = "grey40",
      hjust = 0,
      size = 3.1
    ) + 
    scale_y_continuous(breaks = y_axis_breaks, labels = y_axis_labels) +
    scale_x_continuous(
      breaks = seq(0, 25, 5), 
      minor_breaks = seq(0, 20, 5) + 2.5,
      limits = c(0, 37),
      expand = expansion(mult = c(0, 0), add = c(0.5, 0))
    ) +
    scale_fill_manual(
      labels = legend_labels,
      values = colors
    ) +
    labs(
      x = "Number of studies",
      y = NULL
    ) +
    #hrbrthemes::theme_ipsum_rc() +
    theme_minimal() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      legend.title=element_blank(),
      legend.position = c(0.85, 0.8),
      axis.title.x = element_text(
        size = 10, 
        hjust = 0.64),
      plot.margin = margin(0,0,0,0),
      text = element_text(size=13, family="Roboto Condensed", color = "grey30")
    )
}


make_time_plot <- function(data) {
  binned_data <- data %>% 
    mutate(time = case_when(
      year >= 1992 & year <= 2000 ~ "1992-2000",
      year >= 2001 & year <= 2010 ~ "2001-2010",
      year >= 2011 & year <= 2020 ~ "2011-2020",
      year >= 2021 ~ "2021-2024"
    )) 
  
  published <- binned_data %>%
    filter(published == 1) %>% 
    summarize(study_count = n(), owe_median = median(owe_b), .by = time) %>% 
    mutate(group = "published")
  
  binned_data %>% 
    summarize(study_count = n(), owe_median = median(owe_b), .by = time) %>% 
    mutate(group = "all studies") %>% 
    bind_rows(published) %>% 
    arrange(group, time) %>% 
    ggplot(aes(x = study_count, y = owe_median, color = group, label = time)) +
    geom_path() + 
    geom_point() + 
    geom_text()
}

save_plot <- function(p, file, w = NULL, h = NULL) {
  if (is.null(w)) w <- 9
  if (is.null(h)) h <- 6
  
  ggsave(file, p, device = cairo_pdf, width = w, height = h, units = "in")
  
  file
}