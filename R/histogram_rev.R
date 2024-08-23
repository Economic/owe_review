make_histogram_rev <- function(data) {
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
    count(group = owe_magnitude, bin)
  
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
    summarize(median(owe_b)) %>% 
    pull()
  
  median_owe_label <- paste(
    "Median OWE =", 
    scales::label_number(accuracy = 0.01)(median_owe)
  )
  
  x_axis_breaks <- seq(-14, 14, 4) / 10 
  x_axis_labels <- c(
    "(-\u221e, -1.2)",
    "[ -1.2, -0.8)",
    "[ -0.8, -0.4)",
    "[ -0.4, 0.0)",
    "[ 0.0, 0.4)",
    "[ 0.4, 0.8)",
    "[ 0.8, 1.2)",
    "[ 1.2, \u221e)"
  )
  
  binned_data %>% 
    mutate(group = factor(group, levels = c(
      "Large negative",
      "Medium negative",
      "Small negative",
      "Positive"
    ))) %>% 
    ggplot(aes(x = bin, y = n, fill = group)) +
    geom_col(orientation = "x") + 
    geom_segment(
      aes(y = 0, yend = 25.3, x = median_owe, xend = median_owe),
      lineend = "round", linetype = "dashed"
    ) +
    annotate(
      "text", 
      y = 26, 
      x = median_owe, 
      label = median_owe_label,
      hjust = 0,
      size = 3.1
    ) + 
    scale_x_continuous(
      breaks = x_axis_breaks, 
      labels = x_axis_labels,
    ) +
    scale_y_continuous(
      breaks = seq(0, 25, 5), 
      minor_breaks = seq(0, 20, 5) + 2.5,
      limits = c(0, 26.5),
      expand = expansion(mult = c(0, 0), add = c(0.5, 0))
    ) +
    scale_fill_manual(
      labels = legend_labels,
      values = colors
    ) +
    labs(
      y = "Number of studies",
      x = NULL
    ) +
    #hrbrthemes::theme_ipsum_rc() +
    theme_minimal() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      legend.title=element_blank(),
      legend.position = c(0.83, 0.8),
      axis.title.y = element_text(
        #size = 6, 
        hjust = 0.92),
      axis.text.x = element_text(
        margin=margin(t=-4),
      ),
      plot.margin = margin(0,0,0,0),
      text = element_text(
        #size=10, 
        family="Roboto Condensed", 
        color = "grey30")
    )
}