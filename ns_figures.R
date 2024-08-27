make_ns_decade_plot <- function(ns_data) {
  
  color_1 <- c("#440154FF")
  color_2 <- c("#21908CFF")
  color_3 <- c("#3B528BFF")
  
  colors = c("Mean" = color_2, "Median" = color_3, "ns_elast" = color_1)
  alpha_values = c("Mean" = 1, "Median" = 1, "ns_elast" = 0.4)
  point_sizes = c("Mean" = 3.2, "Median" = 3.2, "ns_elast" = 1.8)
  point_shapes = c("Mean" = 17, "Median" = 15, "ns_elast" = 1)
  
  data = ns_data %>% 
    unite(authors, matches("author"), sep = " ", na.rm = TRUE) %>% 
    unite(year_mult, year, multiple, sep = "", na.rm = TRUE, remove = FALSE) %>% 
    mutate(ns_study = paste(authors, year_mult)) %>% 
    select(ns_study, ns_elast = `Preferred estimate`, year) %>% 
    summarize(ns_elast = median(ns_elast), year = first(year), .by = ns_study) %>%
    mutate(decade = case_when(
      year >= 1990 & year < 2000 ~ "1992 - 1999",
      year >= 2000 & year < 2010 ~ "2000 - 2009",
      year >= 2010 & year < 2020 ~ "2010 - 2019",
      year >= 2020 ~ "2020 - 2024"
    )) 
  
  decade_summaries = data |> 
    summarize(
      Median = median(ns_elast), 
      Mean = mean(ns_elast),
      .by = decade
    ) |> 
    pivot_longer(-decade) 
  
  data |> 
    rename(value = ns_elast) |> 
    mutate(name = "ns_elast") |> 
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
    #geom_errorbar(aes(ymin = ci_95_lo, ymax = ci_95_hi)) +
    scale_color_manual(name = "", values = colors, breaks = c("Mean", "Median")) +
    scale_alpha_manual(values = alpha_values) + 
    scale_size_manual(name = "", values = point_sizes, breaks = c("Mean", "Median")) +
    scale_shape_manual(name = "", values = point_shapes, breaks = c("Mean", "Median")) +
    #scale_y_continuous(limits = c(-2.5, 2), minor_breaks = NULL) + 
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
