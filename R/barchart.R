barchart <- function(
  df, input, aggregation, current_theme,
  current_palette, ncolors, subtitle, rows) {
  df %>%
    mutate(
      vars = fct_inorder(substr(vars, 1, 20)),
      n = aggregation(n)
    ) %>%
    ggplot(aes(x = vars, y = n, fill = vars)) +
    geom_col(color = "white") +
    facet_wrap( ~ organism, nrow = rows) +
    labs(x = "", y = "", subtitle = subtitle) +
    current_theme +
    theme(
      axis.text.x = element_blank(),
      legend.position = "bottom",
      legend.key.size = unit(0.4, "cm")
    ) +
    scale_fill_manual(values = colorRampPalette(current_palette)(ncolors))
}
