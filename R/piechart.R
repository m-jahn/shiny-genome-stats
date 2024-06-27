piechart <- function(df, input, aggregation, current_theme, current_palette, ncolors, subtitle, rows) {
  df %>%
    mutate(
      vars = fct_inorder(substr(vars, 1, 20)),
      fraction = aggregation(n),
      ymax = cumsum(fraction),
      ymin = c(0, head(ymax, n = -1))
    ) %>%
    ggplot(aes(
      xmin = 3,
      xmax = 4,
      ymin = ymin,
      ymax = ymax,
      fill = vars
    )) +
    geom_rect(color = "white") +
    coord_polar(theta = "y") +
    lims(x = c(0, 4)) +
    facet_wrap(~ organism, nrow = rows) +
    labs(x = "", y = "", subtitle = subtitle) +
    current_theme +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "bottom",
      legend.key.size = unit(0.4, "cm")
    ) +
    scale_fill_manual(values = colorRampPalette(current_palette)(ncolors))
}
