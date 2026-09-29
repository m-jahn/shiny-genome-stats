piechart <- function(
  df,
  input,
  aggregation,
  current_theme,
  current_palette,
  fill,
  subtitle,
  rows,
  cols
) {
  ncolors <- length(unique(df[[fill]]))
  nfacets <- length(unique(df[["organism"]]))
  if (rows * cols < nfacets) {
    rows <- ceiling(nfacets / cols)
  }
  df <- df %>%
    group_by(organism) %>%
    mutate(
      fraction = aggregation(n),
      ymax = cumsum(fraction),
      ymin = c(0, head(ymax, n = -1))
    )
  plot <- ggplot(df, aes(
    xmin = 3,
    xmax = 4,
    ymin = ymin,
    ymax = ymax,
    fill = .data[[fill]]
  )) +
    geom_vline(xintercept = 3, color = grey(0.9), linewidth = 0.6) +
    geom_rect(color = "white") +
    coord_polar(theta = "y") +
    lims(x = c(0, 4)) +
    facet_wrap(~organism, nrow = rows, ncol = cols) +
    labs(x = "", y = "", subtitle = subtitle) +
    current_theme +
    theme(
      axis.text.x = element_text(size = 6.5, color = grey(0.4)),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "bottom",
      legend.key.size = unit(0.4, "cm"),
      panel.grid.major.x = element_blank()
    ) +
    scale_fill_manual(values = colorRampPalette(current_palette)(ncolors))
  return(plot)
}
