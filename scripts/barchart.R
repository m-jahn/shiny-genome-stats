barchart <- function(
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
    mutate(n = aggregation(n))
  plot <- ggplot(df, aes(x = vars, y = n, fill = .data[[fill]])) +
    geom_col(color = "white") +
    facet_wrap(~organism, nrow = as.numeric(rows), ncol = cols) +
    labs(x = "", y = "", subtitle = subtitle) +
    current_theme +
    theme(
      axis.text.x = element_blank(),
      legend.position = "bottom",
      legend.key.size = unit(0.4, "cm")
    ) +
    scale_fill_manual(values = colorRampPalette(current_palette)(ncolors))
  if (ncolors <= 10) {
    plot <- plot +
      geom_text(
        aes(label = round(n, 1), color = .data[[fill]]),
        size = 2.5,
        nudge_y = max(df$n) * 0.06
      ) +
      scale_color_manual(values = colorRampPalette(current_palette)(ncolors))
  }
  return(plot)
}
