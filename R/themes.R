# custom ggplot2 theme(s)
custom_theme <- function(base_size = 12, base_line_size = 1.0, base_rect_size = 1.0, ...) {
  theme_light(base_size = base_size, base_line_size = base_line_size, base_rect_size = base_rect_size) + theme(
    title = element_text(colour = grey(0.4), size = 10),
    plot.margin = unit(c(12, 12, 12, 12), "points"),
    axis.ticks.length = unit(0.2, "cm"),
    axis.ticks = element_line(colour = grey(0.4), linetype = "solid", lineend = "round"),
    axis.text.x = element_text(colour = grey(0.4), size = 10),
    axis.text.y = element_text(colour = grey(0.4), size = 10),
    panel.grid.major = element_line(linewidth = 0.6, linetype = "solid", colour = grey(0.9)),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(linetype = "solid", colour = grey(0.4), fill = NA, linewidth = 1.0),
    panel.background = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(colour = grey(0.4), size = 10, margin = unit(rep(3, 4), "points")),
    legend.text = element_text(colour = grey(0.4), size = 10),
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.key.size = unit(0.4, "cm"),
    ...
  )
}
