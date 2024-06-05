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

palettes <- function(pal) {
  switch(
    pal,
    ggplot = c("#F8766D", "#C49A00", "#53B400", "#00C094", "#00B6EB", "#A58AFF", "#FB61D7"),
    rainbow = rainbow_hcl(n = 7, start = 60),
    hawaii = sequential_hcl(n = 7, h = c(-30, 200), c = c(70, 75, 35), l = c(30, 92), power = c(0.3, 1)),
    sunset = sequential_hcl(n = 7, h = c(-80, 78), c = c(60, 75, 55), l = c(40, 91), power = c(0.8, 1)),
    batlow = sequential_hcl(n = 7, h = c(270, -40), c = c(35, 75, 35), l = c(12, 88), power = c(0.6, 1.1)),
    terrain = sequential_hcl(n = 7, h = c(130, 30), c = c(65, NA, 0), l = c(45, 90), power = c(0.5, 1.5)),
    dark_mint = sequential_hcl(n = 7, h = c(240, 130), c = c(30, NA, 33), l = c(25, 95), power = c(1, NA)),
    viridis = sequential_hcl(n = 7, h = c(300, 75), c = c(40, NA, 95), l = c(15, 90), power = c(1, 1.1)),
    plasma = sequential_hcl(n = 7, h = c(-100, 100), c = c(60, NA, 100), l = c(15, 95), power = c(2, 0.9)),
    purple_yellow = sequential_hcl(n = 7, h = c(320, 80), c = c(60, 65, 20), l = c(30, 95), power = c(0.7, 1.3)),
    yellow_green = sequential_hcl(n = 7, h = c(270, 90), c = c(40, 90, 25), l = c(15, 99), power = c(2, 1.5)),
    yellow_red = sequential_hcl(n = 7, h = c(5, 85), c = c(75, 100, 40), l = c(25, 99), power = c(1.6, 1.3)),
    pink_yellow = sequential_hcl(n = 7, h = c(-4, 80), c = c(100, NA, 47), l = c(55, 96), power = c(1, NA))
  )
}
