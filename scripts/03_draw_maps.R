library(ggspatial)  # annotation_scale(), annotation_north_arrow()


# layers
base_layer <- geom_polygon(
  data = base_map_tbl, 
  aes(x = x, y = y, group = group),
  fill = NA, 
  col = "black"
)
text_layer <- geom_text(
  data = as_tibble(pop@coords), 
  aes(x = x, y = y), 
  label = pop@data$region,
  family = "NanumGothic"
)
pop_point_layer <- geom_point(
  data = pop@data, 
  aes(size = density),
  x = pop@coords[, 1], 
  y = pop@coords[, 2],
  col = "red", 
  alpha = .5
)
fpop_point_layer <- geom_point(
  data = fpop@data, 
  aes(size = POPULATION),
  x = fpop@coords[, 1], 
  y = fpop@coords[, 2], 
  col = "brown", 
  alpha = .5
)
rental_point_layer <- geom_point(
  data = as_tibble(rental@coords),
  aes(x = x, y = y), 
  size = 2, 
  col = "blue", 
  alpha = .5
)
pop_layer <- geom_tile(data = score_tbl, aes(x = x, y = y, fill = pop))
fpop_layer <- geom_tile(data = score_tbl, aes(x = x, y = y, fill = fpop))
fpop.25_layer <- geom_tile(data = score_tbl, aes(x = x, y = y, fill = fpop^0.25))
rental_layer <- geom_tile(data = score_tbl, aes(x = x, y = y, fill = rental))
score_layer <- geom_tile(data = score_tbl, aes(x = x, y = y, fill = score))

# create maps
p <- ggplot() +
  scale_fill_distiller(palette = "RdBu", name = NULL) +
  annotation_scale(
    location = "tr", 
    plot_unit = "m", 
    style = "ticks",
    width_hint = .25, 
    tick_height = .5,
    pad_x = unit(.25, "cm"), 
    pad_y = unit(.5, "cm")
  ) +
  annotation_north_arrow(
    location = "tr", 
    which_north = "grid",
    style = north_arrow_fancy_orienteering(text_size = 13),
    height = unit(1.2, "cm"), 
    width = unit(1.2, "cm"),
    pad_x = unit(0, "cm"), 
    pad_y = unit(1.25, "cm")
  ) +
  coord_fixed() +
  theme_void(base_family = "NanumGothic")

pop_point <- 
  p + pop_point_layer + base_layer + text_layer +
  scale_size(name = expression(1~km^{2}~당~인구수), range = c(2, 12))
fpop_point <- 
  p + fpop_point_layer + base_layer + text_layer +
  scale_size(name = "유동인구", range = c(1, 10))
rental_point <- p + rental_point_layer + base_layer + text_layer
pop_map <- p + pop_layer + base_layer + text_layer
fpop_map <- p + fpop_layer + base_layer + text_layer
fpop.25_map <- p + fpop.25_layer + base_layer + text_layer
rental_map <- p + rental_layer + base_layer + text_layer
score_map <- p + score_layer + base_layer + text_layer

# save maps
ggsave("outputs/pop_point.png", pop_point, width = 5, height = 5)
ggsave("outputs/fpop_point.png", fpop_point, width = 5, height = 5)
ggsave("outputs/rental_point.png", rental_point, width = 5, height = 5)
ggsave("outputs/pop_map.png", pop_map, width = 5, height = 5)
ggsave("outputs/fpop_map.png", fpop_map, width = 5, height = 5)
ggsave("outputs/fpop.25_map.png", fpop.25_map, width = 5, height = 5)
ggsave("outputs/rental_map.png", rental_map, width = 5, height = 5)
ggsave("outputs/score_map.png", score_map, width = 5, height = 5)
