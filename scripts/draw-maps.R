library(tidyverse)
library(readxl)
library(rgeos)
library(rgdal)
library(gstat)
library(ggspatial)


# Functions ---------------------------------------------------------------

create_grid_in_polygon <- function(polygon, cell_size, crs) {
  x_range <- polygon@bbox[1, ]
  y_range <- polygon@bbox[2, ]
  grd <- expand.grid(
    x = seq(from = x_range[1], to = x_range[2], by = cell_size), 
    y = seq(from = y_range[1], to = y_range[2], by = cell_size)
  )
  coordinates(grd) <- c("x", "y")
  gridded(grd) <- TRUE
  proj4string(grd) <- crs
  grd_over <- over(grd, geometry(polygon))
  grd[!is.na(grd_over), ]
}

idw_tbl <- function(formula, locations, newdata) {
  idw(formula = formula, locations = locations, newdata = grd) %>% 
    as.tibble() %>% 
    select(1:3)
}

kernel_density <- function(grid, target, r, fold = 100) {
  result <- NULL
  nt <- ntile(seq_len(nrow(grid)), fold)
  for (i in seq_len(fold)) {
    cat("i = ", i, "... ", sep = "")
    batch <- grid[nt ==  i, ]
    n <- nrow(batch)
    dist_mat <- as.matrix(dist(rbind(batch, target)))
    density <- apply(dist_mat[1:n, -(1:n)] < r, 1, sum)
    result <- rbind(result, cbind(batch, density))
    cat("Good!\n")
  }
  as_tibble(result)
}

scale01 <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}


# Data preprocessing ------------------------------------------------------

# coordinate system
crs_wgs84 <- CRS("+init=EPSG:4326")
crs_korea <- CRS("+init=EPSG:5186")
###
# if the above throws an error, try:
# crs_wgs84 <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# crs_korea <- CRS("+proj=tmerc +lat_0=38 +lon_0=127 +k=1 +x_0=200000 +y_0=600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
###

# base map
path <- "data/shapefiles"
base_map <- readOGR(path, "TL_SCCO_EMD")
base_map <- base_map[str_detect(base_map$EMD_CD, "^11710"), ]
base_map <- spTransform(base_map, crs_korea)

base_map_tbl <- NULL
for (i in seq_along(base_map@polygons)) {
  newdata <- 
    base_map@polygons[[i]]@Polygons[[1]]@coords %>% 
    as_tibble() %>% 
    setNames(c("x", "y")) %>% 
    mutate(group  =  i)
  
  base_map_tbl <- bind_rows(base_map_tbl, newdata)
}

area <- sapply(base_map@polygons, function(x) x@area) / 1e6
area <- round(area, 2)
centroid <- gCentroid(base_map, byid = TRUE)

# population data
pop <- read_excel("data/pop.xlsx") %>% setNames(c("region", "n"))

name_adjust <- c(
  "풍납1동" = "풍납동",
  "풍납2동" = "풍납동",
  "거여1동" = "거여동",
  "거여2동" = "거여동",
  "마천1동" = "마천동",
  "마천2동" = "마천동",
  "방이1동" = "방이동",
  "방이2동" = "방이동",
  "오륜동" = "방이동",
  "송파1동" = "송파동",
  "송파2동" = "송파동",
  "가락본동" = "가락동",
  "가락1동" = "가락동",
  "가락2동" = "가락동",
  "문정1동" = "문정동",
  "문정2동" = "문정동",
  "위례동" = "장지동",
  "잠실본동" = "잠실동",
  "잠실2동" = "잠실동",
  "잠실3동" = "잠실동",
  "잠실7동" = "잠실동",
  "잠실4동" = "신천동",
  "잠실6동" = "신천동"
)

pop <- 
  pop %>% 
  mutate(region = plyr::revalue(region, name_adjust)) %>% 
  group_by(region) %>% 
  summarize(n = sum(n))

region_order <- match(base_map@data$EMP_KOR_NM, pop$region)

pop <-
  pop %>% 
  slice(region_order) %>% 
  mutate(area = area,
         density = n / area)

coordinates(pop) <- centroid@coords
proj4string(pop) <- crs_korea

# floating population data
shp_dir <- "data/shapefiles"
fpop <- readOGR(shp_dir, "S_data_floating_pop")
proj4string(fpop) <- crs_wgs84
fpop <- spTransform(fpop, crs_korea)
colnames(fpop@coords) <- c("x", "y")

fpop_over <- over(fpop, geometry(base_map))
fpop <- fpop[!is.na(fpop_over), ]

# rental data
rental <- 
  read_excel("data/rental.xlsx") %>% 
  select(c(8, 7)) %>% 
  setNames(c("long", "lat"))

rental[1:2] <- lapply(rental, as.numeric)
coordinates(rental) <- c("long", "lat")
proj4string(rental) <- crs_wgs84
rental <- spTransform(rental, crs_korea)
colnames(rental@coords) <- c("x", "y")


# Get score ---------------------------------------------------------------

# create grid
grd <- create_grid_in_polygon(base_map, 10, crs_korea)

# interpolation (inverse distance weighting)
pop_idw <- 
  idw_tbl(formula = density ~ 1, locations = pop, newdata = grd) %>% 
  setNames(c("x", "y", "pop"))

fpop_idw <- 
  idw_tbl(formula = POPULATION ~ 1, locations = fpop, newdata = grd) %>% 
  setNames(c("x", "y", "fpop"))

# interpolation (kernel density estimation)
rental_kernel <- 
  kernel_density(grd@coords, rental@coords, r = 1000, fold = 100) %>% 
  setNames(c("x", "y", "rental"))

# score
score_tbl <- 
  bind_cols(pop_idw, fpop_idw[3], rental_kernel[3]) %>% 
  mutate(score = scale01(pop) + scale01(fpop^0.25) + scale01(rental))


# Drawing maps ------------------------------------------------------------

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
  label = pop@data$region
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
  theme_void()

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
