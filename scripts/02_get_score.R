library(gstat)  # idw()
source("src/gis-utils.R")
source("src/normalize_minmax.R")


# create grid
grd <- create_grid_in_polygon(base_map, 10)

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
  mutate(score = normalize_minmax(pop) + normalize_minmax(fpop^0.25) + normalize_minmax(rental))
