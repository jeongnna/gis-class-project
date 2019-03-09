create_grid_in_polygon <- function(polygon, cell_size) {
  x_range <- polygon@bbox[1, ]
  y_range <- polygon@bbox[2, ]
  grd <- expand.grid(
    x = seq(from = x_range[1], to = x_range[2], by = cell_size), 
    y = seq(from = y_range[1], to = y_range[2], by = cell_size)
  )
  coordinates(grd) <- c("x", "y")
  gridded(grd) <- TRUE
  proj4string(grd) <- polygon@proj4string
  grd_over <- over(grd, geometry(polygon))
  grd[!is.na(grd_over), ]
}


idw_tbl <- function(formula, locations, newdata) {
  idw(formula = formula, locations = locations, newdata = grd) %>% 
    as_tibble() %>% 
    select(1:3)
}


kernel_density <- function(grid, target, r, fold = 100) {
  res <- NULL
  nt <- ntile(seq_len(nrow(grid)), fold)
  for (i in seq_len(fold)) {
    cat("i = ", i, "... ", sep = "")
    batch <- grid[nt ==  i, ]
    n <- nrow(batch)
    dist_mat <- as.matrix(dist(rbind(batch, target)))
    density <- apply(dist_mat[1:n, -(1:n)] < r, 1, sum)
    res <- rbind(res, cbind(batch, density))
    cat("Good!\n")
  }
  as_tibble(res)
}
