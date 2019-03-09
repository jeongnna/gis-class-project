library(tidyverse)
library(readxl)
library(rgdal)
library(rgeos)  # gCentroid()


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
base_map <- readOGR(path, "TL_SCCO_EMD", encoding = "EUC-KR")
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

region_order <- match(base_map@data$EMD_KOR_NM, pop$region)

pop <-
  pop %>% 
  slice(region_order) %>% 
  mutate(area = area,
         density = n / area)

coordinates(pop) <- centroid@coords
proj4string(pop) <- crs_korea

# floating population data
shp_dir <- "data/shapefiles"
fpop <- readOGR(shp_dir, "S_data_floating_pop", encoding = "EUC-KR")
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
