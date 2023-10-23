library(rnaturalearth)
library(sf)
library(ggplot2)
library(viridis)
library(patchwork)
library(raster)

suitability <- raster("LassaX/2016_chikv_suitability.tif")

map1 <- ne_countries(type = "countries", country = "Bulgaria", scale = "medium", returnclass = "sf")
map2 <- rnaturalearth::ne_states("Bulgaria", returnclass = "sf")
p1 <- ggplot(map1) + geom_sf()
p2 <- ggplot(map2) + geom_sf()
p1 + p2

str(map2)


