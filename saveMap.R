library(rgdal)
library(spdplyr)
library(geojsonio)
library(rmapshaper)

shapes <- readOGR("STE11aAust.shp")
shapes <- geojson_json(shapes)
shapes <- ms_simplify(shapes)
geojson_write(shapes, file = "map.GeoJSON")
