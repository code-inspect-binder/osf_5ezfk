
# testing SantaLucia shapefile
require(raster)
require(maptools)
require(rgdal)
require(tidyverse)

setwd("C:/Users/rver4657/ownCloud/SouthSouthNetwork/Resources/UruguaySWAT/UruguayCourse")

# Reading the shape file of the catchment: move this to the Inputdata dir
SantaLucia <- readShapePoly("data/SL_Shape/subcuencaSantaLucia.shp")
crs(SantaLucia) <-  "+proj=utm +zone=21 +south +datum=WGS84 +units=m +no_defs"
SantaLucia_ll<-spTransform(SantaLucia,CRS("+proj=longlat +datum=WGS84 +no_defs"))


gp <- ggplot(SantaLucia_ll, aes(x = long, y = lat)) + geom_polygon(fill="gray75") +
  coord_equal()
gp

# reading in the subbasin centroids
sub_b_sp <- SpatialPoints(cbind(Subbasins$Long_, Subbasins$Lat),
                          proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs"))

