#R Version 3.4.4
#Converting X/Y geographic points to Latitude/Longitude
require("maptools")
require("proj4") #Requires installing libproj-dev on your system first

xy <- function(x, y) {
  proj4string <- "+proj=utm +zone=17 +ellps=WGS84 +datum=WGS84 +units=m +no_defs "
  pj <- project(data.frame(x,y), proj4string, inverse=TRUE)
  return(data.frame(Longitude=pj$x, Latitude=pj$y))
}

#Adding Longitude and Latitude columns to ind.data
xy.dat <- xy(x = ind.data$X, y = ind.data$Y)
ind.data <- cbind(ind.data, xy.dat)
space <- SpatialPoints(coords = xy.dat, proj4string = "+proj=longlat +ellps=WGS84")