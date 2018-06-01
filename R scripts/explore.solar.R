#R Version 3.4.4
#Converting "Clock Time" to "Solar Time"
#ind.data must have a column for time ("TimeRad" -- in RADIANS) and columns for Longitude and Latitude, respectively

#source("R scripts/xy.R") #Source this (once) to get the "space" object
require("overlap")
require("maptools")
require("lubridate")

patternAnimal <- "Agouti paca"

#Format date objects nicely
dateFormat = "%m/%d/%Y"
ind.data$Date <- base::as.Date(ind.data$Date, format = dateFormat)
force_tz(ind.data$Date, tzone = "America/Costa_Rica")

ind.data["Solar"] <- sunTime(clockTime = ind.data$TimeRad,
                             Dates = as.Date(ind.data$Date),
                             Coords = space)

densityPlot(
  A = subset(ind.data$Solar, ind.data$Species == patternAnimal),
  xscale = NA,
  xlab = "Solar Position",
  xaxt = "n",
  main = patternAnimal
)
rug(
  x = subset(ind.data$Solar, ind.data$Species == patternAnimal),
  side = 1
)
axis(
  side = 1,
  at = c(0, pi/2, pi, 3*pi/2, 2*pi),
  labels = c("Midnight", "Sunrise", "Noon", "Sunset", "Midnight")
)
