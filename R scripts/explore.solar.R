#R Version 3.4.4
#Converting "Clock Time" to "Solar Time"
#ind.data must have a column for time ("TimeRad" -- in RADIANS) and columns for Longitude and Latitude, respectively

source("R scripts/data.correction.R") #Creates the raw.data object

#Required packages
require("overlap")
require("suncalc")
require("lubridate")

patternAnimal <- "Agouti paca"
timeZone <- "America/Costa_Rica"
ind.data <- raw.data[raw.data$Independent == "Yes", ]

#Format date objects nicely
dateFormat = "%m/%d/%Y"
ind.data$Date <- base::as.Date(ind.data$Date, format = dateFormat)

#Convert Time to Radians
max.time <- max(ind.data$Time)
if (max.time > 12 & max.time <= 24) {
  ind.data["TimeRad"] <- ind.data$Time*2*pi/24
} else if (max.time <= 1) {
  ind.data["TimeRad"] <- ind.data$Time*2*pi
} else print("Unknown time format.")

#Get Solar data
solarTime <- function(dat) {
  #Loosely based on sunTime function from package "overlap"
  #
  #inputs: 'dat' is a data.frame with the following columns: "date" (the POSIXct date), "lat" (the Latitude), "lon" (the Longitude), "time" (the time of day in RADIANS)
  #ouptuts: 'solar' is a vector of "solar times" (in RADIANS) where (1/2)pi is sunrise and (3/2)pi is sunset
  
  
  #Get sunrise and sunset as date-time objects
  sunData <- getSunlightTimes(data = dat, keep = c("sunrise", "sunset"), tz = timeZone)
  sunRise <- sunData$sunrise
  sunSet <- sunData$sunset
  
  #Get sunrise and sunset as fraction of a day (start is forced to 00:00:00 UTC that day, end is appropriate solar event forced to UTC)
  sunRise <- time_length(interval(start = ymd_hms(paste(as.Date(sunRise), "00:00:00")),
                                  end = force_tz(time = sunRise, tzone = "UTC")),
                         unit = "day")
  sunSet <- time_length(interval(start = ymd_hms(paste(as.Date(sunSet), "00:00:00")),
                                 end = force_tz(time = sunSet, tzone = "UTC")),
                        unit = "day")
  
  #Convert sunrise/sunset to radians
  sunRise <- sunRise * 2 * pi
  sunSet <- sunSet * 2 * pi
  
  #Converting clock times to solar times
  clockTime <- dat["time"]
  solar <- ifelse(
    test = (clockTime <= sunRise), #is it before sunrise
    
    #scale pre-dawn times to lie between 0 and (1/2)pi
    yes = (clockTime / sunRise) * (1/2) * pi,
    
    no = ifelse(
      test = (clockTime <= sunSet), #is it between sunrise and sunset?
      
      #scale daylight times to lie between (1/2pi) and (3/2)pi
      yes = (((clockTime - sunRise) / (sunSet - sunRise)) * pi) + ((1/2) * pi),
      
      #scale post-dusk times to lie between (3/2)pi and 2pi
      no =  (((clockTime - sunSet) / ((2 * pi) - sunSet)) * (1/2) * pi) + (3/2) * pi
    )
  )
  
  return(solar)
}

sunData <- data.frame(
  "date" = ind.data$Date,
  "lat" = ind.data$Latitude,
  "lon" = ind.data$Longitude,
  "time" = ind.data$TimeRad
)

ind.data["Solar"] <- solarTime(sunData)

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
