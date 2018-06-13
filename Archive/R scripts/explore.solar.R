#R Version 3.4.4
#Converting "Clock Time" to "Solar Time"
#ind.data must have a column for time ("TimeRad" -- in RADIANS) and columns for Longitude and Latitude, respectively

source("Archive/R scripts/data.correction.R") #Creates the raw.data object

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
solarTime <- function(dat, tzone = timeZone) {
  #inputs: 'dat' is a data.frame with the following columns: "date" (the POSIXct date), "lat" (the Latitude), "lon" (the Longitude), "time" (the time of day in RADIANS)
  #ouptuts: 'solar' is a vector of "solar times" (in RADIANS) where (1/2)pi is sunrise and (3/2)pi is sunset
  
  #Get sunrise and sunset as date-time objects
  sunData <- getSunlightTimes(data = dat, keep = c("sunrise", "sunset"), tz = tzone)
  sunRise <- sunData$sunrise
  sunSet <- sunData$sunset
  
  #Get sunrise and sunset as fraction of a day (start is forced to 00:00:00 UTC that day, end is appropriate solar event forced to UTC)
  sunRise <- time_length(interval(start = ymd_hms(paste(date(sunRise), "00:00:00"), tz = tzone),
                                  end = force_tz(time = sunRise, tzone = tzone)),
                         unit = "day")
  sunSet <- time_length(interval(start = ymd_hms(paste(date(sunSet), "00:00:00"), tz = tzone),
                                 end = force_tz(time = sunSet, tzone = tzone)),
                        unit = "day")
  # plot(sunRise, ylim = c(0,1), pch = ".")
  # plot(sunSet, ylim = c(0,1), pch = ".")
  
  #Convert sunrise/sunset to radians
  sunRise <- sunRise * 2 * pi
  sunSet <- sunSet * 2 * pi
  # plot(sunRise, ylim = c(0, 2*pi), pch=".")
  # plot(sunSet, ylim = c(0,2*pi), pch=".")
  
  clockTime <- dat[["time"]]
  solar <- rep(NA, times = length(clockTime))
  
  for (i in 1:length(clockTime)) {
    if (clockTime[i] <= sunRise[i]) {
      solar[i] <- ((1/2)*pi) * (clockTime[i]/sunRise[i]) #Predawn observations
    } else if (clockTime[i] <= sunSet[i]) {
      solar[i] <- (((clockTime[i] - sunRise[i])/(sunSet[i] - sunRise[i]))*pi) + ((1/2)*pi) #Daylight observations
    } else {
      solar[i] <- (((clockTime[i] - sunSet[i])/((2*pi) - sunSet[i]))*(1/2)*pi) + ((3/2)*pi) #Postdusk observations
    }
  }
  
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
