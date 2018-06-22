#R Version 3.4.4
#Classify species as Diurnal, Nocturnal, or Crespuscular

#Required packages
require("suncalc")
require("lubridate")

raw.dat <- read.csv("~/Documents/MooringData-June2018-Clean.csv")
ind.dat <- raw.dat[raw.dat$Independent == "Yes", ]

#Format date objects nicely
dateFormat = "%m/%d/%Y"
ind.dat$Date <- base::as.Date(ind.dat$Date, format = dateFormat)
timeZone <- "America/Costa_Rica"

#Convert Time to Radians
max.time <- max(ind.dat$Time)
if (max.time > 12 & max.time <= 24) {
  ind.dat["TimeRad"] <- ind.dat$Time*2*pi/24
} else if (max.time <= 1) {
  ind.dat["TimeRad"] <- ind.dat$Time*2*pi
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
  "date" = ind.dat$Date,
  "lat" = ind.dat$Latitude,
  "lon" = ind.dat$Longitude,
  "time" = ind.dat$TimeRad
)

#Add solar data to data.frame
ind.dat["Solar"] <- solarTime(sunData)
ind.dat["SolarHour"] <- 24*ind.dat$Solar/(2*pi)

#Sort observtions into day/night/crepuscular
ind.dat["DayTime"] <- ifelse(test = ind.dat$Solar < (1/2)*pi | ind.dat$Solar > (3/2)*pi,
                                    yes = "Night",
                                    no = "Day")
ind.dat["DayTime"] <- ifelse(test = (ind.dat$SolarHour > 5 & ind.dat$SolarHour < 7) |
                                    (ind.dat$SolarHour > 17 & ind.dat$SolarHour < 19),
                             yes = "Crepuscular",
                             no = ind.dat$DayTime)

speciesList <- sort(unique(ind.dat$Species))
output <- list()

#Tabulate percentage of day/night/crepuscular for each species
for (species in speciesList) {
  temp.dat <- ind.dat[ind.dat$Species == species,]
  dayTime <- table(temp.dat$DayTime)
  dayTime <- dayTime/sum(dayTime)
  
  output[[species]] <- c("Species" = species,
                         "N" = length(temp.dat$DayTime),
                         "Day" = tryCatch(dayTime[["Day"]], error = function(cond) {0}),
                         "Night" = tryCatch(dayTime[["Night"]], error = function(cond) {0}),
                         "Crepuscular" = tryCatch(dayTime[["Crepuscular"]], error = function(cond) {0})
                         )
}

#Put results into a nicely formatted data.frame
results <- data.frame(t(output[[1]]), stringsAsFactors = FALSE)

for (i in 2:length(output)) {
  results <- rbind(results, t(output[[i]]))
}

#Convert to proper data types
results$Night <- as.numeric(as.character(results$Night))
results$N <- as.integer(results$N)
results$Day <- as.numeric(results$Day)
results$Crepuscular <- as.numeric(results$Crepuscular)
results[,3:5] <- round(results[,3:5], digits = 4)

#Classify and export data
results["Classification"] <- ifelse(test = results$Night <= 0.1,
                                    yes = "Diurnal",
                                    no = ifelse(test = results$Night > 0.9,
                                                yes = "Nocturnal",
                                                no = ifelse(test = results$Night > 0.1 & results$Night <= 0.3,
                                                            yes = "Mostly Diurnal",
                                                            no = ifelse(test = results$Night > 0.7 & results$Night <= 0.9,
                                                                        yes = "Mostly Nocturnal",
                                                                        no = "Cathemeral"))))
write.csv(x = results, file = "~/Downloads/dayTime-June2018.csv", row.names = FALSE)
