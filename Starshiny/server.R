#R Version 3.4.4
#Starshiny server script

#Setup ----
require("shiny")
require("shinymaterial")
require("overlap")
require("suncalc")
require("lubridate")
require("proj4") 
size.fileupload <- 256 #Max file size that can be uploaded is this many MB
options(shiny.maxRequestSize = size.fileupload*1024^2)

#Constants ----
colHex <- "#f4511e"
colText <- "deep-orange darken-1"

#Server function ----
function(input, output) {
  
  #Upload raw.dat ----
  raw.dat <- reactive({
    upFile <- input$dataFile
    if (is.null(upFile)) {
      return(data.frame(NULL))
    } else read.csv(file = input$dataFile$datapath, header = TRUE, stringsAsFactors = FALSE)
  })
  
  #Clean raw.dat ----
  clean.dat <- reactive({
    #Import only independent data
    cleanFrame <- raw.dat()[raw.dat$Independent == "Yes"]
    
    #Add a radian time column
    max.time <- max(cleanFrame$Time)
    if (max.time > 12 & max.time <= 24) {
      cleanFrame["TimeRad"] <- cleanFrame$Time*2*pi/24
    } else if (max.time <= 1) {
      cleanFrame["TimeRad"] <- cleanFrame$Time*2*pi
    } else cleanFrame["TimeRad"] <- rep(NA, times = length(cleanFrame$Time))
    
    #Adding a site column
    cleanFrame["Site"] <- gsub("\\s*20\\d\\d\\s*|\\s*Spring\\s*|\\s*Summer\\s*|\\s*Fall\\s*|\\s*El\\s*|\\s*La\\s*|\\s*National Park\\s*|\\s*\\-\\s*",
                             "",
                             cleanFrame$Survey.Name)
    
    #Date extraction
    dateFormat = "%m/%d/%Y"
    cleanFrame$Date <- base::as.Date(cleanFrame$Date, format = dateFormat)
    
    #Adding a solar column
    sunData <- data.frame(
      "date" = cleanFrame$Date,
      "lat" = cleanFrame$Latitude,
      "lon" = cleanFrame$Longitude,
      "time" = cleanFrame$TimeRad
    )
    cleanFrame["Solar"] <- solarTime(sunData)
    
    #Adding a lunar column
    cleanFrame["Lunar"] <- (getMoonIllumination(cleanFrame$Date, "phase")[,2])*2*pi
    
    return(cleanFrame)
  })
  
  #Make speciesList ----
  speciesList <- reactive({
    sL <- unique(clean.dat()$Species)
    names(sL) <- sL
    return(sL)
  })
  
  #Choose species ----
  output$speciesSelect <- renderUI({
    div(
      #Label
      HTML("<br> <p style = \"color:#9e9e9e\"> Which species would you like to analyze?"),
      
      #Choose species A (dropdown)
      material_dropdown(input_id = "speciesA",
                        label = "Species A",
                        choices = speciesList(),
                        multiple = FALSE,
                        color = colHex),
      #Choose species B (dropdown)
      material_dropdown(input_id = "speciesB",
                        label = "Species B",
                        choices = speciesList(),
                        multiple = FALSE,
                        color = colHex)
    )
  })
  
  #Make siteLists ----
  siteListA <- reactive({
    cleanFrame <- clean.dat()[clean.dat()$Species == input$speciesA]
    sort(unique(cleanFrame$Site))
  })
  siteListB <- reactive({
    cleanFrame <- clean.dat()[clean.dat()$Species == input$speciesB]
    sort(unique(cleanFrame$Site))
  })
  
  #Filter UI ----
  output$filterUI <- renderUI({
      material_row(
        material_column(
          width = 6,
          material_card(
            title = "Filter Species A",
            HTML("<p style = \"color:#9e9e9e\"> Sites to include?"),
            checkboxGroupInput(inputId = "siteA",
                               label = NULL,
                               choices = siteListA(),
                               selected = siteListA()
                               ),
            
            HTML("<br> <p style = \"color:#9e9e9e\"> Months to include?"),
            checkboxGroupInput(inputId = "monthsA",
                               label = NULL,
                               choices = 1:12,
                               selected = NULL
                               )
          )
        ),
        material_column(
          width = 6,
          material_card(
            title = "Filter Species B",
            HTML("<p style = \"color:#9e9e9e\"> Sites to include?"),
            checkboxGroupInput(inputId = "siteA",
                               label = NULL,
                               choices = siteListB(),
                               selected = siteListB()
                               ),
            
            HTML("<br> <p style = \"color:#9e9e9e\"> Months to include?"),
            checkboxGroupInput(inputId = "monthsA",
                               label = NULL,
                               choices = 1:12,
                               selected = NULL
                               )
          )
        )
      )
  })
  
  #Debug button ----
  observeEvent(eventExpr = input$bootButton, handlerExpr = {
    dataPeek <<- raw.dat() #to take a peek at what's going on behind the Shiny for debugging purposes
  }, ignoreInit = TRUE)
}

#Utility Functions ----
solarTime <- function(dat, tzone = "America/Costa_Rica") {
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
