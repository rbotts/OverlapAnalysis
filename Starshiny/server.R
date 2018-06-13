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
  
  #Convert sunrise/sunset to radians
  sunRise <- sunRise * 2 * pi
  sunSet <- sunSet * 2 * pi
  
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

materialCheckboxGroupWrite <- function(idBase, choices, initials, color, evaluate = TRUE) {
  #' Generate a group input made from several material_checkbox elements
  #'
  #' Generate a vector of shinymaterial checkbox commands.
  #' @param idBase String. Serves as a base for the inputId of the checkboxes; each checkbox's inputId will start with idBase and have a number appended.
  #' @param choices A character vector containing the labels for each checkbox to be made, respectively.
  #' @param initials A logical vector indicating the initial value for each checkbox to be made, respectively. Defaults to all FALSE, unchecked.
  #' @param color A character vector containing the color for each checkbox to be made, respectively. \emph{This input requires using color hex codes, rather than the word form. E.g., "#ef5350", rather than "red lighten-1".}
  #' @param evaluate Boolean. Should the output expresssions be parsed and evaluated automatically?
  
  #Setup
  nBoxes <- length(choices)
  if (missing(initials)) initials <- rep(FALSE, length.out = nBoxes)
  output <- c()
  
  #Color will be recycled if it is not long enough
  if (length(color) != nBoxes) {
    color <- rep(color, length.out = nBoxes)
    warning("Length of color input not the same as number of choices. Color vector recycled to match.")
  }
  
  #Initials will be recycled if it is not long enough
  if (length(color) != nBoxes) {
    initials <- rep(initials, length.out = nBoxes)
    warning("Length of initial values input not the same as number of choices. Initials vector recycled to match.")
  }
  
  #Loop to generate commands
  for (i in 1:nBoxes) {
    #Set up all the arguments
    id <- paste0("\"", idBase, as.character(i), "\"")
    lab <- paste0("\"", choices[i], "\"")
    init <- as.character(initials[i])
    col <- paste0("\"", color[i], "\"")
    
    #Add a new checkbox command to the output
    output[i] <- paste0("material_checkbox(input_id = ", id,
                        ", label = ", lab,
                        ", initial_value = ", init,
                        ", color = ", col,
                        ")")
  }
  
  if (evaluate) return(lapply(output, function(x) eval(parse(text = x)))) else return(output)
}

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
    cleanFrame <- raw.dat()
    cleanFrame <- subset(cleanFrame, cleanFrame$Independent == "Yes")
    
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
    cleanFrame <- clean.dat()
    cleanFrame <- subset(cleanFrame, cleanFrame$Species == input$speciesA)
    return(sort(unique(cleanFrame$Site)))
  })
  siteListB <- reactive({
    cleanFrame <- clean.dat()
    cleanFrame <- subset(cleanFrame, cleanFrame$Species == input$speciesB)
    return(sort(unique(cleanFrame$Site)))
  })
  
  #Filter UI ----
  output$filterUI <- renderUI({
      material_row(
        #Filter for species A
        material_column(
          width = 6,
          material_card(
            title = "Filter Species A",
            HTML("<p style = \"color:#9e9e9e\"> Sites to include?"),
            materialCheckboxGroupWrite(idBase = "siteA",
                                       choices = siteListA(),
                                       color = colHex),
            
            HTML("<br> <p style = \"color:#9e9e9e\"> Months to include?"),
            materialCheckboxGroupWrite(idBase = "monthsA",
                                       choices = 1:12,
                                       color = colHex)
          )
        ),
        
        #Filter for species B
        material_column(
          width = 6,
          material_card(
            title = "Filter Species B",
            HTML("<p style = \"color:#9e9e9e\"> Sites to include?"),
            materialCheckboxGroupWrite(idBase = "siteB",
                                       choices = siteListA(),
                                       color = colHex),
            
            HTML("<br> <p style = \"color:#9e9e9e\"> Months to include?"),
            materialCheckboxGroupWrite(idBase = "monthsB",
                                       choices = 1:12,
                                       color = colHex)
          )
        )
      )
  })
  
  #Debug button ----
  observeEvent(eventExpr = input$bootButton, handlerExpr = {
    dataPeek <<- clean.dat() #to take a peek at what's going on behind the Shiny for debugging purposes
  }, ignoreInit = TRUE)
}
