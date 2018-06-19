#R Version 3.4.4
#Starshiny server script

#Setup ----
require("shiny")
require("shinymaterial")
require("overlap")
require("suncalc")
require("lubridate")
size.fileupload <- 256 #Max file size that can be uploaded is this many MB
options(shiny.maxRequestSize = size.fileupload*1024^2)

#Constants ----
colHex <- "#f4511e"
colText <- "deep-orange darken-1"

#Utility Functions ----
solarTime <- function(dat, tzone = "America/Costa_Rica") {
  #' Convert "clock times" to "solar times" based on the date and location at which the "clock times" were measured.
  #'
  #' Generate a vector of solar times (in radians), where (1/2)pi is sunrise and (3/2)pi is sunset.
  #' @param dat Data.frame. Contains four columns: "date" (the POSIXct date), "lat" (the Latitude), "lon" (the Longitude), "time" (the time of day in RADIANS)
  #' @param tzone String. The time zone. Use `OlsonNames()` to see the list of all supported on your system.
  
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
    if (length(color) > 1) warning("Length of color input not the same as number of choices. Color vector will be recycled to match.")
    color <- rep(color, length.out = nBoxes)
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

materialCheckboxGroupRead <- function(idBase, choices) {
  #' Get a character vector containing the names for the data stored by a group input made with materialCheckboxGroupWrite. Use something like `lapply(output, function(x) eval(parse(text = x)))` in a reactive context to actually read the values.
  #' 
  #' Get names for the data stored by a group input made with materialCheckboxGroupWrite.
  #' @param idBase String. The idBase used in the original materialCheckboxGroupWrite call.
  #' @param choices The character vector used in the original materialCheckboxGroupWrite call, giving the labels for each checkbox.
  
  output <- c()
  for (i in 1:length(choices)) {
    output[i] <- paste0("input$", idBase, i)
  }
  
  return(output)
}

#Server function ----
function(input, output, session) {
  
  #Upload raw.dat ----
  raw.dat <- reactive({
    upFile <- input$dataFile
    if (is.null(upFile)) {
      return(data.frame(NULL))
    } else read.csv(file = input$dataFile$datapath, header = TRUE, stringsAsFactors = FALSE)
  })
  
  #Clean raw.dat ----
  clean.dat <- reactive({
    #Show a loading spinner during processing
    material_spinner_show(session = session, output_id = "loadingSpinner")
    
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
    
    #Adding a month column
    cleanFrame$Month <- month(cleanFrame$Date)
    
    #Adding a solar column
    sunData <- data.frame(
      "date" = cleanFrame$Date,
      "lat" = cleanFrame$Latitude,
      "lon" = cleanFrame$Longitude,
      "time" = cleanFrame$TimeRad
    )
    cleanFrame["Solar"] <- solarTime(dat = sunData, tzone = input$timeZone)
    
    #Adding a lunar column
    cleanFrame["Lunar"] <- (getMoonIllumination(cleanFrame$Date, "phase")[,2])*2*pi
    
    #Hide the loading spinner when processing is complete
    material_spinner_hide(session = session, output_id = "loadingSpinner")
    
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
  output$filterUI <- renderUI(
    if (input$speciesNumber) {
    material_row(
        #Filter for species A
        material_column(
          width = 6,
          material_card(
            title = "Filter Species A",
            uiOutput(outputId = "selectSiteA"),
            uiOutput(outputId = "selectMonthA")
          )
        ),
        material_column(
          width = 6,
          material_card(
            title = "Filter Species B",
            uiOutput(outputId = "filterB")
          )
        )
    )
    } else {
      material_card(
        title = "Filter Species A",
        material_row(
          material_column(width = 6, uiOutput(outputId = "selectSiteA")),
          material_column(width = 6, uiOutput(outputId = "selectMonthA"))
        )
      )
  }
  )
  
  #Filter A ----
  output$selectSiteA <- renderUI({
    div(
      HTML("<p style = \"color:#9e9e9e\"> Sites to include?"),
      materialCheckboxGroupWrite(idBase = "siteA",
                                 choices = siteListA(),
                                 color = colHex)
    )
  })
  output$selectMonthA <- renderUI({
    div(
      HTML("<br> <p style = \"color:#9e9e9e\"> Months to include?"),
      materialCheckboxGroupWrite(idBase = "monthsA",
                                 choices = 1:12,
                                 color = colHex)
    )
  })
  
  #Filter B ----
  output$filterB <- renderUI({
    div(
      HTML("<p style = \"color:#9e9e9e\"> Sites to include?"),
      materialCheckboxGroupWrite(idBase = "siteB",
                                 choices = siteListB(),
                                 color = colHex),
      
      HTML("<br> <p style = \"color:#9e9e9e\"> Months to include?"),
      materialCheckboxGroupWrite(idBase = "monthsB",
                                 choices = 1:12,
                                 color = colHex)
    )
  })
  
  #Filter outputs ----
  siteA <- reactive({
    nameList <- materialCheckboxGroupRead(idBase = "siteA", choices = siteListA())
    valueList <- unlist(lapply(nameList, function(x) try(eval(parse(text = x)))))
    return(valueList)
  })
  monthsA <- reactive({
    nameList <- materialCheckboxGroupRead(idBase = "monthsA", choices = 1:12)
    valueList <- unlist(lapply(nameList, function(x) try(eval(parse(text = x)))))
    return(valueList)
  })
  siteB <- reactive({
    nameList <- materialCheckboxGroupRead(idBase = "siteB", choices = siteListB())
    valueList <- unlist(lapply(nameList, function(x) try(eval(parse(text = x)))))
    return(valueList)
  })
  monthsB <- reactive({
    nameList <- materialCheckboxGroupRead(idBase = "monthsB", choices = 1:12)
    valueList <- unlist(lapply(nameList, function(x) try(eval(parse(text = x)))))
    return(valueList)
  })
  
  #Data subsets ----
  a.dat <- reactive({
    cleanFrame <- clean.dat()
    if (input$removeNight) cleanFrame <- subset(cleanFrame,
                                                cleanFrame$Solar > (1/2)*pi &
                                                  cleanFrame$Solar < (3/2)*pi)
    if (input$removeDay) cleanFrame <- subset(cleanFrame,
                                              cleanFrame$Solar < (1/2)* pi |
                                                cleanFrame$Solar > (3/2)*pi)
    cleanFrame <- subset(cleanFrame[[input$modeSelect]],
                         cleanFrame$Species == input$speciesA &
                           cleanFrame$Site %in% siteListA()[siteA()] &
                           cleanFrame$Month %in% (1:12)[monthsA()]
                         )
    return(cleanFrame)
  })
  b.dat <- reactive({
    cleanFrame <- clean.dat()
    if (input$removeNight) cleanFrame <- subset(cleanFrame,
                                                cleanFrame$Solar > (1/2)*pi &
                                                  cleanFrame$Solar < (3/2)*pi)
    if (input$removeDay) cleanFrame <- subset(cleanFrame,
                                              cleanFrame$Solar < (1/2)* pi |
                                                cleanFrame$Solar > (3/2)*pi)
    cleanFrame <- subset(cleanFrame[[input$modeSelect]],
                         cleanFrame$Species == input$speciesB &
                           cleanFrame$Site %in% siteListB()[siteB()] &
                           cleanFrame$Month %in% (1:12)[monthsB()]
    )
    return(cleanFrame)
  })
  
  #Plot Card ----
  output$plotCard <- renderUI({
    if (input$speciesNumber) {
      material_card(
        title = HTML(paste("Overlap between <i>", input$speciesA, "</i>and<i>", input$speciesB, "</i>")),
        plotOutput(outputId = "abPlot", height = "480px")
      )
    }
    else {
      material_card(
        title = HTML(paste("Activity Pattern of<i>", input$speciesA, "</i>")),
        plotOutput(outputId = "aPlot", height = "480px")
      )
    }
  })
  
  #Plot A vs B ----
  output$abPlot <- renderPlot({
    overlapPlot(A = a.dat(),
                B = b.dat(), 
                xscale = NA,
                rug = TRUE,
                n.grid = 256,
                xaxt="n",
                xlab = "",
                main = NULL)
    legend("top",
           legend = c(input$speciesA, input$speciesB),
           col = c("black", "blue"),
           lty = c(1, 2)
    )
    
    #The following 3 `if` statements should be identical to those in aPlot below. If you make changes, don't forget to update that section as well.
    if (input$modeSelect == "TimeRad") {
      title(xlab = "Clock Time")
      axis(
        side = 1,
        at = c(0, pi / 2, pi, 3 * pi / 2, 2 * pi),
        labels = c("00:00", "06:00", "12:00", "18:00", "24:00")
      )
    }
    if (input$modeSelect == "Solar") {
      title(xlab = "Solar Time")
      axis(
        side = 1,
        at = c(0, pi / 2, pi, 3 * pi / 2, 2 * pi),
        labels = c("Midnight", "Sunrise", "Noon", "Sunset", "Midnight")
      )
    }
    if (input$modeSelect == "Lunar") {
      title(xlab = "Moon Phase")
      axis(
        side = 1,
        at = c(0, pi / 2, pi, 3 * pi / 2, 2 * pi),
        labels = c("New Moon", "First Quarter", "Full Moon", "Last Quarter", "New Moon")
      )
    }
  })
  
  #Plot A only ----
  output$aPlot <- renderPlot({
    densityPlot(A = a.dat(),
                xscale = NA,
                rug = FALSE,
                n.grid = 256,
                xaxt = "n",
                xlab = "",
                main = NULL)
    
    rug(
      x = a.dat(),
      side = 1
    )
    
    #The following 3 `if` statements should be identical to those in abPlot above. If you make changes, don't forget to update that section as well.
    if (input$modeSelect == "TimeRad") {
      title(xlab = "Clock Time")
      axis(
        side = 1,
        at = c(0, pi / 2, pi, 3 * pi / 2, 2 * pi),
        labels = c("00:00", "06:00", "12:00", "18:00", "24:00")
      )
    }
    if (input$modeSelect == "Solar") {
      title(xlab = "Solar Time")
      axis(
        side = 1,
        at = c(0, pi / 2, pi, 3 * pi / 2, 2 * pi),
        labels = c("Midnight", "Sunrise", "Noon", "Sunset", "Midnight")
      )
    }
    if (input$modeSelect == "Lunar") {
      title(xlab = "Moon Phase")
      axis(
        side = 1,
        at = c(0, pi / 2, pi, 3 * pi / 2, 2 * pi),
        labels = c("New Moon", "First Quarter", "Full Moon", "Last Quarter", "New Moon")
      )
    }
  })
  
  #Analysis UI ----
  output$analysisCard <- renderUI({
    material_card(
      title = "Analysis",
      HTML(paste0("<p style = \"color:#9e9e9e\"> <i><b>", input$speciesA, ":</b></i> n = ", length(a.dat()))),
      uiOutput(outputId = "nB")
    )
  })
  output$nB <- renderUI(if (input$speciesNumber) {
    HTML(paste0("<p style = \"color:#9e9e9e\"> <i><b>", input$speciesB, ":</b></i> n = ", length(b.dat())))
  })
  
  #Debug button ----
  observeEvent(eventExpr = input$bootButton, handlerExpr = {
    dataPeek <<- clean.dat() #to take a peek at what's going on behind the Shiny for debugging purposes
  }, ignoreInit = TRUE)
}
