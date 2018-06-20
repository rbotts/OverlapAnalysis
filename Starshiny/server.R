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

watson2 <- function(x, y) {
  #' Calculate Watson's U-squared statistic for two vectors of circular data in radians. Note that this algorithm is modified to work with "tied" data as suggested by Zar in "Biostatistical Analysis" (1999).
  #' @param x A numerical vector of measured points for the first circular data set (in RADIANS).
  #' @param y A numerical vector of measured points for the second circular data set (in RADIANS).
  
  n1 <- length(x)
  n2 <- length(y)
  if (min(n1, n2) <= 17) return("Sample too small. Both must be >17.")
  N <- n1+n2
  
  a <- c(x,y) #Putting all unique values into one ordered set, creating a unified coordinate system
  a <- a[duplicated(a) == FALSE]
  a <- a[order(a)]
  kmax <- length(a)
  
  t1 <- t2 <- m1 <- m2 <- c1 <- c2 <- rep.int(0, kmax) #Preallocating several variables
  
  for (k in 1:kmax) { #Finding the frequency of each value in x and y, respectively
    t1[k] <- sum(x==a[k])
    t2[k] <- sum(y==a[k])
  }
  
  m1 <- cumsum(t1) #Calculating the cumulative frequency distributions of x and y, respectively
  m2 <- cumsum(t2)
  
  c1 <- m1/n1 #Calculating the cumulative relative frequency distributions of x and y, respectively
  c2 <- m2/n2
  
  d <- c1-c2 
  t <- t1+t2 #The total frequency of each value; used when there are multiple observations of a value (ties)
  
  da <- sum(d*t)
  db <- sum(d*d*t)
  U2 <- ((n1*n2)/(N^2))*(db - ((da^2)/N)) #Calculating the U-squared statistic
  
  return(U2)
}

watson2test <- function(x, y) { 
  #' Estimate the p-value of Watson's U-squared statistic for two given vectors, following the algorithm of Tiku from "Chi-Square Approximations for the Distributions of Goodness-Of-Fit Statistics U^2 and W^2" in Biometrika (1965).
  #' @param x A numerical vector of measured points for the first circular data set (in RADIANS).
  #' @param y A numerical vector of measured points for the second circular data set (in RADIANS).
  
  U2 <- watson2(x, y) #Calculate U-squared
  N <- length(x) + length(y)
  
  a <- ((21*N)-56)/(840*(N-1.5))
  b <- (N-1.5)/(42*N)
  f <- ((49*N)*(N-1))/(20*((N-1.5)^2)) #Approximation constants
  
  chi <- (U2-a)/b
  p <- pchisq(q = chi, df = f, lower.tail = FALSE) #Approximating from chi-squared distribution
  return(p)
}

w.stat <- function(...) {
  #' Calculates the r-sample uniform scores W statistic for r sets of circular data. See: NI Fisher "Statistical analysis of circular data". Section 5.3.6, page 122. Book. (1993)
  #' 
  #' @param ... Two or more numerical vectors of circular data (in RADIANS).
  
  #Data input and organization
  dataList <- list(...)
  unData <- data.frame(Point = sort(unlist(dataList)))
  n <- unlist(lapply(dataList, length)) #n is vector of sample sizes of each input vector
  N <- sum(n) #N is the *total* sample size, how many points were recorded overall
  r <- length(n) #r is the number of vectors input
  
  #Linear Rank
  unData["Rank"] <- 1:N
  for (i in 1:N) {
    whenSame <- unData$Point == unData$Point[i] #Create logical vector of which points of unData$Point are the same as a given point
    numSame <- sum(whenSame) #How many times does this value appear?
    
    if (numSame > 1) { #If a value appears more than once...
      unData$Rank[whenSame] <- rep(mean(unData$Rank[whenSame]), times = numSame) #...every point with that value should take the average Rank
    }
  }
  
  unData["Uniform"] <- 2 * pi * unData$Rank / N #Circular Rank, a.k.a: "Uniform Score"
  
  #Calculating W
  C <- S <- rep(0, times = length(n)) #Preallocate values
  for (i in 1:length(n)) {
    for (j in 1:n[i]) {
      rankOrder <- unData$Uniform[unData$Point == dataList[[i]][j]]
      rankOrder <- mean(rankOrder) #mean() condenses rankOrder to a single value if vector
      C[i] <- C[i] + cos(rankOrder)
      S[i] <- S[i] + sin(rankOrder)
    }
  }
  
  W <- 2 * sum((C^2 + S^2)/n)
  return(W)
}

w.prob.chi <- function(...) {
  #' Estimates the p-value of an r-sample uniform scores W statistic for r sets of circular data. See: NI Fisher "Statistical analysis of circular data". Section 5.3.6, page 122. Book. (1993)
  #' 
  #' @param ... Two or more numerical vectors of circular data (in RADIANS).
  
  #Data input and organization
  dataList <- list(...)
  W0 <- do.call(what = w.stat, args = dataList) #W statistic of actual data
  n <- unlist(lapply(dataList, length)) #n is vector of sample sizes of each input vector
  r <- length(n) #r is the number of vectors input
  
  return(pchisq(q = W0, df = 2*r - 2, lower.tail = FALSE)) #Approximate with Chi-square
}

overlapCI <- function(animal1, animal2, n.boot = 10000) {
  #' Estimates the overlap coefficient (\Delta) from two sets of circular data. See: Ridout and Linkie, "Estimating Overlap of Daily Activity Patterns From Camera Trap Data" in \emph{Journal of Agricultural, Biological, and Environmental Statistics} (2009)
  #' 
  #' @param animal1 A numerical vector of measured points for the first species in RADIANS.
  #' @param animal2 A numerical vector of measured points for the second species in RADIANS.
  #' @param n.boot How many bootstrap trials should be run? Defaults to ten thousand.
  
  require("overlap")
  
  #Preallocate output
  ovl <- c()
  
  if (min(length(animal1), length(animal2)) <= 75 & min(length(animal1), length(animal2)) > 15) {
    ovl["estimate"] <- overlapEst(animal1, animal2, adjust=c(0.8, NA, NA))[1]
    boot1 <- resample(animal1, n.boot)
    boot2 <- resample(animal2, n.boot)
    ovl.boot <- bootEst(boot1, boot2, adjust=c(0.8, NA, NA))[,1]
    ovl.boot.ci <- bootCI(ovl["estimate"], ovl.boot)
    ovl["estimate"] <- round(ovl["estimate"], digits = 4)
    ovl["lower"] <- round(ovl.boot.ci[4,1], digits = 4)
    ovl["upper"] <- round(ovl.boot.ci[4,2], digits = 4)
    
  } else if (min(length(animal1), length(animal2)) > 75) {
    ovl["estimate"] <- overlapEst(animal1, animal2, adjust=c(NA, 1, NA))[2]
    boot1 <- resample(animal1, n.boot)
    boot2 <- resample(animal2, n.boot)
    ovl.boot <- bootEst(boot1, boot2, adjust=c(NA, 1, NA))[,2]
    ovl.boot.ci <<- bootCI(ovl["estimate"], ovl.boot)
    ovl["estimate"] <- round(ovl["estimate"], digits = 4)
    ovl["lower"] <- round(ovl.boot.ci[4,1], digits = 4)
    ovl["upper"] <- round(ovl.boot.ci[4,2], digits = 4)
  } else ovl["estimate"] <- "Sample too small"
  
  return(ovl)
}

permutationTest <- function(FUN, ..., trials = 10000, rightTail = TRUE) {
  #' Performs a permutation test to estimate the probability that a statistic would have a value greater than (or less than, if rightTail = FALSE) the actual value obtained.
  #' 
  #' @param FUN A function that outputs a single numeric value when given two or more data vectors.
  #' @param ... The arguments to be passed into FUN, calculating the "actual" statistical estimate.
  #' @param trials Integer. How many trials should be run? Defaults to ten thousand.
  #' @param rightTail Boolean. Should the p-value be given integrated for the right-side tail?
  
  dataList <- list(...)
  val0 <- do.call(what = FUN, args = dataList) #Statistic of actual data
  n <- unlist(lapply(dataList, length)) #n is vector of sample sizes of each input vector
  N <- sum(n) #N is the *total* sample size, how many points were recorded overall
  r <- length(n) #r is the number of vectors input
  
  #Randomization test
  valRand <- rep(x = NA, times = trials) #Preallocate result vector
  
  for (i in 1:trials) {
    
    #Preallocate dataList for this trial
    randList <- list()
    
    #Randomly resample points for each data vector from total population without replacement
    randDat <- sample(x = unlist(dataList), size = N, replace = FALSE)
    
    #Generate the resampled datasets
    for (j in 1:r) {
      if (j == 1) randList[[1]] <- randDat[1:n[j]] #The first "new" data vector has n[1] points
      else {
        nStart <- length(unlist(randList))
        randList[[j]] <- randDat[(nStart+1):(nStart+n[j])] #The j'th "new" data vector has n[j] points
      }
    }
    valRand[i] <- do.call(what = FUN, args = randList) #Once all "new" data vectors constructed, find FUN of "new" dataset and store it in the valRand vector
  }
  valSort <- sort(valRand) #Arrange results from randomization in ascending order
  p <- which.min(abs(valSort - val0)) #Figure out where val0 fits into the randomized distribution
  
  #Return the right or left p-value, depending on rightTail argument
  if (rightTail) return(c("val" = val0, "p.value" = 1-(p/trials))) else return(c("val" = val0, "p.value" = (p/trials)))
}

circularFisherTest <- function(animal1, animal2, bins = 12) {
  #' Performs a Fisher's Exact Test on two circular data sets to analyze similarity.
  #' 
  #' @param animal1 A numerical vector of measured points for the first species in RADIANS.
  #' @param animal2 A numerical vector of measured points for the second species in RADIANS.
  #' @param bins Into how many bins should the data be divided before analysis? Defaults to 12.
  
  binning <- function(x, bins) {
    #' Break a circular data set into equally-spaced bins \emph{(e.g: for chi-square analysis)}.
    #' 
    #' @param x A numerical vector of measured points in RADIANS
    #' @param bins Integer. How many bins should `x` be divided into?
    
    #Create boundaries for the bins
    cutoffs <- seq(from = 0,
                   to = 2 * pi,
                   length.out = bins + 1)
    n <- length(cutoffs)
    
    #Preallocate the output
    output <- rep(NA, times = length(x))
    
    for (i in cutoffs[-n]) {
      output <- ifelse(test = x >= i, #If x is greater than or equal to cutoff[i]...
                       yes = which(i == cutoffs), #Then output should be i
                       no = output) #Else it should stay what it is)
    }
    
    return(output)
  }
  
  animal1bin <- binning(x = animal1, bins = bins)
  animal1bin <- setNames(tabulate(animal1bin), 1:max(animal1bin))
  animal2bin <- binning(x = animal2, bins = bins)
  animal2bin <- setNames(tabulate(animal2bin), 1:max(animal2bin))
  animalbins <- rbind(animal1bin, animal2bin)
  size = 2 * bins
  
  chistat <- fisher.test(animalbins, simulate.p.value = TRUE, B = 100000)
  
  return(
    tryCatch(expr = {round(x = chistat["p.value"], digits = 4)},
             error = function(cond) {"Error! Sample size probably too small."})
  )
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
    material_card(title = "Analyze",
                  HTML(
                    paste0(
                      "<p style = \"color:#9e9e9e\"> <i><b>",
                      input$speciesA,
                      ":</b></i> n = ",
                      length(a.dat())
                    )
                  ),
                  uiOutput(outputId = "nB"))
  })
  output$nB <- renderUI(if (input$speciesNumber) {
    div(
      #Sample size of B
      HTML(
        paste0(
          "<p style = \"color:#9e9e9e\"> <i><b>",
          input$speciesB,
          ":</b></i> n = ",
          length(b.dat())
        )
      ),
      
      #Overlap statistic estimates
      HTML(
        paste0(
          "<p style = \"color:#9e9e9e\"> ",
          #Overlap
          "<br> Estimated overlap coefficient &Delta;: ",
          tryCatch(
            expr = round(overlapEst(a.dat(), b.dat(), adjust=c(0.8, NA, NA))[1], digits = 4),
            error = function(cond) "Error!"
          ),
          
          #Watson's U-squared
          "<br> Watson's U<sup>2</sup>: ",
          tryCatch(
            expr = {
              if (is.numeric(watson2(x = a.dat(), y = b.dat()))) {
                round(watson2(x = a.dat(), y = b.dat()), digits = 4)
              } else watson2(x = a.dat(), y = b.dat())
            },
            error = function(cond) "Error!"
          ),
          
          #Uniform scores Wr
          "<br> Uniform Scores W<sub>2</sub>: ",
          tryCatch(
            expr = round(w.stat(a.dat(), b.dat()), digits = 4),
            error = function(cond) "Error!"
          )
        )
      )
    )
  })
  
  #Bootstrap UI ----
  output$bootCard <- renderUI(if (input$speciesNumber) {
    material_card(
      title = "Calculate",
      HTML("<p style = \"color:#9e9e9e\"> Estimate a confidence interval (Overlap coefficient) and p-values (Watson's U<sup>2</sup>, Uniform Scores W<sub>2</sub>, and Fisher's Exact Test) using permutation tests. <br> <br>"),
      material_row(
        material_column(
          width = 12,
          align = "center",
          material_slider(input_id = "bootSlider",
                          label = "Thousands of bootstrap trials",
                          min_value = 1,
                          max_value = 10,
                          initial_value = 10,
                          color = colHex),
          material_button(input_id = "bootButton",
                          label = "Confidence Intervals and P-Values",
                          color = colText),
          uiOutput(outputId = "analyses"),
          uiOutput(outputId = "analysisSpinner")
        )
      )
    )
  })
  
  #Calculation ----
  analysis <- eventReactive(eventExpr = input$bootButton, valueExpr = {
    material_spinner_show(session = session, output_id = "analysisSpinner")
    
    overlapResults <- overlapCI(animal1 = a.dat(),
                                animal2 = b.dat(),
                                n.boot = input$bootSlider*1000)
    watsonResults <- permutationTest(FUN = watson2,
                                     a.dat(),
                                     b.dat(),
                                     trials = input$bootSlider*1000)
    uniformResults <- permutationTest(FUN = w.stat,
                                      a.dat(),
                                      b.dat(),
                                      trials = input$bootSlider*1000)
    
    material_spinner_hide(session = session, output_id = "analysisSpinner")
    
    return(list(overlap = overlapResults, watson = watsonResults, uniform = uniformResults))
  }, ignoreInit = TRUE)
  
  output$analyses <- renderUI({
    material_row(
      material_column(
        width = 12,
        HTML(paste0(
          #Overlap
          "<br>",
          "<u>95% Confidence Interval for &Delta;</u> <br>",
          analysis()$overlap["lower"], " to ", analysis()$overlap["upper"],
          "<br> <br>",
          
          #Watson's U^2
          "<u>Watson's U<sup>2</sup></u> <br>",
          "Permutation test estimated p-value: ",
          try(round(
            x = analysis()$watson["p.value"],
            digits = 4
          )),
          "<br>",
          "Chi-Square Approximated p-value: ",
          try(round(
            x = watson2test(a.dat(), b.dat()),
            digits = 4
          )),
          "<br> <br>",
          
          #Uniform Scores
          "<u>Uniform Scores W<sub>2</sub></u> <br>",
          "Permutation test estimated p-value: ",
          try(round(
            x = analysis()$uniform["p.value"],
            digits = 4
          )), "<br>",
          "Chi-Square Approximated p-value: ",
          try(round(
            x = w.prob.chi(a.dat(), b.dat()),
            digits = 4
          )),
          "<br> <br>",
          
          #Fisher's Exact
          "<u>Fisher's Exact Test, 12 bins, 100k trials</u> <br>",
          circularFisherTest(animal1 = a.dat(),
                             animal2 = b.dat(),
                             bins = 12),
          "<br>"
        ))
      )
    )
    
  })
  
}
