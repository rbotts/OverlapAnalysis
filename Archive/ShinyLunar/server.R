#Shiny app for lunar analysis
require(shiny)
require(overlap)
require(suncalc)
size.fileupload <- 64 #Max file size that can be uploaded is this many MB
options(shiny.maxRequestSize = size.fileupload*1024^2)

#Overlap Functions ----
overlapCI <- function(animal1, animal2, n.boot) {
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

#Watson Functions ----
watson2 <- function(x, y) { #Function to calculate U-squared statistic, modified to work with "tied" data (See: Zar 1999)
  n1 <- length(x)
  n2 <- length(y)
  if (min(n1, n2) <= 17) return("Sample too small")
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

watson2test <- function(x, y) { #Function to approximate the p-value of Watson's U-squared, given two vectors (See: Tiku 1965)
  U2 <- watson2(x, y) #Calculate U-squared
  N <- length(x) + length(y)
  
  a <- ((21*N)-56)/(840*(N-1.5))
  b <- (N-1.5)/(42*N)
  f <- ((49*N)*(N-1))/(20*((N-1.5)^2)) #Approximation constants
  
  chi <- (U2-a)/b
  p <- pchisq(q = chi, df = f, lower.tail = FALSE) #Approximating from chi-squared distribution
  return(p)
}

#Server function ----
function(input, output) {
  #Generate dataset ----
  observeEvent(eventExpr = input$dataButton, handlerExpr = {
    ind.data <<- read.table(file = tail((input$updata)$datapath, n=1), header=TRUE, sep=",", stringsAsFactors=FALSE)
    ind.data <<- subset(ind.data, ind.data["Independent"] ==  "Yes")
    ind.data <<- subset(ind.data, ind.data$Species != "Unknown")
    ind.data <<- subset(ind.data, ind.data$Species != "unknown")
    ind.data <<- subset(ind.data, ind.data$Species != "")
    
    ind.data["Date"] <<- as.Date(ind.data$Date, "%m/%d/%Y")
    ind.data["Lunar"] <<- (getMoonIllumination(ind.data$Date, "phase")[,2])*2*pi
    
    ind.data["Site"] <<- gsub("\\s*20\\d\\d\\s*|\\s*Spring\\s*|\\s*Summer\\s*|\\s*Fall\\s*|\\s*El\\s*|\\s*La\\s*|\\s*National Park\\s*", "", ind.data$Survey.Name)
    ind.data["Season"] <<- ifelse(grepl("Spring", ind.data$Survey.Name), "Spring", ifelse(grepl("Summer", ind.data$Survey.Name), "Summer", ifelse(grepl("Fall", ind.data$Survey.Name), "Fall", "Other")))
    
    #Remove daytime data ----
    observeEvent(eventExpr = input$nocturnalButton, handlerExpr = {
      ind.data <<- subset(ind.data, ind.data$Time > 18/24 | ind.data$Time < 6/24)
    })
    
    #Pattern Analysis UI ----
    #* Pattern Select ----
    output$patternSelect <- renderUI({
      namelist <<- names(table(ind.data$Species))
      seasonlist <<- names(table(ind.data$Season))
      sitelist <<- names(table(ind.data$Site))
      
      wellPanel(
        selectInput(
          label = "Choose the species to analyze:",
          inputId = "patternAnimal",
          choices = namelist
        ),
        checkboxGroupInput(
          label = "Filter by season(s):",
          inputId = "patternSeason",
          choices = seasonlist,
          selected = seasonlist
        ),
        checkboxGroupInput(
          label = "Filter by site(s):",
          inputId = "patternSite",
          choices = sitelist,
          selected = sitelist
        )
      )
    })
    
    #* Pattern Plot ----
    output$patternGraph <- renderPlot({
      densityPlot(
        subset(ind.data$Lunar,
               ind.data$Species == input$patternAnimal &
                 ind.data$Season %in% input$patternSeason &
                 ind.data$Site %in% input$patternSite
               ),
        xscale = NA,
        xlab = "Lunar Phase",
        xaxt = "n",
        main = input$patternAnimal
      )
      rug(
        subset(ind.data$Lunar,
               ind.data$Species == input$patternAnimal &
                 ind.data$Season %in% input$patternSeason &
                 ind.data$Site %in% input$patternSite
        ),
        side = 1
      )
      axis(
        side = 1,
        at = c(0, pi/2, pi, 3*pi/2, 2*pi),
        labels = c("New Moon", "First Quarter", "Full Moon", "Last Quarter", "New Moon")
      )
    })
    
    #* Pattern N ----
    output$patternN <- renderText({
      paste0(
        "n = ",
        length(
          subset(ind.data$Lunar,
                 ind.data$Species == input$patternAnimal &
                   ind.data$Season %in% input$patternSeason &
                   ind.data$Site %in% input$patternSite
          )
        )
      )
    })
    
    #Lunar Overlap UI ----
    #* Overlap Selection ----
    output$overlapSelect <- renderUI({
      namelist <<- names(table(ind.data$Species))
      surveylist <<- names(table(ind.data$Survey.Name))
      L <- length(surveylist) %/% 2
      
      wellPanel(
        fluidRow(
          column(3,
            selectInput(
              label = "Choose the first species to analyze:",
              inputId = "overlapAnimal1",
              choices = namelist
            ),
            checkboxGroupInput(
              label = "Choose the surveys to include for the first species:",
              inputId = "overlapSurvey1a",
              choices = surveylist[1:L],
              selected = surveylist[1:L]
            )
          ),
          column(3,
            checkboxGroupInput(
              label = "Choose the surveys to include for the first species (continued):",
              inputId = "overlapSurvey1b",
              choices = surveylist[-(1:L)],
              selected = surveylist[-(1:L)]
            )
          ),
          column(3,
            selectInput(
              label = "Choose the second species to analyze:",
              inputId = "overlapAnimal2",
              choices = namelist
            ),
            checkboxGroupInput(
              label = "Choose the surveys to include for the second species:",
              inputId = "overlapSurvey2a",
              choices = surveylist[1:L],
              selected = surveylist[1:L]
            )
          ),
          column(3,
            checkboxGroupInput(
              label = "Choose the surveys to include for the second species (continued):",
              inputId = "overlapSurvey2b",
              choices = surveylist[-(1:L)],
              selected = surveylist[-(1:L)]
            )
          )
        )
      )
    })
    
    #* Overlap Plot ----
    output$overlapPlot <- renderPlot({
      overlapPlot(
        subset(
          ind.data$Lunar,
          ind.data$Species == input$overlapAnimal1 &
            (
              ind.data$Survey.Name %in% input$overlapSurvey1a |
              ind.data$Survey.Name %in% input$overlapSurvey1b
            )
        ),
        subset(
          ind.data$Lunar,
          ind.data$Species == input$overlapAnimal2 &
            (
              ind.data$Survey.Name %in% input$overlapSurvey2a |
              ind.data$Survey.Name %in% input$overlapSurvey2b
            )
        ),
        xscale = NA,
        xlab = "Lunar Phase",
        xaxt = "n",
        main = paste0("Lunar Overlap between ", input$overlapAnimal1, " and ", input$overlapAnimal2)
      )
      axis(
        side = 1,
        at = c(0, pi/2, pi, 3*pi/2, 2*pi),
        labels = c("New Moon", "First Quarter", "Full Moon", "Last Quarter", "New Moon")
      )
      legend("top", legend = c(input$overlapAnimal1, input$overlapAnimal2), col=c("black", "blue"), lty=c(1,2))
    })
    
    #* Overlap N ----
    output$overlapN <- renderText({
      paste0(input$overlapAnimal1, ", n = ", length(
        subset(
          ind.data$Lunar,
          ind.data$Species == input$overlapAnimal1 &
            (
              ind.data$Survey.Name %in% input$overlapSurvey1a |
                ind.data$Survey.Name %in% input$overlapSurvey1b
            )
        )
      ),
      "; ", 
      input$overlapAnimal2, ", n = ", length(
        subset(
          ind.data$Lunar,
          ind.data$Species == input$overlapAnimal2 &
            (
              ind.data$Survey.Name %in% input$overlapSurvey2a |
                ind.data$Survey.Name %in% input$overlapSurvey2b
            )
        )
      )
    )})
    
    #* Overlap Watson ----
    output$overlapWatson <- renderUI({
        U2 <- watson2(
          subset(
            ind.data$Lunar,
            ind.data$Species == input$overlapAnimal1 &
              (
                ind.data$Survey.Name %in% input$overlapSurvey1a |
                  ind.data$Survey.Name %in% input$overlapSurvey1b
              )
          ),
          subset(
            ind.data$Lunar,
            ind.data$Species == input$overlapAnimal2 &
              (
                ind.data$Survey.Name %in% input$overlapSurvey2a |
                  ind.data$Survey.Name %in% input$overlapSurvey2b
              )
          )
        )
        p <- watson2test(
          subset(
            ind.data$Lunar,
            ind.data$Species == input$overlapAnimal1 &
              (
                ind.data$Survey.Name %in% input$overlapSurvey1a |
                  ind.data$Survey.Name %in% input$overlapSurvey1b
              )
          ),
          subset(
            ind.data$Lunar,
            ind.data$Species == input$overlapAnimal2 &
              (
                ind.data$Survey.Name %in% input$overlapSurvey2a |
                  ind.data$Survey.Name %in% input$overlapSurvey2b
              )
          )
        )
        div(HTML(paste0("Watson's U<sup>2</sup> statistic is ", round(U2, digits = 4), " with an estimated p-value of ", p, ".")))
      })
    
    #* Overlap CI ----
    overlapCIvalue <- eventReactive(eventExpr = input$overlapButton, valueExpr = {
      overlapCI(
        subset(
          ind.data$Lunar,
          ind.data$Species == input$overlapAnimal1 &
            (
              ind.data$Survey.Name %in% input$overlapSurvey1a |
                ind.data$Survey.Name %in% input$overlapSurvey1b
            )
        ),
        subset(
          ind.data$Lunar,
          ind.data$Species == input$overlapAnimal2 &
            (
              ind.data$Survey.Name %in% input$overlapSurvey2a |
                ind.data$Survey.Name %in% input$overlapSurvey2b
            )
        ),
        input$overlapBoot*1000
      )
    }
    )
    
    output$overlapCI <- renderUI({
      HTML(
        paste0("<b>", overlapCIvalue()["estimate"], "</b>", ", ", overlapCIvalue()["lower"], ", ", overlapCIvalue()["upper"])
      )
    })
    
  })
}
