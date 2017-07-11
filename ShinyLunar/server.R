#Shiny app for lunar analysis
require(shiny)
require(overlap)
require(suncalc)

#Declaring some functions for use later:
#Watson
#Rao

#=========================== Server function ==================================
function(input, output) {
  #Generate dataset from uploaded file
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
    
    #Grouping for our data set; hopefully no users call their data file "MooringActivityRawData2.csv"
    if (tail((input$updata)$name, n=1) == "MooringActivityRawData2.csv") {
      ind.data["Site"] <- gsub("\\s*York Univ\\s*", "ASBC", ind.data$Site)
      ind.data["Site"] <- gsub("\\s*Copal\\s*|\\s*Marta\\s*", "Pejibaye", ind.data$Site)
      ind.data["Site"] <- gsub("\\s*PN Carara\\s*|\\s*Carara\\s*", "PNC", ind.data$Site)
      ind.data["Site"] <- gsub("\\s*Tapanti\\s*|\\s*Via Mills\\s*|\\s*Villa Mills\\s*", "PNT", ind.data$Site)
    }
    
    #Pattern Analysis UI
    output$patternSelect <- renderUI({
      namelist <<- names(table(ind.data$Species))
      wellPanel(
        selectInput(
          label = "Choose the species to analyze:",
          inputId = "patternAnimal",
          choices = namelist
        )
      )
    })
    
    output$patternGraph <- renderPlot({
      densityPlot(
        subset(ind.data$Lunar, ind.data$Species == input$patternAnimal),
        xscale = NA,
        xlab = "Lunar Phase",
        xaxt = "n",
        main = input$patternAnimal
      )
      rug(
        subset(ind.data$Lunar, ind.data$Species == input$patternAnimal),
        side = 1
      )
      axis(
        side = 1,
        at = c(0, pi/2, pi, 3*pi/2, 2*pi),
        labels = c("New Moon", "First Quarter", "Full Moon", "Last Quarter", "New Moon")
      )
    })
    
    output$patternN <- renderText({
      paste0("n = ", length(subset(ind.data$Lunar, ind.data$Species == input$patternAnimal)))
    })
    
    output$patternRao <- renderUI({
      U <- rao.space.U(subset(ind.data$Lunar, ind.data$Species == input$patternAnimal))
      p <- rao.space.test(subset(ind.data$Lunar, ind.data$Species == input$patternAnimal))
      div(HTML(paste0("Rao's spacing U statistic is ", round(U, digits = 4), "degrees, with a p-value less than ", p, ".")))
    })
  })
}
