#R Version 3.4.4
#Starshiny server script

require("shiny")
require("shinymaterial")
require("overlap")
require("suncalc")
require("lubridate")
require("proj4") 

function(input, output) {
  
  #Read uploaded CSV into raw.dat
  raw.dat <- reactive({
    if (!is.null(input$dataFile$size)) {
      read.csv(file = input$dataFile$datapath, header = TRUE, stringsAsFactors = FALSE)
    } else data.frame(NULL)
  })
  
  #Subset data into ind.dat
  ind.dat <- reactive({
    subset(raw.dat(), raw.dat()["Independent"] == "Yes" & raw.dat()["Species"] == "Agouti paca")
  })

  output$overlapPlot <- renderPlot({
    densityPlot(ind.dat()$"Time.In.Radians")
  })
  
  output$speciesSelect <- renderUI({
    material_column(
      width = 4,
      HTML(names(raw.dat()))
    )
  })
  
  # observeEvent(eventExpr = input$bootButton, handlerExpr = {
  #   dataFill <<- raw.dat()
  # }, ignoreInit = TRUE)
}
