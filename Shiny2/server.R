##Shiny server script
require(shiny)
require(overlap)

#Declaring confidence interval function, for easy calling later.
overlapCI <- function(animal1, animal2, n.boot) {
  ovl <- c()
  if (min(length(animal1), length(animal2)) <= 75 & min(length(animal1), length(animal2)) > 15) {
    ovl["estimate"] <- overlapEst(animal1, animal2, adjust=c(0.8, NA, NA))[1]
    boot1 <- resample(animal1, n.boot)
    boot2 <- resample(animal2, n.boot)
    ovl.boot <- bootEst(boot1, boot2, adjust=c(0.8, NA, NA))[,1]
    ovl.boot.ci <- bootCI(ovl["estimate"], ovl.boot)
    ovl["estimate"] <- sprintf("%1.4f", ovl["estimate"])
    ovl["lower"] <- sprintf("%1.4f", ovl.boot.ci[4,1])
    ovl["upper"] <- sprintf("%1.4f", ovl.boot.ci[4,2])
    
  } else if (min(length(animal1), length(animal2)) > 75) {
    ovl["estimate"] <- overlapEst(animal1, animal2, adjust=c(NA, 1, NA))[2]
    boot1 <- resample(animal1, n.boot)
    boot2 <- resample(animal2, n.boot)
    ovl.boot <- bootEst(boot1, boot2, adjust=c(NA, 1, NA))[,2]
    ovl.boot.ci <<- bootCI(ovl["estimate"], ovl.boot)
    ovl["estimate"] <- sprintf("%1.4f", ovl["estimate"])
    ovl["lower"] <- sprintf("%1.4f", ovl.boot.ci[4,1])
    ovl["upper"] <- sprintf("%1.4f", ovl.boot.ci[4,2])
  } else ovl["estimate"] <- "Sample too small"
  
  return(ovl)
}

#Server function
function(input, output) {
  #Generate dataset from uploaded file
  observeEvent(eventExpr = input$dataButton, handlerExpr = {
    ind.data <<- read.table(file = tail((input$updata)$datapath, n=1), header=TRUE, sep=",", stringsAsFactors=FALSE)
    ind.data <<- subset(ind.data, ind.data["Independent"] ==  "Yes")
    ind.data <<- subset(ind.data, ind.data$Species != "Unknown")
    ind.data <<- subset(ind.data, ind.data$Species != "unknown")
    ind.data <<- subset(ind.data, ind.data$Species != "")
    max.time <- max(ind.data$Time)
    if (max.time > 12 & max.time <= 24) {
      ind.data["TimeRad"] <<- ind.data$Time*2*pi/24
    } else if (max.time <= 1) {
      ind.data["TimeRad"] <<- ind.data$Time*2*pi
    } else print("Unknown time format.")
    ind.data["Site"] <<- gsub("\\s*20\\d\\d\\s*|\\s*Spring\\s*|\\s*Summer\\s*|\\s*Fall\\s*|\\s*El\\s*|\\s*La\\s*|\\s*National Park\\s*", "", ind.data$Survey.Name)
    ind.data["Season"] <<- ifelse(grepl("Spring", ind.data$Survey.Name), "Spring", ifelse(grepl("Summer", ind.data$Survey.Name), "Summer", ifelse(grepl("Fall", ind.data$Survey.Name), "Fall", "Other")))
    
    #Grouping for our data set; hopefully no users call their data file "MooringActivityRawData2.csv"
    if (tail((input$updata)$name, n=1) == "MooringActivityRawData2.csv") {
      ind.data["Site"] <- gsub("\\s*York Univ\\s*", "ASBC", ind.data$Site)
      ind.data["Site"] <- gsub("\\s*Copal\\s*|\\s*Marta\\s*", "Pejibaye", ind.data$Site)
      ind.data["Site"] <- gsub("\\s*PN Carara\\s*|\\s*Carara\\s*", "PNC", ind.data$Site)
      ind.data["Site"] <- gsub("\\s*Tapanti\\s*|\\s*Via Mills\\s*|\\s*Villa Mills\\s*", "PNT", ind.data$Site)
    }
    
    #Two species UI
    output$"2speciesUI" <- renderUI({
      namelist <<- names(table(ind.data$Species))
      sitelist <<- names(table(ind.data$Site))
      seasonlist <<- names(table(ind.data$Season))
      
      div(
        selectInput(inputId = "2name1", label="Choose the first species:", choices=namelist),
        selectInput(inputId = "2name2", label="Choose the second species:", choices=namelist),
        checkboxGroupInput(inputId = "2sites", label="Filter by site(s)", choices=sitelist, selected=sitelist),
        checkboxGroupInput(inputId = "2seasons", label="Filter by season(s)", choices=seasonlist, selected=seasonlist)
      )
    })
    
    #One species UI
    output$"1speciesUI" <- renderUI({
      namelist <<- names(table(ind.data$Species))
      sitelist <<- names(table(ind.data$Site))
      seasonlist <<- names(table(ind.data$Season))
      
      div(
        fluidRow(
          column(3,
                 selectInput(inputId = "1name1", label = "Choose the species of interest:", choices = namelist)
          )
        ),
        fluidRow(
          column(3,
                 checkboxGroupInput(inputId = "1site1", label = "Group A's sites:", choices = sitelist, selected = sitelist)
          ),
          column(3,
                 checkboxGroupInput(inputId = "1season1", label = "Group A's seasons:", choices = seasonlist, selected = seasonlist)
          ),
          column(3,
                 checkboxGroupInput(inputId = "1site2", label = "Group B's sites:", choices = sitelist, selected = sitelist)
          ),
          column(3,
                 checkboxGroupInput(inputId = "1season2", label = "Group B's seasons:", choices = seasonlist, selected = seasonlist)
          )
        )
      )
    })
  })
  
  #Two species plot
  output$"2ovlplot" <- renderPlot({
    overlapPlot(
      subset(ind.data$TimeRad,
             ind.data$Species == input$"2name1" &
               ind.data$Site %in% input$"2sites" &
               ind.data$Season %in% input$"2seasons"),
      subset(ind.data$TimeRad,
             ind.data$Species == input$"2name2" &
               ind.data$Site %in% input$"2sites" &
               ind.data$Season %in% input$"2seasons"),
      main=paste("Overlap between", input$"2name1", "and", input$"2name2")
      )
    legend("top", legend = c(input$"2name1", input$"2name2"), col=c("black", "blue"), lty=c(1,2))
  })
  output$"2ovln" <- renderText({paste(
    input$"2name1", "n =", length(subset(ind.data$TimeRad,
                                         ind.data$Species == input$"2name1" &
                                           ind.data$Site %in% input$"2sites" &
                                           ind.data$Season %in% input$"2seasons")),
    ";",
    input$"2name2", "n =", length(subset(ind.data$TimeRad,
                                              ind.data$Species == input$"2name2" &
                                                ind.data$Site %in% input$"2sites" &
                                                ind.data$Season %in% input$"2seasons"))
  )})
  
  #Two species confidence interval
  CIvalue2 <- eventReactive(eventExpr = input$"2bootButton", valueExpr = {
    overlapCI(
      subset(ind.data$TimeRad,
             ind.data$Species == input$"2name1" &
               ind.data$Site %in% input$"2sites" &
               ind.data$Season %in% input$"2seasons"),
      subset(ind.data$TimeRad,
             ind.data$Species == input$"2name2" &
               ind.data$Site %in% input$"2sites" &
               ind.data$Season %in% input$"2seasons"),
      input$"2n.boot"*1000
      )
    }
  )
  
  output$"2bootText" <- renderUI({
    HTML(
      paste0("<b>", CIvalue2()["estimate"], "</b>", ", ", CIvalue2()["lower"], ", ", CIvalue2()["upper"])
    )
  })
  
  #Single species plot
  output$"1ovlplot" <- renderPlot({
    overlapPlot(
      subset(ind.data$TimeRad,
             ind.data$Species == input$"1name1" &
               ind.data$Site %in% input$"1site1" &
               ind.data$Season %in% input$"1season1"),
      subset(ind.data$TimeRad,
             ind.data$Species == input$"1name1" &
               ind.data$Site %in% input$"1site2" &
               ind.data$Season %in% input$"1season2"),
      main="Overlap between Group A and Group B"
    )
    legend("top", legend = c("Group A", "Group B"), col=c("black", "blue"), lty=c(1,2))
  })
  output$"1ovln" <- renderText({paste(
    "Group A n =", length(subset(ind.data$TimeRad,
                                 ind.data$Species == input$"1name1" &
                                   ind.data$Site %in% input$"1site1" &
                                   ind.data$Season %in% input$"1season1")),
    "; Group B n =", length(subset(ind.data$TimeRad,
                                 ind.data$Species == input$"1name1" &
                                   ind.data$Site %in% input$"1site2" &
                                   ind.data$Season %in% input$"1season2"))
  )})
  
  #Single species confidence interval
  CIvalue1 <- eventReactive(eventExpr = input$"1bootButton", valueExpr = {
    overlapCI(
      subset(ind.data$TimeRad,
             ind.data$Species == input$"1name1" &
               ind.data$Site %in% input$"1site1" &
               ind.data$Season %in% input$"1season1"),
      subset(ind.data$TimeRad,
             ind.data$Species == input$"1name1" &
               ind.data$Site %in% input$"1site2" &
               ind.data$Season %in% input$"1season2"),
      input$"1n.boot"*1000
    )
  }
  )
  
  output$"1bootText" <- renderUI({
    HTML(
        paste0("<b>", CIvalue1()["estimate"], "</b>", ", ", CIvalue1()["lower"], ", ", CIvalue1()["upper"])
    )
  })
}
