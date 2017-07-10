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

#Declaring Watson functions, for easy calling later
watson2 <- function(x, y) { #Function to calculate U-squared statistic, modified to work with "tied" data (See: Zar 1999)
  n1 <- length(x)
  n2 <- length(y)
  #if (min(n1, n2) <= 17) return("Sample too small")
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

#=========================== Server function ==================================
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
    
    #Manual UI (Site)
    output$"manUI" <- renderUI({
      namelist <<- names(table(ind.data$Species))
      sitelist <<- names(table(ind.data$Site))
      seasonlist <<- names(table(ind.data$Season))
      
      div(
        fluidRow(
          column(3,
                 selectInput(inputId = "mname1", label = "Choose the first species:", choices = namelist)
          ),
          column(3, offset = 3,
            selectInput(inputId = "mname2", label = "Choose the second species:", choices = namelist) 
          )
        ),
        fluidRow(
          column(3,
                 checkboxGroupInput(inputId = "msite1", label = paste("First species's sites:"), choices = sitelist, selected = sitelist)
          ),
          column(3,
                 checkboxGroupInput(inputId = "mseason1", label = paste("First species's seasons:"), choices = seasonlist, selected = seasonlist)
          ),
          column(3,
                 checkboxGroupInput(inputId = "msite2", label = paste("Second species's sites:"), choices = sitelist, selected = sitelist)
          ),
          column(3,
                 checkboxGroupInput(inputId = "mseason2", label = paste("Second species's seasons:"), choices = seasonlist, selected = seasonlist)
          )
        )
      )
    })
    
    #Manual UI (Survey)
    output$"manUI2" <- renderUI({
      namelist <<- names(table(ind.data$Species))
      surveylist <<- names(table(ind.data$Survey.Name))
      seasonlist <<- names(table(ind.data$Season))
      
      div(
        fluidRow(
          column(3,
                 selectInput(inputId = "m2name1", label = "Choose the first species:", choices = namelist)
          ),
          column(3, offset = 3,
                 selectInput(inputId = "m2name2", label = "Choose the second species:", choices = namelist) 
          )
        ),
        fluidRow(
          column(3,
                 checkboxGroupInput(inputId = "m2survey1", label = paste("First species's surveys:"), choices = surveylist, selected = surveylist)
          ),
          column(3,
                 checkboxGroupInput(inputId = "m2season1", label = paste("First species's seasons:"), choices = seasonlist, selected = seasonlist)
          ),
          column(3,
                 checkboxGroupInput(inputId = "m2survey2", label = paste("Second species's surveys:"), choices = surveylist, selected = surveylist)
          ),
          column(3,
                 checkboxGroupInput(inputId = "m2season2", label = paste("Second species's seasons:"), choices = seasonlist, selected = seasonlist)
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
  output$"2watson" <- renderUI({
    U2 <- watson2(subset(ind.data$TimeRad,
                              ind.data$Species == input$"2name1" &
                                ind.data$Site %in% input$"2sites" &
                                ind.data$Season %in% input$"2seasons"),
                       subset(ind.data$TimeRad,
                              ind.data$Species == input$"2name2" &
                                ind.data$Site %in% input$"2sites" &
                                ind.data$Season %in% input$"2seasons"))
    p <- watson2test(subset(ind.data$TimeRad,
                            ind.data$Species == input$"2name1" &
                              ind.data$Site %in% input$"2sites" &
                              ind.data$Season %in% input$"2seasons"),
                     subset(ind.data$TimeRad,
                            ind.data$Species == input$"2name2" &
                              ind.data$Site %in% input$"2sites" &
                              ind.data$Season %in% input$"2seasons"))
    div(HTML(paste0("Watson's U<sup>2</sup> statistic is ", round(U2, digits = 4), " with an estimated p-value of ", p, ".")))
  })
  
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
  output$"1watson" <- renderUI({
    U2 <- watson2(subset(ind.data$TimeRad,
                                ind.data$Species == input$"1name1" &
                                ind.data$Site %in% input$"1site1" &
                                ind.data$Season %in% input$"1season1"),
                       subset(ind.data$TimeRad,
                                ind.data$Species == input$"1name1" &
                                ind.data$Site %in% input$"1site2" &
                                ind.data$Season %in% input$"1season2"))
    p <- watson2test(subset(ind.data$TimeRad,
                            ind.data$Species == input$"1name1" &
                              ind.data$Site %in% input$"1site1" &
                              ind.data$Season %in% input$"1season1"),
                     subset(ind.data$TimeRad,
                            ind.data$Species == input$"1name1" &
                              ind.data$Site %in% input$"1site2" &
                              ind.data$Season %in% input$"1season2"))
    div(HTML(paste0("Watson's U<sup>2</sup> statistic is ", round(U2, digits = 4), " with an estimated p-value of ", p, ".")))
  })
  
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
  
  #Manual overlap plot (Site)
  output$"movlplot" <- renderPlot({
    overlapPlot(
      subset(ind.data$TimeRad,
             ind.data$Species == input$"mname1" &
               ind.data$Site %in% input$"msite1" &
               ind.data$Season %in% input$"mseason1"),
      subset(ind.data$TimeRad,
             ind.data$Species == input$"mname2" &
               ind.data$Site %in% input$"msite2" &
               ind.data$Season %in% input$"mseason2"),
      main=paste("Overlap between", input$"mname1", "and", input$"mname2")
    )
    legend("top", legend = c(input$"mname1", input$"mname2"), col=c("black", "blue"), lty=c(1,2))
  })
  output$"movln" <- renderText({paste(
    input$"mname1", "n =", length(subset(ind.data$TimeRad,
                                         ind.data$Species == input$"mname1" &
                                           ind.data$Site %in% input$"msite1" &
                                           ind.data$Season %in% input$"mseason1")),
    ";",
    input$"mname2", "n =", length(subset(ind.data$TimeRad,
                                         ind.data$Species == input$"mname2" &
                                           ind.data$Site %in% input$"msite2" &
                                           ind.data$Season %in% input$"mseason2"))
  )})
  output$"mwatson" <- renderUI({
    U2 <- watson2(subset(ind.data$TimeRad,
                              ind.data$Species == input$"mname1" &
                                ind.data$Site %in% input$"msite1" &
                                ind.data$Season %in% input$"mseason1"),
                       subset(ind.data$TimeRad,
                              ind.data$Species == input$"mname2" &
                                ind.data$Site %in% input$"msite2" &
                                ind.data$Season %in% input$"mseason2"))
    p <- watson2test(subset(ind.data$TimeRad,
                            ind.data$Species == input$"mname1" &
                              ind.data$Site %in% input$"msite1" &
                              ind.data$Season %in% input$"mseason1"),
                     subset(ind.data$TimeRad,
                            ind.data$Species == input$"mname2" &
                              ind.data$Site %in% input$"msite2" &
                              ind.data$Season %in% input$"mseason2"))
    div(HTML(paste0("Watson's U<sup>2</sup> statistic is ", round(U2, digits = 4), " with an estimated p-value of ", p, ".")))
  })
  
  #Manual confidence interval (Site)
  CIvaluem <- eventReactive(eventExpr = input$"mbootButton", valueExpr = {
    overlapCI(
      subset(ind.data$TimeRad,
             ind.data$Species == input$"mname1" &
               ind.data$Site %in% input$"msite1" &
               ind.data$Season %in% input$"mseason1"),
      subset(ind.data$TimeRad,
             ind.data$Species == input$"mname2" &
               ind.data$Site %in% input$"msite2" &
               ind.data$Season %in% input$"mseason2"),
      input$"mn.boot"*1000
    )
  }
  )
  
  output$"mbootText" <- renderUI({
    HTML(
      paste0("<b>", CIvaluem()["estimate"], "</b>", ", ", CIvaluem()["lower"], ", ", CIvaluem()["upper"])
    )
  })
  
  #Manual overlap plot (Survey)
  output$"movlplot2" <- renderPlot({
    overlapPlot(
      subset(ind.data$TimeRad,
             ind.data$Species == input$"m2name1" &
               ind.data$Survey.Name %in% input$"m2survey1" &
               ind.data$Season %in% input$"m2season1"),
      subset(ind.data$TimeRad,
             ind.data$Species == input$"m2name2" &
               ind.data$Survey.Name %in% input$"m2survey2" &
               ind.data$Season %in% input$"m2season2"),
      main=paste("Overlap between", input$"m2name1", "and", input$"m2name2")
    )
    legend("top", legend = c(input$"m2name1", input$"m2name2"), col=c("black", "blue"), lty=c(1,2))
  })
  output$"movln2" <- renderText({paste(
    input$"m2name1", "n =", length(subset(ind.data$TimeRad,
                                          ind.data$Species == input$"m2name1" &
                                            ind.data$Survey.Name %in% input$"m2survey1" &
                                            ind.data$Season %in% input$"m2season1")),
    ";",
    input$"m2name2", "n =", length(subset(ind.data$TimeRad,
                                          ind.data$Species == input$"m2name2" &
                                            ind.data$Survey.Name %in% input$"m2survey2" &
                                            ind.data$Season %in% input$"m2season2"))
  )})
  output$"mwatson2" <- renderUI({
    U2 <- watson2(subset(ind.data$TimeRad,
                         ind.data$Species == input$"m2name1" &
                           ind.data$Survey.Name %in% input$"m2survey1" &
                           ind.data$Season %in% input$"m2season1"),
                  subset(ind.data$TimeRad,
                         ind.data$Species == input$"m2name2" &
                           ind.data$Survey.Name %in% input$"m2survey2" &
                           ind.data$Season %in% input$"m2season2"))
    p <- watson2test(subset(ind.data$TimeRad,
                            ind.data$Species == input$"m2name1" &
                              ind.data$Survey.Name %in% input$"m2survey1" &
                              ind.data$Season %in% input$"m2season1"),
                     subset(ind.data$TimeRad,
                            ind.data$Species == input$"m2name2" &
                              ind.data$Survey.Name %in% input$"m2survey2" &
                              ind.data$Season %in% input$"m2season2"))
    div(HTML(paste0("Watson's U<sup>2</sup> statistic is ", round(U2, digits = 4), " with an estimated p-value of ", p, ".")))
  })
  
  #Manual confidence interval (Survey)
  CIvaluem2 <- eventReactive(eventExpr = input$"mbootButton2", valueExpr = {
    overlapCI(
      subset(ind.data$TimeRad,
             ind.data$Species == input$"m2name1" &
               ind.data$Survey.Name %in% input$"m2survey1" &
               ind.data$Season %in% input$"m2season1"),
      subset(ind.data$TimeRad,
             ind.data$Species == input$"m2name2" &
               ind.data$Survey.Name %in% input$"m2survey2" &
               ind.data$Season %in% input$"m2season2"),
      input$"mn.boot2"*1000
    )
  }
  )
  
  output$"mbootText2" <- renderUI({
    HTML(
      paste0("<b>", CIvaluem2()["estimate"], "</b>", ", ", CIvaluem2()["lower"], ", ", CIvaluem2()["upper"])
    )
  })
}
