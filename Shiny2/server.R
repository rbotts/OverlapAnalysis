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

#Following function based on S-plus code licensed under GPLv2+ by Ulric Lund at http://statweb.calpoly.edu/lund/
#Modified by TJ Wiegman in June 2017, with the following changes:
#1 - Removed all cat() statements
#2 - Now returns values rather than printing to console
#3 - Changed default alpha value from 0 to 0.05
watson2 <- function(x, y, alpha = 0.05, plot = FALSE) {
  n1 <- length(x)
  n2 <- length(y)
  n <- n1 + n2
  if(n < 18) {
    return("Total Sample Size < 18:  Consult tabulated critical values")
  }
  if(plot == TRUE) {
    x <- sort(x %% (2 * pi))
    y <- sort(y %% (2 * pi))
    plot.edf(x, main = "Comparison of Empirical CDFs", xlab= "", ylab = "")
    par(new = TRUE)
    plot.edf(y, xlab = "", ylab = "", axes = FALSE, lty = 2)
  }
  x <- cbind(sort(x %% (2 * pi)), rep(1, n1))
  y <- cbind(sort(y %% (2 * pi)), rep(2, n2))
  xx <- rbind(x, y)
  rank <- order(xx[, 1])
  xx <- cbind(xx[rank,  ], seq(1:n))
  a <- c(1:n)
  b <- c(1:n)
  for(i in 1:n) {
    a[i] <- sum(xx[1:i, 2] == 1)
    b[i] <- sum(xx[1:i, 2] == 2)
  }
  d <- b/n2 - a/n1
  dbar <- mean(d)
  u2 <- (n1 * n2)/n^2 * sum((d - dbar)^2)
  crits <- c(99, 0.385, 0.268, 0.187, 0.152)
  if(sum(alpha == c(0, 0.001, 0.01, 0.05, 0.1)) == 0)
    stop("Invalid input for alpha")
  else if(alpha == 0) {
    if(u2 > 0.385)
      return("P-value < 0.001")
    else if(u2 > 0.268)
      return("0.001 < P-value < 0.01")
    else if(u2 > 0.187)
      return("0.01 < P-value < 0.05")
    else if(u2 > 0.152)
      return("0.05 < P-value < 0.10")
    else return("P-value > 0.10")
  }
  else {
    index <- (1:5)[alpha == c(0, 0.001, 0.01, 0.05, 0.1)]
    Critical <- crits[index]
    if (u2 > Critical) Reject <- TRUE #Reject the null hypothesis; x and y are significantly different
    else Reject <- FALSE #Do not reject the null hypothesis; x and y are not significantly different
    
    return(list("Reject"=Reject, "U2"=u2, "Alpha"=alpha, "CriticalValue"=Critical))
  }
}
#End of code based on work of Ulric Lund

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
    
    #Manual UI
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
    wresult <- watson2(subset(ind.data$TimeRad,
                              ind.data$Species == input$"2name1" &
                                ind.data$Site %in% input$"2sites" &
                                ind.data$Season %in% input$"2seasons"),
                       subset(ind.data$TimeRad,
                              ind.data$Species == input$"2name2" &
                                ind.data$Site %in% input$"2sites" &
                                ind.data$Season %in% input$"2seasons"))
    if (wresult$Reject == TRUE) {
      div(HTML(
        paste0("Reject the null hypothesis. ", input$"2name1", " and ", input$"2name2", " have <b>significantly different</b> activity patterns. U<sup>2</sup> is equal to ", round(wresult$U2, digits = 4), ".")
      ))
    } else if (wresult$Reject == FALSE) {
      div(HTML(
        paste0("Do not reject the null hypothesis. ", input$"2name1", " and ", input$"2name2", " do <b>not</b> have significantly different activity patterns. U<sup>2</sup> is equal to ", round(wresult$U2, digits = 4), ".")
      ))
    }
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
    wresult <- watson2(subset(ind.data$TimeRad,
                                ind.data$Species == input$"1name1" &
                                ind.data$Site %in% input$"1site1" &
                                ind.data$Season %in% input$"1season1"),
                       subset(ind.data$TimeRad,
                                ind.data$Species == input$"1name1" &
                                ind.data$Site %in% input$"1site2" &
                                ind.data$Season %in% input$"1season2"))
    if (wresult$Reject == TRUE) {
      div(HTML(
        paste0("Reject the null hypothesis. Group A and Group B have <b>significantly different</b> activity patterns. U<sup>2</sup> is equal to ", round(wresult$U2, digits = 4), ".")
      ))
    } else if (wresult$Reject == FALSE) {
      div(HTML(
        paste0("Do not reject the null hypothesis. Group A and Group B do <b>not</b> have significantly different activity patterns. U<sup>2</sup> is equal to ", round(wresult$U2, digits = 4), ".")
      ))
    }
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
  
  #Manual overlap plot
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
    wresult <- watson2(subset(ind.data$TimeRad,
                              ind.data$Species == input$"mname1" &
                                ind.data$Site %in% input$"msite1" &
                                ind.data$Season %in% input$"mseason1"),
                       subset(ind.data$TimeRad,
                              ind.data$Species == input$"mname2" &
                                ind.data$Site %in% input$"msite2" &
                                ind.data$Season %in% input$"mseason2"))
    if (wresult$Reject == TRUE) {
      div(HTML(
        paste0("Reject the null hypothesis. ", input$"mname1", " and ", input$"mname2", " have <b>significantly different</b> activity patterns. U<sup>2</sup> is equal to ", round(wresult$U2, digits = 4), ".")
      ))
    } else if (wresult$Reject == FALSE) {
      div(HTML(
        paste0("Do not reject the null hypothesis. ", input$"mname1", " and ", input$"mname2", " do <b>not</b> have significantly different activity patterns. U<sup>2</sup> is equal to ", round(wresult$U2, digits = 4), ".")
      ))
    }
  })
  
  #Manual confidence interval
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
}
