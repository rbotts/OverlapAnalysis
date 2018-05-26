##Shiny server script
require(shiny)
require(overlap)
size.fileupload <- 64 #Max file size that can be uploaded is this many MB
options(shiny.maxRequestSize = size.fileupload*1024^2)

#Utility functions ----
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

#Declaring W statistic functions, for easy calling later
w.stat <- function(...) {
  #Function to calculate W statistic from 2 or more vectors of data (in RADIANS)
  
  #Data input and organization
  dataList <- list(...)
  unData <- data.frame(Point = sort(unlist(dataList)))
  n <- unlist(lapply(dataList, length)) #n is vector of sample sizes of each input vector
  N <- sum(n) #N is the *total* sample size, how many points were recorded overall
  r <- length(n) #r is the number of vectors input
  
  #Linear Rank ----
  unData["Rank"] <- 1:N
  for (i in 1:N) {
    whenSame <- unData$Point == unData$Point[i] #Create logical vector of which points of unData$Point are the same as a given point
    numSame <- sum(whenSame) #How many times does this value appear?
    
    if (numSame > 1) { #If a value appears more than once...
      unData$Rank[whenSame] <- rep(mean(unData$Rank[whenSame]), times = numSame) #...every point with that value should take the average Rank
    }
  }
  
  unData["Uniform"] <- 2 * pi * unData$Rank / N #Circular Rank, a.k.a: "Uniform Score"
  
  #Calculating W ----
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

w.prob <- function(..., trials = 10000, randomize = FALSE) {
  #Function to calculate p-value of the W statistic of given 2+ vectors of data (in RADIANS)
  
  #Data input ----
  dataList <- list(...)
  W0 <- do.call(what = w.stat, args = dataList) #W statistic of actual data
  n <- unlist(lapply(dataList, length)) #n is vector of sample sizes of each input vector
  N <- sum(n) #N is the *total* sample size, how many points were recorded overall
  r <- length(n) #r is the number of vectors input
  
  if(randomize | min(n) < 10) {
    #Randomization test ----
    randW <- rep(x = NA, times = trials) #Preallocate result vector
    
    for (i in 1:trials) {
      randList <- list()
      randDat <- sample(x = unlist(dataList), size = N, replace = FALSE) #Randomly resample points for each data vector from total population without replacement
      for (j in 1:r) {
        if (j == 1) randList[[1]] <- randDat[1:n[j]] #The first "new" data vector has n[1] points
        else {
          nStart <- length(unlist(randList))
          randList[[j]] <- randDat[(nStart+1):(nStart+n[j])] #The j'th "new" data vector has n[j] points
        }
      }
      randW[i] <- do.call(what = w.stat, args = randList) #Once all "new" data vectors constructed, calculate W of "new" dataset and store it in the randW vector
    }
    sortW <- sort(randW) #Arrange W statistics from randomization in ascending order
    p <- which.min(abs(sortW-W0)) #Figure out where W0 fits into the randomized distribution
    return(c("W" = W0, "p-value" = 1-(p/trials))) #p-value = the fraction of trials that gave a W greater than W0
  }
  else {
    return(c("W" = W0, "p-value" = pchisq(q = W0, df = 2*r - 2, lower.tail = FALSE))) #Approximate with Chi-square
  }
}


#Server Function ----
function(input, output) {
  #Generate dataset ----
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
    
    #Two species UI ----
    output$"2speciesUI" <- renderUI({
      namelist <<- names(table(ind.data$Species))
      sitelist <<- names(table(ind.data$Site))
      seasonlist <<- names(table(ind.data$Season))
      
      div(
        selectInput(inputId = "2name1",
                    label = "Choose the first species:",
                    choices = namelist),
        selectInput(inputId = "2name2",
                    label = "Choose the second species:",
                    choices = namelist),
        checkboxGroupInput(
          inputId = "2sites",
          label = "Filter by site(s)",
          choices = sitelist,
          selected = sitelist
        ),
        checkboxGroupInput(
          inputId = "2seasons",
          label = "Filter by season(s)",
          choices = seasonlist,
          selected = seasonlist
        )
      )
    })
    
    #One species UI ----
    output$"1speciesUI" <- renderUI({
      namelist <<- names(table(ind.data$Species))
      sitelist <<- names(table(ind.data$Site))
      seasonlist <<- names(table(ind.data$Season))
      
      div(fluidRow(column(3,
        selectInput(
          inputId = "1name1",
          label = "Choose the species of interest:",
          choices = namelist
        )
      )),
      fluidRow(
        column(3,
          checkboxGroupInput(
            inputId = "1site1",
            label = "Group A's sites:",
            choices = sitelist,
            selected = sitelist
          )
        ),
        column(3,
          checkboxGroupInput(
            inputId = "1season1",
            label = "Group A's seasons:",
            choices = seasonlist,
            selected = seasonlist
          )
        ),
        column(3,
          checkboxGroupInput(
            inputId = "1site2",
            label = "Group B's sites:",
            choices = sitelist,
            selected = sitelist
          )
        ),
        column(3,
          checkboxGroupInput(
            inputId = "1season2",
            label = "Group B's seasons:",
            choices = seasonlist,
            selected = seasonlist
          )
        )
      ))
    })
    
    #Manual UI (Site) ----
    output$"manUI" <- renderUI({
      namelist <<- names(table(ind.data$Species))
      sitelist <<- names(table(ind.data$Site))
      seasonlist <<- names(table(ind.data$Season))
      
      div(
        fluidRow(
          column(3,
            selectInput(
              inputId = "mname1",
              label = "Choose the first species:",
              choices = namelist
            )
          ),
          column(3,
            offset = 3,
            selectInput(
              inputId = "mname2",
              label = "Choose the second species:",
              choices = namelist
            )
          )
        ),
        fluidRow(
          column(3,
            checkboxGroupInput(
              inputId = "msite1",
              label = paste("First species's sites:"),
              choices = sitelist,
              selected = sitelist
            )
          ),
          column(3,
            checkboxGroupInput(
              inputId = "mseason1",
              label = paste("First species's seasons:"),
              choices = seasonlist,
              selected = seasonlist
            )
          ),
          column(3,
            checkboxGroupInput(
              inputId = "msite2",
              label = paste("Second species's sites:"),
              choices = sitelist,
              selected = sitelist
            )
          ),
          column(3,
            checkboxGroupInput(
              inputId = "mseason2",
              label = paste("Second species's seasons:"),
              choices = seasonlist,
              selected = seasonlist
            )
          )
        )
      )
    })
    
    #Manual UI (Survey) ----
    output$"manUI2" <- renderUI({
      namelist <<- names(table(ind.data$Species))
      surveylist <<- names(table(ind.data$Survey.Name))
      seasonlist <<- names(table(ind.data$Season))
      
      div(fluidRow(column(3,
        selectInput(
          inputId = "m2name1",
          label = "Choose the first species:",
          choices = namelist
        )
      ),
      column(3,
        offset = 3,
        selectInput(
          inputId = "m2name2",
          label = "Choose the second species:",
          choices = namelist
        )
      )),
      fluidRow(
        column(3,
          checkboxGroupInput(
            inputId = "m2survey1",
            label = paste("First species's surveys:"),
            choices = surveylist,
            selected = surveylist
          )
        ),
        column(3,
          checkboxGroupInput(
            inputId = "m2season1",
            label = paste("First species's seasons:"),
            choices = seasonlist,
            selected = seasonlist
          )
        ),
        column(3,
          checkboxGroupInput(
            inputId = "m2survey2",
            label = paste("Second species's surveys:"),
            choices = surveylist,
            selected = surveylist
          )
        ),
        column(3,
          checkboxGroupInput(
            inputId = "m2season2",
            label = paste("Second species's seasons:"),
            choices = seasonlist,
            selected = seasonlist
          )
        )
      ))
    })
    
  })
  
  #Two species plot ----
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
  output$"2W" <- renderUI({
    W <- w.prob(subset(ind.data$TimeRad,
                            ind.data$Species == input$"2name1" &
                              ind.data$Site %in% input$"2sites" &
                              ind.data$Season %in% input$"2seasons"),
                     subset(ind.data$TimeRad,
                            ind.data$Species == input$"2name2" &
                              ind.data$Site %in% input$"2sites" &
                              ind.data$Season %in% input$"2seasons"),
                trials = 1000)
    U2 <- W[1]
    p <- W[2]
    div(HTML(paste0("Uniform scores test W statistic is ", round(U2, digits = 4), " with an estimated p-value of ", p, ".")))
  })
  
  #Two species CI ----
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
  
  #Single species plot ----
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
  output$"1W" <- renderUI({
    W <- w.prob(subset(ind.data$TimeRad,
                            ind.data$Species == input$"1name1" &
                              ind.data$Site %in% input$"1site1" &
                              ind.data$Season %in% input$"1season1"),
                     subset(ind.data$TimeRad,
                            ind.data$Species == input$"1name1" &
                              ind.data$Site %in% input$"1site2" &
                              ind.data$Season %in% input$"1season2"),
                trials = 1000)
    U2 <- W[1]
    p <- W[2]
    div(HTML(paste0("Uniform scores test W statistic is ", round(U2, digits = 4), " with an estimated p-value of ", p, ".")))
  })
  
  #Single species CI ----
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
  
  #Manual plot (Site) ----
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
  output$"mW" <- renderUI({
    W <- w.prob(subset(ind.data$TimeRad,
                            ind.data$Species == input$"mname1" &
                              ind.data$Site %in% input$"msite1" &
                              ind.data$Season %in% input$"mseason1"),
                     subset(ind.data$TimeRad,
                            ind.data$Species == input$"mname2" &
                              ind.data$Site %in% input$"msite2" &
                              ind.data$Season %in% input$"mseason2"),
                trials = 1000)
    U2 <- W[1]
    p <- W[2]
    div(HTML(paste0("Uniform scores test W statistic is ", round(U2, digits = 4), " with an estimated p-value of ", p, ".")))
  })
  
  #Manual CI (Site) ----
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
  
  #Manual plot (Survey) ----
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
  output$"mW2" <- renderUI({
    W <- w.prob(subset(ind.data$TimeRad,
                            ind.data$Species == input$"m2name1" &
                              ind.data$Survey.Name %in% input$"m2survey1" &
                              ind.data$Season %in% input$"m2season1"),
                     subset(ind.data$TimeRad,
                            ind.data$Species == input$"m2name2" &
                              ind.data$Survey.Name %in% input$"m2survey2" &
                              ind.data$Season %in% input$"m2season2"),
                trials = 1000)
    U2 <- W[1]
    p <- W[2]
    div(HTML(paste0("Uniform scores test W statistic is ", round(U2, digits = 4), " with an estimated p-value of ", p, ".")))
  })
  
  #Manual CI (Survey) ----
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
