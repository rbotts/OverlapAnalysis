#Function to calculate r-sample uniform-scores W statistic
w.stat <- function(...) {
  #' Calculates the r-sample uniform scores W statistic for r sets of circular data. See: NI Fisher "Statistical analysis of circular data". Section 5.3.6, page 122. Book. (1993)
  #' 
  #' @param ... Two or more numerical vectors of circular data (in RADIANS)
  
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
