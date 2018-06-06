#R version 3.4.4
#Statistical Tests for Homogeneity on a Circle
#See: NI Fisher (1993) "Statistical analysis of circular data". Section 5.3.6, page 122. Book.
#See also: KV Mardia (1972) "A Multi-Sample Uniform Scores Test on a Circle and Its Parametric Competitor". Journal of the Royal Statistical Society.


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
  
  #Data input and organization
  dataList <- list(...)
  W0 <- do.call(what = w.stat, args = dataList) #W statistic of actual data
  n <- unlist(lapply(dataList, length)) #n is vector of sample sizes of each input vector
  N <- sum(n) #N is the *total* sample size, how many points were recorded overall
  r <- length(n) #r is the number of vectors input
  
  if(randomize | min(n) < 10) {
    #Randomization test
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

#Sample Dataset, found in Appendix B14 of Fisher 1993.
# Spring <- c(0,  20,  40,  60, 160, 170,  200, 220, 270, 290, 340, 350)
# Autumn <- c(30, 70,  110, 170, 180, 190, 240, 250, 260, 260, 290, 350)
# Winter <- c(50, 120, 190, 210, 220, 250, 260, 290, 290, 320, 320, 340)

#w.prob(Winter, Spring, Autumn, randomize = TRUE, trials = 1000000) -> 0.452103
#w.prob(Winter, Spring, Autumn) -> 0.4280753
#Chisquare has ~5% error in p-value at around p=0.45
