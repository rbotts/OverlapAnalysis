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
  #Function to calculation p-value of the W statistic of given 2+ vectors of data (in RADIANS)
  
  #Data input and organization
  dataList <- list(...)
  W0 <- do.call(what = w.stat, args = dataList)
  n <- unlist(lapply(dataList, length)) #n is vector of sample sizes of each input vector
  N <- sum(n) #N is the *total* sample size, how many points were recorded overall
  r <- length(n) #r is the number of vectors input
  
  if(randomize | min(n) < 10) {
    #Randomization test
    randW <- rep(x = NA, times = trials)
    
    for (i in 1:trials) {
      randList <- list()
      randDat <- sample(x = unlist(dataList), size = N, replace = FALSE)
      for (j in 1:r) {
        if (j == 1) randList[[1]] <- randDat[1:n[j]]
        else {
          nStart <- length(unlist(randList))
          randList[[j]] <- randDat[(nStart+1):(nStart+n[j])]
        }
      }
      randW[i] <- do.call(what = w.stat, args = randList)
    }
    sortW <- sort(randW)
    p <- which.min(abs(sortW-W0))
    return(1-(p/trials))
  }
  else {
    return(pchisq(q = W0, df = 2*r - 2, lower.tail = FALSE))
  }
}

Spring <- c(0,  20,  40,  60, 160, 170,  200, 220, 270, 290, 340, 350)
Summer <- c(10, 10,  20,  20,  30,  30,   40, 150, 150, 150, 170, 190, 290)
Autumn <- c(30, 70,  110, 170, 180, 190, 240, 250, 260, 260, 290, 350)
Winter <- c(50, 120, 190, 210, 220, 250, 260, 290, 290, 320, 320, 340)

print(w.prob(Winter, Spring, Autumn, randomize = TRUE))
