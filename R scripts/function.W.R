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
  
  #Linear Rank ----
  unData["Rank"] <- 1:sum(n)
  for (i in 1:length(unData$Point)) {
    whenSame <- unData$Point == unData$Point[i] #Create logical vector of which points of unData$Point are the same as a given point
    numSame <- sum(whenSame) #How many times does this value appear?
    if (numSame > 1) { #If a value appears more than once...
      unData$Rank[whenSame] <- rep(mean(unData$Rank[whenSame]), times = numSame) #Every point should take the average Rank
    }
  }
  
  unData["Uniform"] <- 2 * pi * unData$Rank / sum(n) #Circular Rank, a.k.a: "Uniform Score"
  
  C <- S <- rep(0, times = length(n)) #Preallocate values
  for (i in 1:length(n)) {
    for (j in 1:n[i]) {
      rankOrder <- unData$Uniform[unData$Point == dataList[[i]][j]]
      C[i] <- C[i] + cos(rankOrder)
      S[i] <- S[i] + sin(rankOrder)
    }
  }
  
  W <- 2 * sum((C^2 + S^2)/n)
  unData$W <- rep(W)
  
  return(unData)
}

Spring <- c(0,  20,  40,  60, 160, 170,  200, 220, 270, 290, 340, 350)
Summer <- c(10, 10,  20,  20,  30,  30,   40, 150, 150, 150, 170, 190, 290)
Autumn <- c(30, 70,  110, 170, 180, 190, 240, 250, 260, 260, 290, 350)
Winter <- c(50, 120, 190, 210, 220, 250, 260, 290, 290, 320, 320, 340)

#print(w.stat(Winter, Spring, Autumn))
