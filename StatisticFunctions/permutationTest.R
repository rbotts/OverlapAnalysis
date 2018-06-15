permutationTest <- function(FUN, ..., trials = 10000, rightTail = TRUE) {
  #' Performs a permutation test to estimate the probability that a statistic would have a value greater than (or less than, if rightTail = FALSE) the actual value obtained.
  #' 
  #' @param FUN A function that outputs a single numeric value when given two or more data vectors.
  #' @param ... The arguments to be passed into FUN, calculating the "actual" statistical estimate.
  #' @param trials Integer. How many trials should be run? Defaults to ten thousand.
  #' @param rightTail Boolean. Should the p-value be given integrated for the right-side tail?
  
  dataList <- list(...)
  val0 <- do.call(what = FUN, args = dataList) #Statistic of actual data
  n <- unlist(lapply(dataList, length)) #n is vector of sample sizes of each input vector
  N <- sum(n) #N is the *total* sample size, how many points were recorded overall
  r <- length(n) #r is the number of vectors input
  
  #Randomization test
  valRand <- rep(x = NA, times = trials) #Preallocate result vector
  
  for (i in 1:trials) {
    
    #Preallocate dataList for this trial
    randList <- list()
    
    #Randomly resample points for each data vector from total population without replacement
    randDat <- sample(x = unlist(dataList), size = N, replace = FALSE)
    
    #Generate the resampled datasets
    for (j in 1:r) {
      if (j == 1) randList[[1]] <- randDat[1:n[j]] #The first "new" data vector has n[1] points
      else {
        nStart <- length(unlist(randList))
        randList[[j]] <- randDat[(nStart+1):(nStart+n[j])] #The j'th "new" data vector has n[j] points
      }
    }
    valRand[i] <- do.call(what = FUN, args = randList) #Once all "new" data vectors constructed, find FUN of "new" dataset and store it in the valRand vector
  }
  valSort <- sort(valRand) #Arrange results from randomization in ascending order
  p <- which.min(abs(valSort - val0)) #Figure out where val0 fits into the randomized distribution
  
  #Return the right or left p-value, depending on rightTail argument
  if (rightTail) return(c("val" = val0, "p.value" = 1-(p/trials))) else return(c("val" = val0, "p.value" = (p/trials)))
}
