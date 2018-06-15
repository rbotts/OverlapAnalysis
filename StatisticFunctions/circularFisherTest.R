#Function to calculate Chi-Squared
circularFisherTest <- function(animal1, animal2, bins = 12) {
  #' Performs a Fisher's Exact Test on two circular data sets to analyze similarity.
  #' 
  #' @param animal1 A numerical vector of measured points for the first species in RADIANS.
  #' @param animal2 A numerical vector of measured points for the second species in RADIANS.
  #' @param bins Into how many bins should the data be divided before analysis? Defaults to 12.
  
  binning <- function(x, bins) {
    #' Break a circular data set into equally-spaced bins \emph{(e.g: for chi-square analysis)}.
    #' 
    #' @param x A numerical vector of measured points in RADIANS
    #' @param bins Integer. How many bins should `x` be divided into?
    
    #Create boundaries for the bins
    cutoffs <- seq(from = 0,
                   to = 2 * pi,
                   length.out = bins + 1)
    n <- length(cutoffs)
    
    #Preallocate the output
    output <- rep(NA, times = length(x))
    
    for (i in cutoffs[-n]) {
      output <- ifelse(test = x >= i, #If x is greater than or equal to cutoff[i]...
                       yes = which(i == cutoffs), #Then output should be i
                       no = output) #Else it should stay what it is)
    }
    
    return(output)
  }
  
  animal1bin <- binning(x = animal1, bins = bins)
  animal1bin <- setNames(tabulate(animal1bin), 1:max(animal1bin))
  animal2bin <- binning(x = animal2, bins = bins)
  animal2bin <- setNames(tabulate(animal2bin), 1:max(animal2bin))
  animalbins <- rbind(animal1bin, animal2bin)
  size = 2 * bins
  
  chistat <- fisher.test(animalbins, simulate.p.value = TRUE, B = 100000)
  
  return(chistat["p.value"])
}
