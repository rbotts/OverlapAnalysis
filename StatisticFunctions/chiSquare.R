 #chi squared and plot

#Overlap Plot Function
plot <- function(animalname1,animalname2) {
  animal1<<-subset(ind.data$Time.In.Radians,
                   ind.data$Species == animalname1) 
  
  animal2<<-subset(ind.data$Time.In.Radians,
                   ind.data$Species == animalname2)
  overlapPlot(
    animal1, animal2,
    main=paste0("Activity overlap between ", animalname1, " and ", animalname2),
    rug=TRUE
  )
  legend("top", legend = c(animalname1, animalname2), col=c("black", "blue"), lty=c(1,2)
  )
}

#Functions to bin data 
binning <- function(x, bins = 12) {
  #x is a vector of times in radians
  #bins is the number of bins to put the data in
  
  cutoffs <- seq(from = 0, to = 2*pi, length.out = bins+1) #creates cutoffs for bins (time is in radians)
  n <- length(cutoffs)
  output <- rep(NA, times = length(x))   #puts all values to NA
  
  for (i in cutoffs[-n]) {
    output <- ifelse(
      test = x >= i,               #If x is greater than cutoff[n]...
      yes = which(i == cutoffs),   #Then output should be n
      no = output                  #Else it should stay what it is
    )
  }
  
  return(output)
}

#Function to calculate Chi-Squared
chisquared <- function(animal1, animal2, bins=12) {
  animal1bin <- binning(animal1, bins)
  animal1bin <- setNames(tabulate(animal1bin), 1:max(animal1bin))
  animal2bin <- binning(animal2, bins)
  animal2bin <- setNames(tabulate(animal2bin), 1:max(animal2bin))
  animalbins <- rbind(animal1bin, animal2bin)
  size = 2 * bins
  
  chistat <- fisher.test(animalbins, simulate.p.value=TRUE, B=1e5) #may need B=1e7 (# replicaetes)
  
  pvalue = chistat["p.value"]
  
  return(pvalue)
}
