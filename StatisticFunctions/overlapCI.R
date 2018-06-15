#Function to find the confidence interval for the overlap coefficient
overlapCI <- function(animal1, animal2, n.boot = 10000) {
  #' Estimates the overlap coefficient (\Delta) from two sets of circular data. See: Ridout and Linkie, "Estimating Overlap of Daily Activity Patterns From Camera Trap Data" in \emph{Journal of Agricultural, Biological, and Environmental Statistics} (2009)
  #' 
  #' @param animal1 A numerical vector of measured points for the first species in RADIANS.
  #' @param animal2 A numerical vector of measured points for the second species in RADIANS.
  #' @param n.boot How many bootstrap trials should be run? Defaults to ten thousand.
  
  require("overlap")
  
  #Preallocate output
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