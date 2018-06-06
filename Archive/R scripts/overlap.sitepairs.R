##Overlap of one species at all pairs of sites
require(overlap)

#Setup and preallocation
namelist <- names(table(ind.data$Species))
sitelist <- names(table(ind.data$Site))
dimensions <- length(sitelist)
ovl.sites.orig <- matrix(nrow = dimensions, ncol = dimensions, dimnames = list(sitelist, sitelist))
ovl.sites.lower <- matrix(nrow = dimensions, ncol = dimensions, dimnames = list(sitelist, sitelist))
ovl.sites.upper <- matrix(nrow = dimensions, ncol = dimensions, dimnames = list(sitelist, sitelist))
ready <- "n"
n.boot <- 10000 #The number of bootstrap samples to take, recommend ten thousand (10000)

#Interactive selection of species
while (ready == "n") {
  
  cat("\n")
  print(namelist)
  cat("\n")
  
  x1 <- readline("Number of the species of interest: ")
  x1 <- as.numeric(x1)
  name1 <- namelist[x1]
  
  print(paste("You chose", name1, ". Is that correct?"))
  ready <- readline("y/n/quit: ")
  if (ready == "n") print("No? Try again.")
}

#Actually calculating the values
if (ready == "y") {
  for (i in 1:dimensions) {
    animal1 <- subset(ind.data$TimeRad, ind.data$Species == name1 & ind.data$Site == sitelist[i])
    
    if (length(animal1) > 15) {
      boot1 <- resample(animal1, n.boot)
      
      for (j in (i+1):dimensions) {
        animal2 <- subset(ind.data$TimeRad, ind.data$Species == name1 & ind.data$Site == sitelist[j])
        
        print(paste("Estimating overlap between", name1, "at", sitelist[i], "and", name1, "at", sitelist[j], "..."))
        #Small sample size estimator
        if (min(length(animal1), length(animal2)) <= 75 & length(animal2) > 15) {
          boot2 <- resample(animal2, n.boot)
          
          ovl.orig <- overlapEst(animal1, animal2, adjust=c(0.8, NA, NA))[1]
          ovl.boot <- bootEst(boot1, boot2, adjust=c(0.8, NA, NA))[,1]
          
          #Writing results to matrices
          ovl.sites.orig[i,j] <- ovl.orig
          ovl.boot.ci <- bootCI(ovl.orig, ovl.boot)
          ovl.sites.lower[i,j] <- ovl.boot.ci[4,1]
          ovl.sites.upper[i,j] <- ovl.boot.ci[4,2]
          
          #Large sample size estimator
        } else if (min(length(animal1), length(animal2)) > 75) {
          boot1 <- resample(animal1, n.boot)
          boot2 <- resample(animal2, n.boot)
          
          ovl.orig <- overlapEst(animal1, animal2, adjust=c(NA, 1, NA))[2]
          ovl.boot <- bootEst(boot1, boot2, adjust=c(NA, 1, NA))[,2]
          
          #Writing results to matrices
          ovl.sites.orig[i,j] <- ovl.orig
          ovl.boot.ci <- bootCI(ovl.orig, ovl.boot)
          ovl.sites.lower[i,j] <- ovl.boot.ci[4,1]
          ovl.sites.upper[i,j] <- ovl.boot.ci[4,2]
        } else print("Sample size too small to compare here.")
      }
    }
  }
  #Remove all columns without data
  l <- 0
  for (k in 1:dimensions) {
    if (all(is.na(ovl.sites.orig[,(k-l)])) & all(is.na(ovl.sites.lower[,(k-l)])) & all(is.na(ovl.sites.upper[,(k-l)]))) {
      ovl.sites.orig <- ovl.sites.orig[,-(k-l)]
      ovl.sites.lower <- ovl.sites.lower[,-(k-l)]
      ovl.sites.upper <- ovl.sites.upper[,-(k-l)]
      l <- l+1
    }
  }
  
  #Remove all rows without data
  l <- 0
  for (k in 1:dimensions) {
    if (all(is.na(ovl.sites.orig[(k-l),])) & all(is.na(ovl.sites.lower[(k-l),])) & all(is.na(ovl.sites.upper[(k-l),]))) {
      ovl.sites.orig <- ovl.sites.orig[-(k-l),]
      ovl.sites.lower <- ovl.sites.lower[-(k-l),]
      ovl.sites.upper <- ovl.sites.upper[-(k-l),]
      l <- l+1
    }
  }
  #Print out results
  print(ovl.sites.orig)
  print(ovl.sites.lower)
  print(ovl.sites.upper)
}
