##Overlap of two given species at each site
require(overlap)

#Setup and preallocation
namelist <- names(table(ind.data$Species))
sitelist <- names(table(ind.data$Site))
ovl.list <- list()
ready <- "n"
n.boot <- 10000 #The number of bootstrap samples to take, recommend ten thousand (10000)

#Interactive selection of two species
while (ready == "n") {

cat("\n")
print(namelist)
cat("\n")

x1 <- readline("Number of the first species: ")
x2 <- readline("Number of the second species: ")

x1 <- as.numeric(x1)
x2 <- as.numeric(x2)

name1 <- namelist[x1]
name2 <- namelist[x2]

print(paste("You chose", name1, "and", name2, ". Is that correct?"))
ready <- readline("y/n/quit: ")
if (ready == "n") print("No? Try again.")
}

if (ready == "y") {
  #Actually calculating the values
  for (i in 1:length(sitelist)) {
    print(paste("Estimating overlap between", name1, "and", name2, "at", sitelist[i], "..."))
    animal1 <- subset(ind.data$TimeRad, ind.data$Species == name1 & ind.data$Site == sitelist[i])
    animal2 <- subset(ind.data$TimeRad, ind.data$Species == name2 & ind.data$Site == sitelist[i])
    
    #Small sample size estimator
    if (min(length(animal1), length(animal2)) <= 75 & min(length(animal1), length(animal2)) > 5) {
      boot1 <- resample(animal1, n.boot)
      boot2 <- resample(animal2, n.boot)
      
      ovl.orig <- overlapEst(animal1, animal2, adjust=c(0.8, NA, NA))[1]
      ovl.boot <- bootEst(boot1, boot2, adjust=c(0.8, NA, NA))[,1]
      
      #Writing results to list
      ovl.boot.ci <- bootCI(ovl.orig, ovl.boot)
      ovl.list[[sitelist[i]]] <- c(estimate=ovl.orig, lower=ovl.boot.ci[4,1], upper=ovl.boot.ci[4,2], n1=length(animal1), n2=length(animal2))
      
    #Large sample size estimator
    } else if (min(length(animal1), length(animal2)) > 75) {
      boot1 <- resample(animal1, n.boot)
      boot2 <- resample(animal2, n.boot)
      
      ovl.orig <- overlapEst(animal1, animal2, adjust=c(NA, 1, NA))[2]
      ovl.boot <- bootEst(boot1, boot2, adjust=c(NA, 1, NA))[,2]
      
      #Writing results to list
      ovl.boot.ci <- bootCI(ovl.orig, ovl.boot)
      ovl.list[[sitelist[i]]] <- c(estimate=ovl.orig, lower=ovl.boot.ci[4,1], upper=ovl.boot.ci[4,2], n1=length(animal1), n2=length(animal2))
    } else print("Sample size too small to compare here.")
  }
  
  cat("\n")
  print(ovl.list)

}
