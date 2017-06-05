##Comparing every species between lowland and cloud forests
require(overlap)

#Setup and preallocation
namelist <- names(table(ind.data$Species))
dimensions <- length(namelist)
hab.list <- list()
n.boot <- 10000 #The number of bootstrap samples to take, recommend ten thousand (10000)

for (i in 1:dimensions) {
  name1 <- namelist[i]
  print(paste("Analyzing habitat usage by", name1, "..."))
  animal1 <- subset(ind.data$TimeRad, ind.data$Species == name1 & ind.data$Habitat == "Lowland Forest")
  animal2 <- subset(ind.data$TimeRad, ind.data$Species == name1 & ind.data$Habitat == "Cloud Forest")
  
  #Small sample size estimator
  if (min(length(animal1), length(animal2)) <= 75 & min(length(animal1), length(animal2)) > 15) {
    boot1 <- resample(animal1, n.boot)
    boot2 <- resample(animal2, n.boot)
    
    ovl.orig <- overlapEst(animal1, animal2, adjust=c(0.8, NA, NA))[1]
    ovl.boot <- bootEst(boot1, boot2, adjust=c(0.8, NA, NA))[,1]
    
    #Writing results to list
    ovl.boot.ci <- bootCI(ovl.orig, ovl.boot)
    hab.list[[namelist[i]]] <- c(estimate=ovl.orig, lower=ovl.boot.ci[4,1], upper=ovl.boot.ci[4,2], nLow=length(animal1), nCloud=length(animal2))
    
  #Large sample size estimator
  } else if (min(length(animal1), length(animal2)) > 75) {
    boot1 <- resample(animal1, n.boot)
    boot2 <- resample(animal2, n.boot)
    
    ovl.orig <- overlapEst(animal1, animal2, adjust=c(NA, 1, NA))[2]
    ovl.boot <- bootEst(boot1, boot2, adjust=c(NA, 1, NA))[,2]
    
    #Writing results to list
    ovl.boot.ci <- bootCI(ovl.orig, ovl.boot)
    hab.list[[namelist[i]]] <- c(estimate=ovl.orig, lower=ovl.boot.ci[4,1], upper=ovl.boot.ci[4,2], nLow=length(animal1), nCloud=length(animal2))
  } else {
    hab.list[[namelist[i]]] <- c(estimate=NA, lower=NA, upper=NA, nLow=length(animal1), nCloud=length(animal2))
    print("Sample size too small to analyze.")
  }
}

print(hab.list)
