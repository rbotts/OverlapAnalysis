##Creating a matrix that shows the overlap between every set of two species
#Setup and preallocation
dimensions <- length(names(table(ind.data$Common)))
ovl.table.orig <- matrix(nrow = dimensions, ncol = dimensions, dimnames = list(names(table(ind.data$Common)), names(table(ind.data$Common))))
ovl.table.lower <- matrix(nrow = dimensions, ncol = dimensions, dimnames = list(names(table(ind.data$Common)), names(table(ind.data$Common))))
ovl.table.upper <- matrix(nrow = dimensions, ncol = dimensions, dimnames = list(names(table(ind.data$Common)), names(table(ind.data$Common))))

#Actually calculating the values
for (i in 1:dimensions) {
  name1 <- names(table(ind.data$Common))[i]
  animal1 <- subset(ind.data$TimeRad, ind.data$Common == name1)
  boot1 <- resample(animal1, 10000)
  for (j in 1:dimensions) {
    name2 <- names(table(ind.data$Common))[j]
    animal2 <- subset(ind.data$TimeRad, ind.data$Common == name2)
    boot2 <- resample(animal2, 10000)
    
    print(paste("Estimating overlap between", name1, "and", name2, "..."))
    if (length(min(animal1, animal2)) <= 75) {
      ovl.orig <- overlapEst(animal1, animal2, adjust=c(0.8, NA, NA))[1]
      ovl.boot <- bootEst(boot1, boot2, adjust=c(0.8, NA, NA))[,1]
    } else {
      ovl.orig <- overlapEst(animal1, animal2, adjust=c(NA, 1, NA))[2]
      ovl.boot <- bootEst(boot1, boot2, adjust=c(NA, 1, NA))[,2]
    }
    #Storing the values into the matrices
    ovl.table.orig[i,j] <- ovl.orig
    ovl.boot.ci <- bootCI(ovl.orig, ovl.boot)
    ovl.table.lower[i,j] <- ovl.boot.ci[4,1]
    ovl.table.upper[i,j] <- ovl.boot.ci[4,2]
  }
}

print(ovl.table.orig)
print(ovl.table.lower)
print(ovl.table.upper)