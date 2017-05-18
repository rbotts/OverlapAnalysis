##Creating a matrix that shows the overlap between every set of two species
dimensions <- length(names(table(ind.data$Common)))
ovl.table <- matrix(nrow = dimensions, ncol = dimensions, dimnames = list(names(table(ind.data$Common)), names(table(ind.data$Common))))

for (i in 1:dimensions) {
  name1 <- names(table(ind.data$Common))[i]
  animal1 <- subset(ind.data$TimeRad, ind.data$Common == name1)
  boot1 <- resample(animal1, 10000)
  for (j in 1:dimensions) {
    name2 <- names(table(ind.data$Common))[j]
    animal2 <- subset(ind.data$TimeRad, ind.data$Common == name2)
    boot2 <- resample(animal2, 10000)
    
    if (min(length(animal1, animal2)) <= 75) {
      ovl.boot <- bootEst(boot1, boot2, adjust=c(0.8, NA, NA))[,1]
      ovl <- overlapEst(animal1, animal2, adjust=c(0.8, NA, NA))[1]
    } else {
      ovl.boot <- bootEst(boot1, boot2, adjust=c(NA, 1, NA))[,2]
      ovl <- overlapEst(animal1, animal2, adjust=c(NA, 1, NA))[2]
    }
  }
}