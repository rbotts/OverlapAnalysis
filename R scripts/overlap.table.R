##Creating a matrix that shows the overlap between every set of two species
library(compiler)
require(overlap)

#Setup and preallocation
dimensions <- length(names(table(ind.data$Common)))
ovl.table.orig <- matrix(nrow = dimensions, ncol = dimensions, dimnames = list(names(table(ind.data$Common)), names(table(ind.data$Common))))
ovl.table.lower <- matrix(nrow = dimensions, ncol = dimensions, dimnames = list(names(table(ind.data$Common)), names(table(ind.data$Common))))
ovl.table.upper <- matrix(nrow = dimensions, ncol = dimensions, dimnames = list(names(table(ind.data$Common)), names(table(ind.data$Common))))
n.boot <- 10000 #The number of bootstrap samples to take, recommend ten thousand (10000)

#Actually calculating the values
n.compile <- getCompilerOption("optimize")
enableJIT(3)
for (i in 1:dimensions) {
  name1 <- names(table(ind.data$Common))[i]
  animal1 <- subset(ind.data$TimeRad, ind.data$Common == name1)
  boot1 <- resample(animal1, n.boot)
  for (j in (i+1):dimensions) {
    name2 <- names(table(ind.data$Common))[j]
    animal2 <- subset(ind.data$TimeRad, ind.data$Common == name2)
    boot2 <- resample(animal2, n.boot)
    
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
enableJIT(n.compile)

print(ovl.table.orig)
print(ovl.table.lower)
print(ovl.table.upper)