##Comparing overlap of a given species across seasons
require(overlap)

#Setup and preallocation
namelist <- names(table(ind.data$Species))
ovl.season.orig <- matrix(nrow=2, ncol=2, dimnames=list(c("Spring", "Summer"), c("Summer", "Fall")))
ovl.season.lower <- matrix(nrow=2, ncol=2, dimnames=list(c("Spring", "Summer"), c("Summer", "Fall")))
ovl.season.upper <- matrix(nrow=2, ncol=2, dimnames=list(c("Spring", "Summer"), c("Summer", "Fall")))
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

#Calculating Spring/Summer Overlap
animal1 <- subset(ind.data$TimeRad, ind.data$Species == name1 & ind.data$Season == "Spring")
boot1 <- resample(animal1, n.boot)
animal2 <- subset(ind.data$TimeRad, ind.data$Species == name1 & ind.data$Season == "Summer")
print(paste("Estimating overlap between", name1, "in the spring and", name1, "in the summer..."))
if (min(length(animal1), length(animal2)) <= 75 & min(length(animal1), length(animal2)) > 15) {
  boot2 <- resample(animal2, n.boot)
  ovl.orig <- overlapEst(animal1, animal2, adjust=c(0.8, NA, NA))[1]
  ovl.boot <- bootEst(boot1, boot2, adjust=c(0.8, NA, NA))[,1]
  
  #Storing the values into the matrices
  ovl.season.orig[1,1] <- ovl.orig
  ovl.boot.ci <- bootCI(ovl.orig, ovl.boot)
  ovl.season.lower[1,1] <- ovl.boot.ci[4,1]
  ovl.season.upper[1,1] <- ovl.boot.ci[4,2]
  
} else if (min(length(animal1), length(animal2)) > 75) {
  boot2 <- resample(animal2, n.boot)
  ovl.orig <- overlapEst(animal1, animal2, adjust=c(NA, 1, NA))[2]
  ovl.boot <- bootEst(boot1, boot2, adjust=c(NA, 1, NA))[,2]
  
  #Storing the values into the matrices
  ovl.season.orig[1,1] <- ovl.orig
  ovl.boot.ci <- bootCI(ovl.orig, ovl.boot)
  ovl.season.lower[1,1] <- ovl.boot.ci[4,1]
  ovl.season.upper[1,1] <- ovl.boot.ci[4,2]
  
} else print("Sample size too small to compare between these seasons.")

#Calculating Spring/Fall Overlap
animal1 <- subset(ind.data$TimeRad, ind.data$Species == name1 & ind.data$Season == "Spring")
boot1 <- resample(animal1, n.boot)
animal2 <- subset(ind.data$TimeRad, ind.data$Species == name1 & ind.data$Season == "Fall")
print(paste("Estimating overlap between", name1, "in the spring and", name1, "in the fall..."))
if (min(length(animal1), length(animal2)) <= 75 & min(length(animal1), length(animal2)) > 15) {
  boot2 <- resample(animal2, n.boot)
  ovl.orig <- overlapEst(animal1, animal2, adjust=c(0.8, NA, NA))[1]
  ovl.boot <- bootEst(boot1, boot2, adjust=c(0.8, NA, NA))[,1]
  
  #Storing the values into the matrices
  ovl.season.orig[1,2] <- ovl.orig
  ovl.boot.ci <- bootCI(ovl.orig, ovl.boot)
  ovl.season.lower[1,2] <- ovl.boot.ci[4,1]
  ovl.season.upper[1,2] <- ovl.boot.ci[4,2]
  
} else if (min(length(animal1), length(animal2)) > 75) {
  boot2 <- resample(animal2, n.boot)
  ovl.orig <- overlapEst(animal1, animal2, adjust=c(NA, 1, NA))[2]
  ovl.boot <- bootEst(boot1, boot2, adjust=c(NA, 1, NA))[,2]
  
  #Storing the values into the matrices
  ovl.season.orig[1,2] <- ovl.orig
  ovl.boot.ci <- bootCI(ovl.orig, ovl.boot)
  ovl.season.lower[1,2] <- ovl.boot.ci[4,1]
  ovl.season.upper[1,2] <- ovl.boot.ci[4,2]
  
} else print("Sample size too small to compare between these seasons.")

#Calculating Summer/Fall Overlap
animal1 <- subset(ind.data$TimeRad, ind.data$Species == name1 & ind.data$Season == "Summer")
boot1 <- resample(animal1, n.boot)
animal2 <- subset(ind.data$TimeRad, ind.data$Species == name1 & ind.data$Season == "Fall")
print(paste("Estimating overlap between", name1, "in the summer and", name1, "in the fall..."))
if (min(length(animal1), length(animal2)) <= 75 & min(length(animal1), length(animal2)) > 15) {
  boot2 <- resample(animal2, n.boot)
  ovl.orig <- overlapEst(animal1, animal2, adjust=c(0.8, NA, NA))[1]
  ovl.boot <- bootEst(boot1, boot2, adjust=c(0.8, NA, NA))[,1]
  
  #Storing the values into the matrices
  ovl.season.orig[2,2] <- ovl.orig
  ovl.boot.ci <- bootCI(ovl.orig, ovl.boot)
  ovl.season.lower[2,2] <- ovl.boot.ci[4,1]
  ovl.season.upper[2,2] <- ovl.boot.ci[4,2]
  
} else if (min(length(animal1), length(animal2)) > 75) {
  boot2 <- resample(animal2, n.boot)
  ovl.orig <- overlapEst(animal1, animal2, adjust=c(NA, 1, NA))[2]
  ovl.boot <- bootEst(boot1, boot2, adjust=c(NA, 1, NA))[,2]
  
  #Storing the values into the matrices
  ovl.season.orig[2,2] <- ovl.orig
  ovl.boot.ci <- bootCI(ovl.orig, ovl.boot)
  ovl.season.lower[2,2] <- ovl.boot.ci[4,1]
  ovl.season.upper[2,2] <- ovl.boot.ci[4,2]
  
} else print("Sample size too small to compare between these seasons.")

print(ovl.season.orig)
print(ovl.season.lower)
print(ovl.season.upper)
