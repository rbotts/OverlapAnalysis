##Exploratory analysis based on Coyote abundance
require(overlap)

#Setup and preallocation
namelist <- names(table(ind.data$Species))
sitelist <- names(table(ind.data$Site))
coyote.abundance <- list(ASBC=17.89, CB=0, Chirripo=13.34, Copal=0, Marta=5.68, Pejibaye=mean(5.68, 0), PNC=0, PNLC=0, PNT=mean(1.85,1.66), "Savegre Valley"=83.00)
coyote.ovl.list <- list()

for (j in 1:length(namelist)) {
  name1 <- namelist[j]
  animal1 <- subset(ind.data$TimeRad, ind.data$Species == name1 & ind.data$Coyote == FALSE)

  for (i in 1:length(sitelist)) {
    animal2 <- subset(ind.data$TimeRad, ind.data$Species == name1 & ind.data$Site == sitelist[i])
  
    print(paste("Comparing", name1, "activity at sites without coyotes to activity at", sitelist[i]))
    #Small sample size estimator
    if (min(length(animal1), length(animal2)) <= 75 & min(length(animal1), length(animal2)) > 15) {
      #ovl.orig <- overlapEst(animal1, animal2, adjust=c(0.8, NA, NA))[1]
      coyote.ovl.list[[namelist[j]]][[sitelist[i]]] <- overlapEst(animal1, animal2, adjust=c(NA, 1, NA))[2] 
      
    #Large sample size estimator
    } else if (min(length(animal1), length(animal2)) > 75) {
      #ovl.orig <- overlapEst(animal1, animal2, adjust=c(NA, 1, NA))[2]
      coyote.ovl.list[[namelist[j]]][[sitelist[i]]] <- overlapEst(animal1, animal2, adjust=c(NA, 1, NA))[2] #c(estimate=ovl.orig, n1=length(animal1), n2=length(animal2))
    } else print("Sample size too small to compare here.")
  }
}

#Plotting correlation and calculating coefficients
for (j in 1:length(namelist)) {
  if (length(coyote.ovl.list[[namelist[j]]]) > 3) {
    namelist2 <- names(coyote.ovl.list[[namelist[j]]])
    x <- c()
    y <- c()
    for (i in 1:length(namelist2)) {
      x <- c(x, coyote.abundance[[namelist2[i]]][1])
      y <- c(y, coyote.ovl.list[[namelist[j]]][i])
    }
    plot(x,y, main=namelist[j], xlab="Coyote RAI", ylab="Overlap Coefficient")
    coyote.ovl.list[[namelist[j]]] <- c(coyote.ovl.list[[namelist[j]]], r=cor(x,y))
  }
}

print(coyote.ovl.list)
