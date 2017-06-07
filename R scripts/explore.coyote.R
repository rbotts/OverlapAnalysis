##Exploratory analysis based on Coyote abundance
require(overlap)

#Setup and preallocation
namelist <- names(table(ind.data$Species))
sitelist <- names(table(ind.data$Site))
coyote.abundance <- list(ASBC=17.89, CB=0, Chirripo=13.34, Pejibaye=mean(0, 5.68), PNC=0, PNLC=0, PNT=mean(1.85,1.66), "Savegre Valley"=83.00)
list.ovl <- list()
list.lower <- list()
list.upper <- list()
n.boot <- 10000 #The number of bootstrap samples to take, recommend ten thousand (10000)

for (i in 1:length(namelist)) {
  print(i)
}