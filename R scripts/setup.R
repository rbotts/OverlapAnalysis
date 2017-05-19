#!!! Get the most updated code from the github repository here: https://github.com/rbotts/OverlapAnalysis
#Guide to the overlap package here: https://cran.r-project.org/web/packages/overlap/vignettes/overlap.pdf
require("overlap")

#Importing data from the sample csv
sample.data <- read.table("Examples/sampledata.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)

#Reformatting data
sample.data["Date"] <- as.Date(sample.data["Date"][[1]], "%m/%d/%Y")
sample.data["TimeRad"] <- sample.data["Time"][[1]]*2*pi/24
ind.data <- subset(sample.data, sample.data["Independent"] == "Yes")

#Removing species that have less than 2 independent observations (somewhat sloppily)
namelist <- names(table(ind.data$Species))
for (i in 1:length(namelist)) {
  if (table(ind.data$Species)[namelist[i]] <= 2) {
    while (namelist[i] %in% ind.data$Species) {
      j <- match(namelist[i], ind.data$Species)
      ind.data <- ind.data[-j,]
    }
  }
}

print(table(ind.data$Species))