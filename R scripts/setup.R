#!!! Get the most updated code from the github repository here: https://github.com/rbotts/OverlapAnalysis
#Guide to the overlap package here: https://cran.r-project.org/web/packages/overlap/vignettes/overlap.pdf
require("overlap")
require("data.table")

#Importing data from the sample csv
sample.data <- read.table("Examples/sampledata.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)

#Reformatting data
ind.data <- subset(sample.data, sample.data["Independent"] == "Yes")
ind.data <- subset(ind.data, ind.data$Species != "Unknown")
ind.data <- subset(ind.data, ind.data$Species != "")
ind.data["Date"] <- as.Date(ind.data["Date"][[1]], "%m/%d/%Y")
max.time <- max(ind.data$Time)
if (max.time > 1 & max.time <= 24) {
  ind.data["TimeRad"] <- ind.data["Time"][[1]]*2*pi/24
} else if (max.time <= 1) {
  ind.data["TimeRad"] <- ind.data["Time"][[1]]*2*pi
} else print("Unknown time format.")

#Removing species that have less than 2 independent observations
ind.data <- as.data.frame(setDT(ind.data)[, .SD[.N >2], Species])

print(table(ind.data$Species))