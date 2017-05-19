#!!! Get the most updated code from the github repository here: https://github.com/rbotts/OverlapAnalysis
#Guide to the overlap package here: https://cran.r-project.org/web/packages/overlap/vignettes/overlap.pdf
require("overlap")
require("data.table")

#Importing data from the sample csv
sample.data <- read.table("Examples/sampledata.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)

#Reformatting data
sample.data["Date"] <- as.Date(sample.data["Date"][[1]], "%m/%d/%Y")
sample.data["TimeRad"] <- sample.data["Time"][[1]]*2*pi/24
ind.data <- subset(sample.data, sample.data["Independent"] == "Yes")

#Removing species that have less than 2 independent observations
ind.data <- as.data.frame(setDT(ind.data)[, .SD[.N >2], Species])

print(table(ind.data$Species))