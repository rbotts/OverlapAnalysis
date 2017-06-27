#!!! Get the most updated code from the github repository here: https://github.com/rbotts/OverlapAnalysis
#Guide to the overlap package here: https://cran.r-project.org/web/packages/overlap/vignettes/overlap.pdf
require(overlap)

#Importing data from the sample csv
sample.data <- read.table("Examples/sampledata.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)

#Reformatting data
ind.data <- subset(sample.data, sample.data["Independent"] == "Yes")
ind.data <- subset(ind.data, ind.data$Species != "Unknown")
ind.data <- subset(ind.data, ind.data$Species != "")
ind.data["Date"] <- as.Date(ind.data$Date, "%m/%d/%Y")
max.time <- max(ind.data$Time)
if (max.time > 12 & max.time <= 24) {
  ind.data["TimeRad"] <- ind.data$Time*2*pi/24
} else if (max.time <= 1) {
  ind.data["TimeRad"] <- ind.data$Time*2*pi
} else print("Unknown time format.")
ind.data["Site"] <- gsub("\\s*20\\d\\d\\s*|\\s*Spring\\s*|\\s*Summer\\s*|\\s*Fall\\s*|\\s*El\\s*|\\s*La\\s*|\\s*National Park\\s*", "", ind.data$Survey.Name)
ind.data["Season"] <- ifelse(grepl("Spring", ind.data$Survey.Name), "Spring", ifelse(grepl("Summer", ind.data$Survey.Name), "Summer", ifelse(grepl("Fall", ind.data$Survey.Name), "Fall", "Other")))

#ind.data["Coyote"] <- ifelse(grepl("\\s*Bosque de Agua\\s*|\\s*Campanario\\s*|\\s*Copal\\s*|\\s*PNLC\\s*|\\s*PN Carara\\s*|\\s*Carara\\s*|\\s*PNC\\s*", ind.data$Site), TRUE, FALSE) #Only for exploratory coyote study, not necessary for sampledata.csv

#Grouping for our full data set; not necessary for sampledata.csv
ind.data["Site"] <- gsub("\\s*York Univ\\s*", "ASBC", ind.data$Site)
ind.data["Site"] <- gsub("\\s*Copal\\s*|\\s*Marta\\s*", "Pejibaye", ind.data$Site) #Comment out during exploratory coyote study
ind.data["Site"] <- gsub("\\s*PN Carara\\s*|\\s*Carara\\s*", "PNC", ind.data$Site)
ind.data["Site"] <- gsub("\\s*Tapanti\\s*|\\s*Via Mills\\s*|\\s*Villa Mills\\s*", "PNT", ind.data$Site)
ind.data$Habitat[ind.data$Site == "Savegre Valley"|ind.data$Site == "Chirripo"|ind.data$Site == "Bosque de Agua"|ind.data$Site == "PNT"|ind.data$Site == "Pejibaye"] <- "Cloud Forest"
ind.data$Habitat[ind.data$Site == "PNC"|ind.data$Site == "PNLC"|ind.data$Site == "CB"|ind.data$Site == "Campanario"|ind.data$Site == "ASBC"] <- "Lowland Forest"

print(table(ind.data$Species))
print(table(ind.data$Site))
print(table(ind.data$Habitat))
print(table(ind.data$Season))
