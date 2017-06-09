##Setting global variables for first Shiny app
require(overlap)

#Preparing dataset ind.data
sample.data <- read.table("../Examples/sampledata.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
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

#Grouping for our full data set; not necessary for sampledata.csv
ind.data["Site"] <- gsub("\\s*York Univ\\s*", "ASBC", ind.data$Site)
ind.data["Site"] <- gsub("\\s*Copal\\s*|\\s*Marta\\s*", "Pejibaye", ind.data$Site)
ind.data["Site"] <- gsub("\\s*PN Carara\\s*|\\s*Carara\\s*", "PNC", ind.data$Site)
ind.data["Site"] <- gsub("\\s*Tapanti\\s*|\\s*Via Mills\\s*|\\s*Villa Mills\\s*", "PNT", ind.data$Site)

namelist <- names(table(ind.data$Species))
sitelist <- names(table(ind.data$Site))
seasonlist <- names(table(ind.data$Season))
