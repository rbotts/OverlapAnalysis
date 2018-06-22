#R Version 3.4.4
#Database correction 2018
#This is just for our data set; not applicable for other research groups

raw.data <- read.csv("~/Documents/MooringData-June2018.csv", stringsAsFactors = FALSE)[-c(2,5:7,9,10,22:24)]

#Remove bad data from York and ASBC
raw.data <- raw.data[!(raw.data$CamNumber1 %in% c("N1", "N10", "N12", "N16", "Y5")), ]

#Surveys and Sites ----
raw.data$Survey.Name <- gsub(pattern = "Villa Mills", replacement = "Via Mills", x = raw.data$Survey.Name)
raw.data$Survey.Name <- gsub(pattern = "CB", replacement = "Cabo Blanco", x = raw.data$Survey.Name)

raw.data["Site"] <-
  gsub(
    "\\s*20\\d\\d\\s*|\\s*Spring\\s*|\\s*Summer\\s*|\\s*Fall\\s*|\\s*El\\s*|\\s*La\\s*|\\s*National Park\\s*",
    "",
    raw.data$Survey.Name
  )

raw.data["Season"] <- ifelse(grepl("Spring", raw.data$Survey.Name), "Spring", 
                             ifelse(grepl("Summer", raw.data$Survey.Name), "Summer",
                                    ifelse(grepl("Fall", raw.data$Survey.Name), "Fall", "Other")))

raw.data$Site[raw.data$Site == "Chirripo-"] <- "Chirripo"
raw.data$Site[raw.data$Site == "Bosque de Agua-"] <- "Bosque de Agua"

#Individual Cameras ----
source("Archive/R scripts/function.xy.R")
raw.data <- cbind(raw.data, xy(x = raw.data$X, y = raw.data$Y))

raw.data$Longitude[raw.data$CamNumber1 == "13"] <- -83.7838825778
raw.data$Latitude[raw.data$CamNumber1 == "13"] <- 9.563279

raw.data$Longitude[raw.data$CamNumber1 == "14"] <- -83.801117
raw.data$Latitude[raw.data$CamNumber1 == "14"] <- 9.584717

raw.data$Longitude[raw.data$CamNumber1 == "15"] <- -83.793333
raw.data$Latitude[raw.data$CamNumber1 == "15"] <- 9.585833

raw.data$Longitude[raw.data$CamNumber1 == "26"] <- -83.856367
raw.data$Latitude[raw.data$CamNumber1 == "26"] <- 9.614533

raw.data$Longitude[raw.data$CamNumber1 == "28"] <- -83.801117
raw.data$Latitude[raw.data$CamNumber1 == "28"] <- 9.586383

raw.data$Longitude[raw.data$CamNumber1 == "29"] <- -83.80195
raw.data$Latitude[raw.data$CamNumber1 == "29"] <- 9.58555

raw.data$Longitude[raw.data$CamNumber1 == "40"] <- -83.792833
raw.data$Latitude[raw.data$CamNumber1 == "40"] <- 9.551483

raw.data$Longitude[raw.data$CamNumber1 == "6"] <- -83.707417
raw.data$Latitude[raw.data$CamNumber1 == "6"] <- 9.551517

raw.data$Longitude[raw.data$CamNumber1 == "A13"] <- -83.610867
raw.data$Latitude[raw.data$CamNumber1 == "A13"] <- 9.33195

raw.data$Longitude[raw.data$CamNumber1 == "C1"] <- -83.561917
raw.data$Latitude[raw.data$CamNumber1 == "C1"] <- 9.4599

raw.data$Longitude[raw.data$CamNumber1 == "C10"] <- -83.501317
raw.data$Latitude[raw.data$CamNumber1 == "C10"] <- 9.463

raw.data$Longitude[raw.data$CamNumber1 == "C13"] <- -83.50595
raw.data$Latitude[raw.data$CamNumber1 == "C13"] <- 9.45175
  
raw.data$Longitude[raw.data$CamNumber1 == "C19"] <- -83.504333
raw.data$Latitude[raw.data$CamNumber1 == "C19"] <- 9.404717

raw.data$Longitude[raw.data$CamNumber1 %in% c("C22", "C23")] <- -83.4829199966
raw.data$Latitude[raw.data$CamNumber1 %in% c("C22", "C23")] <- 9.3467289582

raw.data$Longitude[raw.data$CamNumber1 %in% c("C24", "C25", "C26")] <- -83.4833
raw.data$Latitude[raw.data$CamNumber1 %in% c("C24", "C25", "C26")] <- 9.4833

raw.data$Longitude[raw.data$CamNumber1 == "C3"] <- -83.54795
raw.data$Latitude[raw.data$CamNumber1 == "C3"] <- 9.456517

raw.data$Longitude[raw.data$CamNumber1 == "CBBA4"] <- -83.4929199966
raw.data$Latitude[raw.data$CamNumber1 == "CBBA4"] <- 9.3467289582

raw.data$Longitude[raw.data$CamNumber1 == "LC1"] <- -84.392061
raw.data$Latitude[raw.data$CamNumber1 == "LC1"] <- 9.700102

raw.data$Longitude[raw.data$CamNumber1 == "LC2"] <- -84.3855445272
raw.data$Latitude[raw.data$CamNumber1 == "LC2"] <- 9.698995

raw.data$Longitude[raw.data$CamNumber1 == "LC3"] <- -84.3767923254
raw.data$Latitude[raw.data$CamNumber1 == "LC3"] <- 9.691998

raw.data$Longitude[raw.data$CamNumber1 == "LC4"] <- -84.4008442282
raw.data$Latitude[raw.data$CamNumber1 == "LC4"] <- 9.70903

raw.data$Longitude[raw.data$CamNumber1 == "LC5"] <- -84.3903
raw.data$Latitude[raw.data$CamNumber1 == "LC5"] <- 9.7064

raw.data$Longitude[raw.data$CamNumber1 == "LM6"] <- -83.682933
raw.data$Latitude[raw.data$CamNumber1 == "LM6"] <- 9.774967

raw.data$Longitude[raw.data$CamNumber1 %in% c("Molina", "Molinas Property")] <- -83.623467
raw.data$Latitude[raw.data$CamNumber1 %in% c("Molina", "Molinas Property")] <- 9.33185

# raw.data$Longitude[raw.data$CamNumber1 %in% c("N1", "N10", "N12", "N16")] <- -84.5966
# raw.data$Latitude[raw.data$CamNumber1 %in% c("N1", "N10", "N12", "N16")] <- 9.331

raw.data$Longitude[raw.data$CamNumber1 == "PILA5"] <- -82.97786
raw.data$Latitude[raw.data$CamNumber1 == "PILA5"] <- 9.08614

raw.data$Longitude[raw.data$CamNumber1 == "PNC2"] <- -84.597917
raw.data$Latitude[raw.data$CamNumber1 == "PNC2"] <- 9.79835

raw.data$Longitude[raw.data$CamNumber1 == "PNC3"] <- -84.601017
raw.data$Latitude[raw.data$CamNumber1 == "PNC3"] <- 9.775317

raw.data$Longitude[raw.data$CamNumber1 == "PNC4"] <- -84.601917
raw.data$Latitude[raw.data$CamNumber1 == "PNC4"] <- 9.77445

raw.data$Longitude[raw.data$CamNumber1 == "Q6"] <- -83.726841
raw.data$Latitude[raw.data$CamNumber1 == "Q6"] <- 8.6387890577

raw.data$Longitude[raw.data$CamNumber1 == "RCB1"] <- -85.1009965409
raw.data$Latitude[raw.data$CamNumber1 == "RCB1"] <- 9.58203

raw.data$Longitude[raw.data$CamNumber1 == "RCB2"] <- -85.0957057436
raw.data$Latitude[raw.data$CamNumber1 == "RCB2"] <- 9.586729

raw.data$Longitude[raw.data$CamNumber1 == "RCB3"] <- -85.0938910788
raw.data$Latitude[raw.data$CamNumber1 == "RCB3"] <- 9.588735

raw.data$Longitude[raw.data$CamNumber1 == "RCB4"] <- -85.1090110185
raw.data$Latitude[raw.data$CamNumber1 == "RCB4"] <- 9.580537

raw.data$Longitude[raw.data$CamNumber1 == "RCB5"] <- -85.1131997153
raw.data$Latitude[raw.data$CamNumber1 == "RCB5"] <- 9.580741

raw.data$Longitude[raw.data$CamNumber1 == "RCB6"] <- -85.1100533362
raw.data$Latitude[raw.data$CamNumber1 == "RCB6"] <- 9.589691

# raw.data$Longitude[raw.data$CamNumber1 == "Y5"] <- -84.5966
# raw.data$Latitude[raw.data$CamNumber1 == "Y5"] <- 9.331

#Camera Sites ----
switchLong <- Vectorize(function(a) {
  switch(
    EXPR = a,
    "Savegre Valley" = -83.7867,
    "ASBC" = -83.63265,
    "Tapanti" = -83.80563,
    "PN Carara" = -84.60031,
    "Cabo Blanco" = -85.10316,
    "Marta" = -83.68404,
    "Chirripo" = -83.52597,
    "Via Mills" = -83.68726
  )
})
switchLat <- Vectorize(function(a) {
  switch(
    EXPR = a,
    "Savegre Valley" = 9.55733,
    "ASBC" = 9.328311,
    "Tapanti" = 9.702569,
    "PN Carara" = 9.780654,
    "Cabo Blanco" = 9.584736,
    "Marta" = 9.773979,
    "Chirripo" = 9.451092,
    "Via Mills" = 9.56043
  )
})

raw.data$Longitude <- unlist(
  ifelse(
    test = is.na(raw.data$Longitude),
    yes = switchLong(raw.data$Site),
    no = raw.data$Longitude
  ))

raw.data$Latitude <- unlist(
  ifelse(
    test = is.na(raw.data$Latitude),
    yes = switchLat(raw.data$Site),
    no = raw.data$Latitude
  ))

#Plotting locations ----
omar <- par("mar")
par(mar = c(4,4,0,0))

require("rworldmap")
require("rworldxtra")
newmap <- getMap(resolution = "high")

plot(newmap, xlim = c(-86, -82.5), ylim = c(8, 11.2), xlab = "Longitude", ylab = "Latitude")
axis(1, at = seq(from = -82, to = -86, by = -0.2))
axis(side = 2)
points(x = raw.data$Longitude, y = raw.data$Latitude, pch = "+", col = "red")

par("mar" = omar)
