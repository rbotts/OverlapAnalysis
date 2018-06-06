#Exploratory study on the effects of lunar cycle on mammal behavior
require(overlap)
require(suncalc)

ind.data["Lunar"] <- (getMoonIllumination(ind.data$Date, "phase")[,2])*2*pi
namelist <- names(table(ind.data$Species))

for (i in 1:length(namelist)) {
  active <- subset(
    ind.data$Lunar,
    ind.data$Species == namelist[i] & (ind.data$TimeRad <= pi/2 | ind.data$TimeRad >= 3*pi/2)
  )
  if (length(active) >= 15) {
    fn <- gsub(pattern=" ", replacement="-", x=paste0("~/Downloads/Lunar/",namelist[i],"-lunar.jpg"), fixed = TRUE)
    jpeg(filename=fn, width=1280, height=720)
    densityPlot(active, xscale=NA, xlab="Lunar Phase", xaxt="n", main=paste0(namelist[i], ", n = ", length(active)))
    rug(active, side = 1)
    axis(side = 1, at = c(0, pi/2, pi, 3*pi/2, 2*pi), labels = c("New Moon", "First Quarter", "Full Moon", "Last Quarter", "New Moon"))
    dev.off()
  }
}
