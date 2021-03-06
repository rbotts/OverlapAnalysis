##Comparing activity patterns of Puma concolor (Puma) and Tayassu tajuca (Collared Peccary)
require(overlap)

puma <- subset(ind.data$TimeRad, ind.data["Species"] == "Puma concolor")
peccary <- subset(ind.data$TimeRad, ind.data["Species"] == "Tayassu tajuca")
overlapPlot(puma, peccary, lty=c(1,2))
legend("topleft", c("Puma", "Peccary"), lty=c(1,2), col=c("black", "blue"))

#Drawing smoothed bootstrap samples from each distribution and comparing overlap
puma.boot <- resample(puma, 10000)
print("Puma bootstrap successful.")
peccary.boot <- resample(peccary, 10000)
print("Peccary bootstrap successful.")

print("Estimating overlap...")
if (min(length(puma), length(peccary)) <= 75) {
  puma.peccary.boot <- bootEst(puma.boot, peccary.boot, adjust=c(0.8, NA, NA))[,1]
  puma.peccary.ovl <- overlapEst(puma, peccary, adjust=c(0.8, NA, NA))[1]
} else {
  puma.peccary.boot <- bootEst(puma.boot, peccary.boot, adjust=c(NA, 1, NA))[,2]
  puma.peccary.ovl <- overlapEst(puma, peccary, adjust=c(NA, 1, NA))[2]
}

#Printing out confidence interval and a warning if assumptions were violated
puma.peccary.ci <- bootCI(puma.peccary.ovl, puma.peccary.boot)
print(puma.peccary.ci[4,])
if (min(length(puma), length(peccary)) < 30 & puma.peccary.ovl > 0.9) print("Warning! Confidence interval may be too narrow! Your sample size is small and the overlap is nearly 100%.")