#Comparing activity patterns of Puma concolor (Puma) and Tayassu tajuca (Collared Peccary)
puma <- subset(ind.data$TimeRad, ind.data["Species"] == "Puma concolor")
peccary <- subset(ind.data$TimeRad, ind.data["Species"] == "Tayassu tajuca")

if (min(length(puma), length(peccary)) <= 75) {
  print("delta1")
} else {
  print("delta5")
}