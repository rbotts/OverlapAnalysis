#Characterizing Effect of Sample Size on Wn Statistic
source("Wn/function.Wn.R")

#Declaring Constants
pts <- 1000 #Number of points in each dataset, must be 150 or greater (or change sizeList)
sizeList <- c(3:30, seq(from = 35, to = 100, by = 5), seq(from = 125, to = pts, by = 25))
sz <- length(sizeList)
trials <- 200 #Number of trials to run for each bootstrap

#Opposite data ----
#Generate random data
dat1 <- rnorm(n = pts, mean = 0, sd = 1) #Normally distributed random data clustered around 0 = 2pi
while (min(dat1) < 0) dat1[dat1 < 0] <- dat1[dat1 < 0] + 2*pi #Add 2pi to negative values
while (max(dat1) > 2*pi) dat1[dat1 > 2*pi] <- dat1[dat1 > 2*pi] - 2*pi #Subtract 2pi from values over 2pi

dat2 <- rnorm(n = pts, mean = pi, sd = 1) #Normally distributed random data clustered around pi
while (min(dat2) < 0) dat2[dat2 < 0] <- dat2[dat2 < 0] + 2*pi #Add 2pi to negative values
while (max(dat2) > 2*pi) dat2[dat2 > 2*pi] <- dat2[dat2 > 2*pi] - 2*pi #Subtract 2pi from values over 2pi

#Bootstrap comparison of sample size effect
outWnO <- rep(NA, times = length(sizeList))
outpO <- rep(NA, times = length(sizeList))
for (j in sizeList[1:(sz-1)]) {
  if (j%%5 == 0) print(paste0("Working on n = ", j, "..."))
  randFrame <- data.frame("Wn" = rep(NA, times = trials))
  for (i in 1:trials) {
    sample1 <- sample(x = dat1, size = j)
    sample2 <- sample(x = dat2, size = j)
    out <- wn.prob(sample1, sample2, trials = 100)
    randFrame$"Wn"[i] <- out["Wn"]
    randFrame$"p-value"[i] <- out["p-value"]
  }
  outWnO[which(j == sizeList)] <- mean(randFrame$"Wn")
  outpO[which(j == sizeList)] <- mean(randFrame$"p-value")
}
out <- wn.prob(dat1, dat2, trials = 100)
outWnO[sz] <- out["Wn"]
outpO[sz] <- out["p-value"]

plot(
  x = sizeList,
  y = outWnO,
  xlab = "Sample Size of Bootstrap Samples",
  ylab = "",
  ylim = c(0, 3*max(outWnO)),
  main = bquote(atop("Bootstrapped Wn statistic of Random Data", .(paste0("from Opposite Normal Distributions (", trials, " Bootstraps)")))),
  pch = 20
)
points(
  x = sizeList,
  y = outWnO/outWnO[sz],
  pch = 1,
  col = "blue"
)
points(
  x = sizeList,
  y = outpO,
  pch = 18,
  col = "darkred"
)
legend(
  "topright",
  legend = c("Average Wn Statistic", "Proportion of True Wn Statistic", "Average p-value"),
  pch = c(20, 1, 18),
  col = c("black",  "blue", "darkred")
)


#Identical data ----
#Generate random data
dat1 <- rnorm(n = pts, mean = pi, sd = 1) #Normally distributed random data clustered around pi
while (min(dat1) < 0) dat1[dat1 < 0] <- dat1[dat1 < 0] + 2*pi #Add 2pi to negative values
while (max(dat1) > 2*pi) dat1[dat1 > 2*pi] <- dat1[dat1 > 2*pi] - 2*pi #Subtract 2pi from values over 2pi

dat2 <- rnorm(n = pts, mean = pi, sd = 1) #Normally distributed random data clustered around pi
while (min(dat2) < 0) dat2[dat2 < 0] <- dat2[dat2 < 0] + 2*pi #Add 2pi to negative values
while (max(dat2) > 2*pi) dat2[dat2 > 2*pi] <- dat2[dat2 > 2*pi] - 2*pi #Subtract 2pi from values over 2pi

#Bootstrap comparison of sample size effect
outWnI <- rep(NA, times = length(sizeList))
outpI <- rep(NA, times = length(sizeList))
for (j in sizeList[1:(sz-1)]) {
  if (j%%5 == 0) print(paste0("Working on n = ", j, "..."))
  randFrame <- data.frame("Wn" = rep(NA, times = trials))
  for (i in 1:trials) {
    sample1 <- sample(x = dat1, size = j)
    sample2 <- sample(x = dat2, size = j)
    out <- wn.prob(sample1, sample2, trials = 100)
    randFrame$"Wn"[i] <- out["Wn"]
    randFrame$"p-value"[i] <- out["p-value"]
  }
  outWnI[which(j == sizeList)] <- mean(randFrame$"Wn")
  outpI[which(j == sizeList)] <- mean(randFrame$"p-value")
}
out <- wn.prob(dat1, dat2, trials = 100)
outWnI[sz] <- out["Wn"]
outpI[sz] <- out["p-value"]

plot(
  x = sizeList,
  y = outWnI,
  xlab = "Sample Size of Bootstrap Samples",
  ylab = "",
  ylim = c(0, 3*max(outWnI)),
  main = bquote(atop("Bootstrapped Wn statistic of Random Data", .(paste0("from Identical Normal Distributions (", trials, " Bootstraps)")))),
  pch = 20
)
points(
  x = sizeList,
  y = outWnI/outWnI[sz],
  pch = 1,
  col = "blue"
)
points(
  x = sizeList,
  y = outpI,
  pch = 18,
  col = "darkred"
)
legend(
  "topright",
  legend = c("Average Wn Statistic", "Proportion of True Wn Statistic", "Average p-value"),
  pch = c(20, 1, 18),
  col = c("black",  "blue", "darkred")
)
