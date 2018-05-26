#R version 3.4.4
#Testing the effect of sample size on W statistic

#Script with W statistic functions
source("~/Documents/OverlapAnalysis/R scripts/function.W.R")

#Data import & Variable Definition
ind.dat <- read.csv(file = "~/Documents/MooringData2018.csv", stringsAsFactors = TRUE)
ind.dat <- subset(ind.dat, ind.dat$Independent == "Yes")
sizeList <- c(10,30,50,100,200)
trials <- 10000
finalW <- rep(NA, times = length(sizeList))

#Sample data set (very little overlap)
dat1 <- ind.dat$Time[ind.dat$Species == "Agouti paca"] #n = 607
dat2 <- ind.dat$Time[ind.dat$Species == "Tayassu tajuca"] #n = 605

tic("Analyzing data...")
for (size in sizeList) {
  randW <- rep(NA, times = trials)
  for (i in 1:trials) {
    sample1 <- sample(x = dat1, size = size)
    sample2 <- sample(x = dat2, size = size)
    randW[i] <- w.stat(sample1, sample2)
  }
  finalW[which(size==sizeList)] <- mean(randW)
}
toc()

plot(
  x = sizeList,
  y = finalW,
  xlab = "Sample Size of Bootstrap Samples",
  ylab = "Average W statistic",
  main = bquote(atop("Bootstrapped W statistic of activity data",
    paste(" of ", italic("Agouti paca "), " and ", italic("Sylvilagus dicei "), " in ", .(trials), " trials")
  ))
)
abline(lm(finalW~sizeList), col = "darkred")
legend("topleft", col = "darkred", legend = paste("r =", round(cor(finalW, sizeList), 4)))
