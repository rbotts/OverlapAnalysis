#R version 3.4.4
#Testing the effect of sample size on W statistic

#Script with W statistic functions
source("~/Documents/OverlapAnalysis/R scripts/function.W.R")
library("tictoc") #For measuring performance; not necessary for function, just comment out tic() and toc() functions

#Data import & Variable Definition
ind.dat <- read.csv(file = "~/Documents/MooringData2018.csv", stringsAsFactors = FALSE)
ind.dat <- subset(ind.dat, ind.dat$Independent == "Yes")
trials <- 100

#Sample data set
speciesList <- unique(ind.dat$Species)
dat1 <- ind.dat$Time[ind.dat$Species == speciesList[1]]
n1 <- length(dat1) #n1 = 607

for (k in 2:length(speciesList)) {
  species2 <- speciesList[k]
  tic(paste("Total work time for", species2))
  
  dat2 <- ind.dat$Time[ind.dat$Species == species2]
  n2 <- length(dat2)
  if (n2 > 15) {
    sizeList <- seq(from = 5, to = min(n1, n2), length.out = 50)
    finalW <- rep(NA, times = length(sizeList))
    print(paste0("Working on ", species2, "..."))
    
    for (size in sizeList) {
      randW <- rep(NA, times = trials)
      for (i in 1:trials) {
        sample1 <- sample(x = dat1, size = size)
        sample2 <- sample(x = dat2, size = size)
        randW[i] <- w.stat(sample1, sample2)
      }
      finalW[which(size==sizeList)] <- mean(randW)
    }
    
    plot(
      x = sizeList,
      y = finalW,
      xlab = "Sample Size of Bootstrap Samples",
      ylab = "Average W statistic",
      main = bquote(atop("Bootstrapped W statistic of activity data",
                         paste(" of ", italic("Agouti paca"), " and ", italic(.(speciesList[k])), " in ", .(trials), " trials")
      ))
    )
    abline(lm(finalW~sizeList), col = "darkred")
    legend("topright", col = "darkred", legend = paste("r =", round(cor(finalW, sizeList), 4)))
  }
  toc()
}
