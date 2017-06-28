#Based on S-plus code licensed under GPLv2 by Ulric Lund at http://statweb.calpoly.edu/lund/
#Modified by TJ Wiegman in June 2017, with the following changes:
#1 - Removed all cat() statements
#2 - Now returns values rather than printing to console
#3 - Changed default alpha value from 0 to 0.05

watson2 <- function(x, y, alpha = 0.05, plot = FALSE) {
  n1 <- length(x)
  n2 <- length(y)
  n <- n1 + n2
  if(n < 18) {
    return("Total Sample Size < 18:  Consult tabulated critical values")
  }
  if(plot == TRUE) {
    x <- sort(x %% (2 * pi))
    y <- sort(y %% (2 * pi))
    plot.edf(x, main = "Comparison of Empirical CDFs", xlab= "", ylab = "")
    par(new = TRUE)
    plot.edf(y, xlab = "", ylab = "", axes = FALSE, lty = 2)
  }
  x <- cbind(sort(x %% (2 * pi)), rep(1, n1))
  y <- cbind(sort(y %% (2 * pi)), rep(2, n2))
  xx <- rbind(x, y)
  rank <- order(xx[, 1])
  xx <- cbind(xx[rank,  ], seq(1:n))
  a <- c(1:n)
  b <- c(1:n)
  for(i in 1:n) {
    a[i] <- sum(xx[1:i, 2] == 1)
    b[i] <- sum(xx[1:i, 2] == 2)
  }
  d <- b/n2 - a/n1
  dbar <- mean(d)
  u2 <- (n1 * n2)/n^2 * sum((d - dbar)^2)
  crits <- c(99, 0.385, 0.268, 0.187, 0.152)
  if(sum(alpha == c(0, 0.001, 0.01, 0.05, 0.1)) == 0)
    stop("Invalid input for alpha")
  else if(alpha == 0) {
    if(u2 > 0.385)
      return("P-value < 0.001")
    else if(u2 > 0.268)
      return("0.001 < P-value < 0.01")
    else if(u2 > 0.187)
      return("0.01 < P-value < 0.05")
    else if(u2 > 0.152)
      return("0.05 < P-value < 0.10")
    else return("P-value > 0.10")
  }
  else {
    index <- (1:5)[alpha == c(0, 0.001, 0.01, 0.05, 0.1)]
    Critical <- crits[index]
    if (u2 > Critical) Reject <- TRUE #Reject the null hypothesis; x and y are significantly different
    else Reject <- FALSE #Do not reject the null hypothesis; x and y are not significantly different
    
    return(list("Reject"=Reject, "U2"=u2, "Alpha"=alpha, "CriticalValue"=Critical))
  }
}
