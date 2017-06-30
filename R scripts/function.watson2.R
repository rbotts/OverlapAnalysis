##Custom watson2 functions written from scratch
#Based on section 27.6 in book "Biostatistical Analysis" by JH Zar (1999), 4th Ed (ISBN: 0-13-081542-X),
#journal article "Goodness-of-fit tests on a circle" by GS Watson (1962) in Biometrika (DOI: 10.2307/2333467),
#and journal article "Chi-Square Approximations for the Distributions of Goodness-Of-Fit Statistics U^2 and W^2" by ML Tiku (1965) in Biometrika (DOI: 10.2307/2333714)

#NOTE: The p-value calculations done in watson2test and watson2p are approximations, and tend to be less accurate with smaller sample sizes and/or larger p-values

watson2 <- function(x, y) { #Function to calculate U-squared statistic, modified to work with "tied" data (See: Zar 1999)
  n1 <- length(x)
  n2 <- length(y)
  #if (min(n1, n2) <= 17) return("Sample too small")
  N <- n1+n2
  
  a <- c(x,y) #Putting all unique values into one ordered set, creating a unified coordinate system
  a <- a[duplicated(a) == FALSE]
  a <- a[order(a)]
  kmax <- length(a)
  
  t1 <- t2 <- m1 <- m2 <- c1 <- c2 <- rep.int(0, kmax) #Preallocating several variables
  
  for (k in 1:kmax) { #Finding the frequency of each value in x and y, respectively
    t1[k] <- sum(x==a[k])
    t2[k] <- sum(y==a[k])
  }
  
  m1 <- cumsum(t1) #Calculating the cumulative frequency distributions of x and y, respectively
  m2 <- cumsum(t2)
  
  c1 <- m1/n1 #Calculating the cumulative relative frequency distributions of x and y, respectively
  c2 <- m2/n2
  
  d <- c1-c2 
  t <- t1+t2 #The total frequency of each value; used when there are multiple observations of a value (ties)
  
  da <- sum(d*t)
  db <- sum(d*d*t)
  U2 <- ((n1*n2)/(N^2))*(db - ((da^2)/N)) #Calculating the U-squared statistic
  
  return(U2)
}

watson2test <- function(x, y) { #Function to approximate the p-value of Watson's U-squared, given two vectors (See: Tiku 1965)
  U2 <- watson2(x, y) #Calculate U-squared
  N <- length(x) + length(y)
  
  a <- ((21*N)-56)/(840*(N-1.5))
  b <- (N-1.5)/(42*N)
  f <- ((49*N)*(N-1))/(20*((N-1.5)^2)) #Approximation constants
  
  chi <- (U2-a)/b
  p <- pchisq(q = chi, df = f, lower.tail = FALSE) #Approximating from chi-squared distribution
  return(p)
}

watson2p <- function(U2, N) { #Function to approxiate the p-value of Watson's U-squared statistic, given total sample size N (See: Tiku 1965)
  a <- ((21*N)-56)/(840*(N-1.5))
  b <- (N-1.5)/(42*N)
  f <- ((49*N)*(N-1))/(20*((N-1.5)^2)) #Approximation constants
  
  chi <- (U2-a)/b
  p <- pchisq(q = chi, df = f, lower.tail = FALSE) #Approximating from chi-squared distribution
  return(p)
}

x1 <- c(40,45,50,55,70,80,80,95,105,110,110,120)
y1 <- c(30,35,50,60,65,65,75,80, 90,100)
x2 <- c(35,45,50,55,60,70,85,95,105,120)
y2 <- c(75,80,90,100,110,130,135,140,150,155,165)