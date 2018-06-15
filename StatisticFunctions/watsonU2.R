#Function to calculate Watson's U-squared statistic
watson2 <- function(x, y) {
  #' Calculates Watson's U-squared from two sets of circular data. See: Zar, "Biostatistical Analysis", section 27.6 (1999)
  #' 
  #' @param x A numerical vector of circular data.
  #' @param y A numerical vector of circular data.
  
  n1 <- length(x)
  n2 <- length(y)
  if (min(n1, n2) <= 17) return("Sample too small")
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