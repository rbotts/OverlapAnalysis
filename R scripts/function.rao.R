##Custom Rao spacing test functions

rao.space.U <- function(x) { #Generates the U statistic in degrees. Input x must be a vector of values in radians
  p <- 2*pi
  x <- x[!is.na(x)]
  if (max(x) > p | min(x) < 0) {
    warning("Input must be in radians")
    return(NA)
  }
  
  n <- length(x)
  if (n <= 4) {
    warning("Sample size too small!")
    return(NA)
  }
  
  lambda <- 360/n
  x <- 360*x/p #Converting x to degrees
  
  fi <- x[order(x)]
  fi1 <- c(fi[-1], 0)
  Ti <- fi1 - fi #Calculates the distance between each point after being sorted
  Ti[n] <- (p-fi[n])+fi[1] #Calculates the distance between first and last point, to account for "wrap" at edge of circular distribution
  
  U <- 0.5*sum(abs(Ti-lambda))
  return(U)
}

rao.space.test <- function(x) { #Returns the maximum p-value of the U statistic generated from the input data set x
  U <- rao.space.U(x)
  n <- length(x)
  return(rao.space.p(U, n)) #See below for function rao.space.p()
}


#The following is based on work from package "circular", by Ulric Lund and Claudio Agostinelli, licensed under the Gnu GPL v2+
#Changes primarily include output as return() [rather than cat()] and some readability improvements

rao.space.p <- function(U, n) { #Returns the maximum p-value of obtaining the given U statistic from a sample of size n
  if (is.na(U)) return(NA)
  #Load table of critical values and select row based on sample size
  load("rao.table.rda")
  ifelse(n <= 30, table.row <- n-3,
        ifelse(n <= 32, table.row <- 27,
        ifelse(n <= 37, table.row <- 28,
        ifelse(n <= 42, table.row <- 29,
        ifelse(n <= 47, table.row <- 30,
        ifelse(n <= 62, table.row <- 31,
        ifelse(n <= 87, table.row <- 32,
        ifelse(n <= 125, table.row <- 33,
        ifelse(n <= 175, table.row <- 34,
        ifelse(n <= 250, table.row <- 35,
        ifelse(n <= 350, table.row <- 36,
        ifelse(n <= 450, table.row <- 37,
        ifelse(n <= 550, table.row <- 38,
        ifelse(n <= 650, table.row <- 39,
        ifelse(n <= 750, table.row <- 40,
        ifelse(n <= 850, table.row <- 41,
        ifelse(n <= 950, table.row <- 42,
        table.row <- 43)))))))))))))))))
  
  #Returns *maximum* p-value based on sample size and given value of U
  if (U > rao.table[table.row, 1])
    return(0.001)
  else if (U > rao.table[table.row, 2])
    return(0.01)
  else if (U > rao.table[table.row, 3])
    return(0.05)
  else if (U > rao.table[table.row, 4])
    return(0.10)
  else return(1)
}
