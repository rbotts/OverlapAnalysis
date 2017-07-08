##Custom Rao spacing test functions

rao <- function(x) { #Generates the U statistic. x must be a vector of values in radians
  if (max(x) > 2*pi | min(x) < 0) return("Input must be in radians")
  n <- length(x)
  lambda <- 2*pi/n
  
  fi <- x[order(x)]
  fi1 <- c(fi[-1], 0)
  Ti <- fi1 - fi #Calculates the distance between each point after being sorted
  Ti[n] <- ((2*pi)-fi[n])+fi[1] #Calculates the distance between first and last point, to account for "wrap" at edge of circular distribution
  
  U <- 0.5*sum(abs(Ti-lambda))
  return(U)
}

dU <- function(n) { #Supposed to calculate the U distribution for a given sample size
  p <- 2*pi
  
  combination <- function(a, b) {
   return(factorial(a)/(factorial(b)*factorial(a-b)))
  }
  
  g <- function(x, j) {
    return(h(x, j)/(p*factorial(j-1)))
  }
  
  h <- function(x, j) {
    a <- x/p
    k <- 0
    while (k < a) {
      if (k + 1 > a) {
        kmax <- k
        break
      }
      else k <- k+1
    }
    b <- rep.int(0, kmax+1)
    for (k in 0:kmax) {
      b[k] <- ((-1)^k)*combination(j, k)*((a-k)^(j-1))
    }
    return(sum(b))
  }
  
  f <- function(u) {
    s <- rep.int(0, n-1)
    for (j in 1:(n-1)) {
      s[j] <- combination(n, j)*((u/p)^(n-j-1))*g(n*u, j)/(factorial(n-j-1)*(n^(j-1)))
    }
    return(factorial(n-1)*sum(s))
  }
  
  return(f)
}

qU <- function(U, n) { #Supposed to compute the p-value of a given U statistic for the given sample size n
  integrate(dU(n), 0, U)
}
