###############################################################
# Simulation of a continuous random variable via an 
# inverse lookup of its CDF
#
# Shankar Venkatagiri
#
# Disclaimer: You don't have to understand what the code does.
###############################################################

# For skewness and kurtosis
library(moments)

# The PDF of our random variable is an undulating
# symmetric polynomial function. We have constructed 
# this function by some reverse engineering, so that
# its integral between 0 and 4 is 1
pdf <- function(x) {
  return(1/10.1333 *(x^4 - 8*x^3 + 22*x^2 - 24*x + 10))
}

# CDF = Integral of the PDF. Not to worry!
cdf <- function(x) {
  return(1/10.1333 * ((x^5)/5 - 2*x^4 + 22*(x^3)/3 - 12*x^2 + 10*x))
}

# Inverse of a function, via binary search algorithm
# The argument FUN is assumed monotone increasing, 
# like in the case of a CDF
#
# Also note: an example where a function is passed
# as an argument to another function
inverseOf <- function(x, FUN) {
  
  leftLim <- 0; rightLim <- 4
  midLim <- (leftLim + rightLim)/2
  
  tolerance <- 0.001
  while(abs(FUN(midLim) - x) > tolerance) {

    if(x <= FUN(midLim)) {
      rightLim <- midLim
    } 
    else {
      leftLim <- midLim
    }
    midLim <- (leftLim + rightLim)/2
  }
  return(midLim)
}

# x values to help plot a continuous density
xrange <- seq(0, 4, 0.01)

# Display 2 square type plots, side by side
par(mfrow=c(1, 2), pty="s")

# Plot the density of the variable X
plot(xrange, pdf(xrange),
     main="Density of variable X",
     type="l", lwd=2,
     xlab="x", ylab="Density",
     col="navy")

plot(xrange, pdf(xrange),
     main="Density and CDF of variable X",
     type="l", lwd=2,
     ylim=c(0, 1),
     xlab="x", ylab="Distributions",
     yaxt="n",
     col="navy")

# Next, overlay the CDF of X
par(new=TRUE)
plot(xrange, cdf(xrange),
     type="l", lwd=2,
     ylim=c(0, 1),
     ann=FALSE,
     xaxt="n",
     col="red")

# Check that it's indeed a probability density, 
# by integrating the PDF over its range
cat("The integral of the PDF over the range of X is",
    round(integrate(pdf, 0, 4)$value, 2), "\n")

# Simulate this random variable X via the Inverse CDF
# For that, we generate a lot of random numbers, and
# map each of them to the inverse under the CDF
numSim <- 10000
randVals <- runif(numSim)

# XVals is the reference distribution. 
XVals <- sapply(randVals, inverseOf, cdf)

# Calculate the parameters and print
cat("******************************************\n")
cat("Parameters of the simulated numbers for X:\n")
cat("Mean = ", round(mean(XVals), 2), "\n")
cat("S.D. = ", round(sd(XVals), 2), "\n")
cat("Skew = ", round(skewness(XVals), 2), "\n")
cat("Kurtosis = ", round(kurtosis(XVals), 2), "\n")
cat("******************************************\n")

# Plot them - you will see these numbers mimic the PDF
plot(xrange, pdf(xrange),
     main="Density of variable X",
     type="l", lwd=2,
     xlab="x", 
     ylab="Density",
     col="navy")

histX <- hist(XVals, 
              breaks=30,
              freq=TRUE,
              col=colors()[20:40],
              xlim=c(0, 4),
              ylim=c(0,1000),
              xlab="x",
              main=paste("Simulation of X (n=", 
                         numSim,")", sep=""))
