###############################################
# Code to demonstrate that the Binomial
# Distribution for large values of n,
# and a significantly high value of p
# converges to a Normal distribution for
# the corresponding parameters
#
# Here mean = np and variance = np(1 - p)
#
# We will see this by interacting with the
# sliders on the side window
###############################################

# Do not try to install this package!
# It is shipped by default with RStudio
library("manipulate")

# Function to plot the binomial PMF
# given the parameters n and p
plotBinomial <- function(n , p) {
  
  # Parameters of the binomial variable
  mean <- n*p
  variance <- n*p*(1 - p)
  sd <- sqrt(variance)
  
  # If we believe that a Binomial approaches
  # a Normal, then the distribution is 
  # pretty much contained by these limits
  limLow <- round(mean - 4*sd, 0)
  limHigh <- round(mean + 4*sd, 0)
  
  # x and y values for the PDF
  xValues <- seq(limLow, limHigh, 1)
  yValues <- dbinom(xValues, n, p)
  
  # Now plot the points
  points(xValues, yValues,
         col="navy", pch=20)
}

# Function to plot the normal PDF
# given the parameters n and p
plotNormal <- function(n, p) {
  
  # Parameters corresponding to the
  # Binomial parameters
  mean <- n*p
  variance <- n*p*(1 - p)
  sd <- sqrt(variance)
  
  limLow <- mean - 4*sd
  limHigh <- mean + 4*sd
  
  # Plot the normal density
  xValues <- seq(limLow, limHigh, 0.01)
  yValues <- dnorm(xValues, mean, sd)
  
  plot(xValues, yValues,
       type="l",
       lwd=2,
       col="red",
       ann=FALSE,
       yaxt="n")
  abline(v=mean)
  
  title(paste("Binomial & Normal Distributions",
              "with Mean =", round(mean, 2),
              "and SD =", round(sd, 2)))
}

# Overlay both the plots on one pane
plotAll <- function(n, p) {
  plotNormal(n, p)
  par(new=TRUE)
  plotBinomial(n, p)
}

# Limits for the sliders
nMin <- 20; nMax <- 2000
pMin <- 0.05; pMax <- 0.95

# Starting parameters for the Binomial
nVal <- nMin
pVal <- pMin

# Interact with the control and see
# how the points (Binomial) and the 
# density (Normal) approach one another
manipulate(
  plotAll(nVal, pVal),
  
  nVal = slider(nMin, nMax,
                 step=5,
                 initial=nMin),
  pVal = slider(pMin, pMax,
                step=0.05,
                initial=pMin)
)
