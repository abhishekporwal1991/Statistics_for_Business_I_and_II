############################################################
# Simulation for a discrete random variable
#
# Shankar Venkatagiri
############################################################
# Change these settings if you need to play with the program!

# Variable assumes values between 0 and maxVal
maxVal <- 5

# How many values to simulate for each random variable?
numRows <- 500

##########################################################
# A function to calculate the parameters of the distribution
# The structure of the distribution variable is a frame
# containing combinations of (value, pdf, cdf)
getParameters <- function(distribution) {
  
  meanDist <- t(distribution[ , 1]) %*% distribution[ , 2]
  varDist <-  t(distribution[ , 1])^2 %*% (distribution[ , 2]) - meanDist^2
  
  return(c(meanDist, varDist))
}

##########################################################
# A function that generated a discrete random variable
# and returns the distribution, namely (values, pdf, cdf)
# The values taken by the RV are in 0, 1, 2, . . ., max
createRV <- function(max) {
    
  # Declare a frame to hold the values and the distribution
  distribution <- data.frame(matrix(nrow=max + 1, ncol=3))
  colnames(distribution) <- c("value", "pdf", "cdf")
  
  # Generate a random set of numbers between 0 and 1
  probs <- runif(max + 1)
  
  # Normalise them to the PDF, taking relative frequencies
  pdfs <- probs/sum(probs)
  
  # Now construct the CDF
  cdfs <- c()
  cdf <- 0
  
  for(i in 1: (max + 1)) {
    cdf <- cdf + pdfs[i]
    cdfs[i] <- cdf
  }
  
  # Compose the distribution as (Values, PDF, CDF)
  distribution$value <- c(0:max)
  distribution$pdf <- round(pdfs, 2)
  distribution$cdf <- round(cdfs, 2)
  
  # Return the distribution
  return(distribution)
}

##########################################################
# Function that profiles a discrete distribution
#
profileDist <- function(dist, varName) {
  
  # First plot the PDF as dots, with a dynamic title
  maxDist <- max(dist$pdf)
  
  plot(dist$value, dist$pdf,
       main=paste("Profile for", varName),
       xlab="Value assumed",
       ylab="pdf",
       ylim=c(0, maxDist*1.2),
       pch=20, cex=0.7, col="darkgreen")
  
  maxValue <- max(dist$value)
  
  # Draw line segments to the dots, and label them
  for(i in 1:nrow(dist)) {
    segments(dist$value[i], 0,
             dist$value[i], dist$pdf[i],
             col="red")
    text(dist$value[i], dist$pdf[i] * 1.1,
         round(dist$pdf[i], 2))
  }
  
  # Want the parameters for the distribution, with 
  params <- getParameters(dist)
  meanDist <- round(params[1], 2)
  varDist <- round(params[2], 2)
  sdDist <- round(sqrt(varDist), 2)
  
  # Use the bquote() function to display Greek symbols
  # with a .(varName) if you need the actual value
  text(2, maxDist*1.2, # x & y coordinates for the text
       bquote(paste(mu, "=", .(meanDist), "\n",
                    sigma, "=", .(sdDist))))
}

##########################################################
# For a given distribution, this function figures out 
# into which bucket an input x value in [0, 1] will fall,
# based on the cdf of the dist
# This is essentially our Recoding function!
whichValue <- function(dist, x) {
  
  values <- dist[ , 1]
  cdf <- dist[ , 3]
  
  index <- 1
  while(x > cdf[index]) index <- index + 1
  
  return(values[index])
}

##########################################################
# So far, we've declared some functions. Now use them!
##########################################################

# Create our "base" random variable
baseVar <- createRV(max=maxVal)

# Parameters for the distribution
params <- getParameters(baseVar)
meanDist <- round(params[1], 2)
varDist <- round(params[2], 2)

##########################################################
# Q: How do we simulate a random variable? 
#
# Answer: Starting with a uniformly distributed dataset, 
# "map" each value to the appropriate bucket in the CDF 
# of the corresponding random variable X
#
# You will get a bunch of values assumed by X, whose 
# probabilities will broadly agree with the PDF of X
##########################################################

# This is best understood by bringing up & contrasting
# the two data frames unifValues & mappedRVValues
# after you Source the script, from the Environment

# Frame to store what the above values map to, for each Xi
mappedRVValues <- data.frame(matrix(nrow=numRows, 
                                    ncol=1))
colnames(mappedRVValues) <- c("X")

for(i in 1:numRows) {
  # Now map it, and register it as a value assumed by X
  mappedRVValues[i, 1] <- whichValue(baseVar, runif(1))
}
    

##########################################################
# Time to start showing what you've built up
par(mfrow=c(1, 2))

# Begin by profiling the base discrete distribution
profileDist(baseVar, "X")

meanMapped <- round(mean(mappedRVValues$X), 2)
sdMapped <- round(sd(mappedRVValues$X), 2)

# Profile the Sum X1 + . . . + Xn
histRV <- hist(mappedRVValues$X,
               breaks=20,
               xlab="x",
               col=colors()[20:45],
               main=paste(numRows, "Simulations of X"))

# Use the bquote() function to display Greek symbols
# with a .(varName) if you need the actual value
text(2, max(histRV$counts), # x & y coordinates for the text
     bquote(paste(mu, "=", .(meanMapped), "\n",
                  sigma, "=", .(sdMapped))))


