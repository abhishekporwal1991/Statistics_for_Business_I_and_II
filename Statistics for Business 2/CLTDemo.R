############################################################
# Demonstration of the Central Limit Theorem (CLT)
# Shankar Venkatagiri
#
# README 
#    1. First, there are a few functions, which provide
#       all kinds of assistance: creating a random variable,
#       figuring out its parameters, profiling it, etc.
#    2. We then create multiple random variables, simulate
#       a large set of values for each RV, store them in a
#       data frame, and sum them up across the rows.
#    3. The profile of these sums is compared with what is 
#       predicted by the CLT. The proof is in the pudding!
#
############################################################
library(car) # For normality checks

# Change these settings if you need to play with the program!

# We call our variables X1, . . ., Xn, where n = numVars
numVars <- 30

# Each Xi is discrete, assuming values between 0 and maxVal
maxVal <- 10

# How many values to simulate for each random variable?
# We are going to store them in a 2-D frame
numRows <- 1000

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
  distribution$pdf <- pdfs
  distribution$cdf <- cdfs
  
  # Return the distribution
  return(distribution)
}

##########################################################
# For a given distribution, this function "jitters" the
# probabilities and returns a perturbed distribution
perturbDist <- function(randVar) {
    
  # Return distribution
  retDist <- randVar
  lastRow <- nrow(retDist)
  
  # First, jitter (perturb) all the probabilities
  # Those near 0 can swing to negative, so take abs
  retDist$pdf <- abs(jitter(retDist$pdf, amount=0.2))
  
  # Compute with CDF values with the jittered distribution
  cdf <- 0
  
  for(i in 1:lastRow) {
    cdf <- cdf + retDist$pdf[i]
    
    # Correct the distribution if necessary
    if(cdf > 1) {
      cdf <- 1.0
      retDist$pdf[i] <- 1 - retDist$cdf[i - 1]
      retDist$cdf[i] <- cdf
    }
    else {
      retDist$cdf[i] <- cdf
    }
    
    # Check for all PDFs totalling to less than 1
    if(cdf < 1) {
      retDist$pdf[lastRow] <- 1 - retDist$cdf[lastRow - 1]
      retDist$cdf[lastRow] <- 1.0 
    }
    
  }
  
  return(retDist)
}

##########################################################
# Function that profiles a discrete distribution
#
profileDist <- function(dist, varName) {
  
  # First plot the PDF as dots, with a dynamic title
  title <- bquote(expression(X[.(varName)]))
  
  plot(dist$value, dist$pdf,
       main=eval(title),
       xlab="Value assumed",
       ylab="pdf",
       ylim=c(0, 1),
       pch=20, cex=0.7, col="darkgreen")
  
  maxValue <- max(dist$value)
  
  # Draw line segments to the dots, and label them
  for(i in 1:nrow(dist)) {
    segments(dist$value[i], 0,
             dist$value[i], dist$pdf[i],
             col="red")
    text(dist$value[i], dist$pdf[i] + 0.1,
         round(dist$pdf[i], 2))
  }
  
  # Want the parameters for the distribution, with 
  params <- getParameters(dist)
  meanDist <- round(params[1], 2)
  varDist <- round(params[2], 2) 
  sdDist <- round(sqrt(varDist), 2)
  
  # Use the bquote() function to display Greek symbols
  # with a .(varName) if you need the actual value
  text(maxValue - 2, 0.9, # x & y coordinates for the text
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

# Create a "base" random variable, which we will perturb
# to get several other variables X1, . . . , Xn
baseVar <- createRV(max=maxVal)

# Parameters for the distribution
params <- getParameters(baseVar)
meanDist <- round(params[1], 2)
varDist <- round(params[2], 2)

# We store the random variables we create in a list
randVars <- list()


# We generate variables of comparable variances
toleranceMean <- 0.1 * meanDist
toleranceVar <- 0.1 * varDist
i <- 0
while(i <= numVars) {
  newVar <- perturbDist(baseVar)
  paramsVar <- getParameters(newVar)
  if(abs(paramsVar[1] - meanDist) < toleranceMean &
       abs(paramsVar[2] - varDist) < toleranceVar) {
    i <- i + 1
    randVars[[i]] <- newVar
  }
}

##########################################################
# Q: How do we simulate a random variable? 
#
# Answer: Starting with a uniformly distributed dataset, 
# "map" each value to the appropriate bucket in the CDF 
# of the corresponding random variable Xi.
#
# You will get a bunch of values assumed by Xi, whose 
# probabilities will broadly agree with the PDF of Xi
##########################################################

# This is best understood by bringing up & contrasting
# the two data frames unifValues & mappedRVValues
# after you Source the script, from the Environment

# Frame to store randomly generated values
unifValues <- data.frame(matrix(nrow=numRows, 
                                ncol=numVars))
colnames(unifValues) <- paste("Rand", sep="", c(1:numVars))

# Frame to store what the above values map to, for each Xi
mappedRVValues <- data.frame(matrix(nrow=numRows, 
                                    ncol=numVars))
colnames(mappedRVValues) <- paste("X", sep="", c(1:numVars))

for(i in 1:numRows) {
  for(j in 1:numVars) {
    
    # Generate a random number
    randVal <- runif(1)
    unifValues[i, j] <- randVal
    
    # Now map it, and register it as a value assumed by Xj
    mappedRVValues[i, j] <- whichValue(randVars[[j]], 
                                       randVal)
  }  
}

# Create a new column to store the sum X1 + . . . + Xn
mappedRVValues$Sum <- rowSums(mappedRVValues)

##########################################################
# Time to start showing what you've built up
par(mfrow=c(2, 2))

# Begin by profiling the base discrete distribution
profileDist(baseVar, "Base")

# Profile the Sum X1 + . . . + Xn
hist(mappedRVValues$Sum,
     breaks=20,
     xlab="Sum",
     col=colors()[30:45],
     main=paste("Sum of", numVars, "Variables"))

# Let's contrast the observed versus expected densities
# for the sum X1 + . . . + Xn
minSum <- min(mappedRVValues$Sum)
maxSum <- max(mappedRVValues$Sum)
meanSum <- mean(mappedRVValues$Sum)
sdSum <- sd(mappedRVValues$Sum)

# Profile the density of the sum
plot(density(mappedRVValues$Sum),
     main="CLT Prediction",
     lwd=2,
     xlim=c(minSum, maxSum),
     col="navy")

# This is the piece de resistance of the CLT result: that the
# sum X1 + . . . + Xn shall converge to  a normal distribution

# The range for this normal should be that for the Sum
# This command generates a sequence of x values for which the
# normal density can be calculated: in other words, an interval!
xVals <- seq(minSum, maxSum, by=0.01)

# Calculate the (expected) normal density for the interval
# which is predicted by CLT to be ~ N(meanSum, sdSum)
expectedDensity <- dnorm(xVals,
                         mean=meanSum, sd=sdSum)

# Let's have a legend
legend(minSum + 0.66*(maxSum - minSum), max(expectedDensity),
       c("Observed", "Expected"),
       lty=1,
       lwd=c(2, 2),
       col=c("navy", "red"),
       y.intersp=0.2,
       bty="n")

# Now overlay the expected density 
par(new=TRUE)
plot(xVals, expectedDensity,
     ann=FALSE,
     axes=FALSE,
     type="l",
     lwd=2,
     xlim=c(minSum, maxSum),
     col="red")

# Check with a quantile plot for any deviations
qqPlot(mappedRVValues$Sum,
       main="Q-Q Plot for the Sum",
       pch=20, cex=0.7, col="navy")

# Finally, profile EACH of the constituent variables Xi
for(i in 1:numVars) {
  profileDist(randVars[[i]], i)
}

