#################################################################
# Simulate a Poisson Distribution for bounced checks 
# arriving at a bank. Also simulate the companion 
# Exponential distribution
# 
# BONUS: We learn how to include Greek symbols in titles
#
# Shankar Venkatagiri
#################################################################
# Install this package for skew calculations
library(moments) 

layout(matrix(c(1, 2, 3, 3), nrow=2))

# Parameters for the Poisson distribution
lam <- 8
numDays <- 1000

# Simulate a Poisson variable
genPois <- rpois(numDays, lam)

# Print out the SKEW in the generated numbers
cat("*********************************************************\n")
cat("Mean in the generated Poisson numbers is ",
    round(mean(genPois), 3), "\n")
cat("Median in the generated Poisson numbers is ",
    round(median(genPois), 3), "\n")
cat("SKEW in the generated Poisson numbers is ",
    round(skewness(genPois), 3), "\n")
cat("*********************************************************\n")

mp <- hist(genPois,
           # Check out how to get Greek symbols in your title!
           main=substitute(paste("Simulated Poisson with ",
                                 lambda, "=", 8)),
           breaks=c(0:20),
           right=FALSE,
           col=colors()[100:120],
           xlab="Bad checks in a day",
           xlim=c(0,20),
           ylim=c(0,150),
           xaxt="n",
           freq=TRUE)

# Prettified centre-justified axis
axis(side=1,
     labels=as.character(0:19),
     pos=0,
     at=mp$mids)

predPois <- round(dpois(c(0:19), lambda=lam)*numDays, 0)

bar <- barplot(predPois,
     main=substitute(paste("Predicted Poisson with ",
                           lambda, "=", 8)),
     col=colors()[100:120],
     space=0,
     xlab="Bad checks in a day",
     ylab="Frequency",
     xlim=c(0,19), ylim=c(0,160))

# Prettified centre-justified axis
axis(side=1,
     labels=as.character(0:19),
     pos=0,
     at=c(0:19))

# Simulate the corresponsing Exponential variable
genExp <- rexp(numDays, lam)

mq <- hist(genExp,
           main=substitute(paste("Exponential with ", 
                                 lambda, "=", 8)),
           labels=TRUE,
           right=FALSE,
           breaks=seq(0, 1.5, 0.1),
           col=colors()[35:45],
           xlab="Waiting time for next bounced check",
           xlim=c(0,1),
           ylim=c(0,700))

par(new=TRUE)

# Turn our attention to the Exponential distribution

# A set of x-values for the plot
x <- seq(0, 1, 0.01)

# Generate the Exponential distribution for the parameters
expVals <- dexp(x, lam)

plot(expVals, 
     type="l",
     lwd=2,
     col="navy",
     xaxt="n", yaxt="n",
     ann=FALSE)

cat("Mean of companion Exponential is ", 
          round(1/lam, 3), "\n")
cat("Mean of generated Exponential is ", 
          round(mean(genExp), 3), "\n")
cat("Median of generated exp is ", 
          round(median(genExp), 3), "\n")
cat(paste("SKEW of generated exp is ", 
          round(skewness(genExp), 3), "\n"))
cat("*********************************************************\n")


