#################################################
# R Tutorial 
# Filename: 1-Data.R
# Shankar Venkatagiri
#################################################

# This is how you declare a set of numbers
# In this case, from 1 to 10. 
position <- c(1:10)

# Explicitly declare the first 10 primes
primes <- c(2,3,5,7,11,13,17,19,23,29)

# Run some stats on these primes
myMean <- mean(primes)
cat("The mean of primes is", myMean, 
    "\n") # This is an end-of-line character

mySD <- sd(primes)
cat("The SD of primes is", 
    round(mySD, 2), "\n")

# Even get a correlation between the rank order of 
# the prime and its magnitude
cor_np <- cor(position,primes)
cat("Correlation of position vs primes is", 
    round(cor_np, 2), "\n")

# Here's a rudimentary plot
plot(position,primes,
     pch=20,
     col=colors()[30:40],
     main="Colourful Primes")

# Or, how about a regression fit 
# through the primes? Don't worry
# if you don't figure this out now!
lmFit <- lm(primes ~ position)
abline(lmFit,
       col="red")
print(summary(lmFit))
  
  
  