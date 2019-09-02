#################################################
# R Tutorial 
# Filename: 8-Function.R
# Shankar Venkatagiri
#################################################

# Here's how you write a function of your own
# The function's graph is a pair (x, f(x))

#########################
# Simple polynomial
funx <- function(x) {
  return (x^3 + 2*x^2 + sin(x) - 1)
}

# x coordinate for the pair
xvals <- seq(-3, 2, by=0.001)

# Plot the function
plot(xvals, funx(xvals),
     main="Function plotting",
     type="l", lwd=2, 
     col="darkred",
     xlab="x", ylab="f(x)")

# Axes
abline(v=0)
abline(h=0)


funLog = function(x) {
  return(log10(x))
}

xvalues = seq(0,100, by = 0.01)
plot(xvalues,funLog(xvalues),
     type = "l", lwd = "2",
     xlab = "x", ylab = "f(x)" )

funLogDiff = function(x) {
  return(log10(x))
}

LogDiff = 0

for(i in 2:10) {
  LogDiff[i-1] = funLogDiff(i) - funLogDiff(i-1)
}

LogDiff
plot(LogDiff)
