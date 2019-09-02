#################################################
# R Tutorial 
# Filename: 5-Values.R
# Shankar Venkatagiri
#################################################

# To know about the mtcars dataset,
# type ?mtcars at the Console

# We shall regress miles/gallon as 
# against the weight
lmfit <- lm(mpg ~ wt, 
            data=mtcars) 

# Print out the summary
print(summary(lmfit), digits=3)

# Visualise the regression
plot(mtcars$wt, mtcars$mpg,
     xlab="Weight of car ('000 lbs)",
     ylab="Mileage (mpg)",
     pch=20, cex=0.7, col="navy",
     ylim=c(0,40))

title("Regression Plot")

# Overlay the regression line
abline(lmfit, col="red", lwd=2)

# Overlay the fitted vales in red
points(mtcars$wt, lmfit$fitted.values,
       pch=20, col="darkgreen")
