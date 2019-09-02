##########################################################
# edX Demo: Analysis of Salaries for people working in
#           City Government departments of a large metro
#
# Copyright: Shankar Venkatagiri
##########################################################

# Install these packages before you hit Source
library(car)      # For normality plots
library(moments)  # For skew calculations
library(lattice)  # Use trellis graphics
library(e1071)

# Read in the Salaries dataset
Metro <- read.csv("HRSalaries.csv", header=TRUE)

colnames(Metro) <- c("EmployeeID", 
                     "Title", 
                     "Department",
                     "AnnualSalary",
                     "ReviewScore")

# Let's depict the distribution of the salaries
muPop <- mean(Metro$AnnualSalary)
sdPop <- sd(Metro$AnnualSalary)

hist(Metro$AnnualSalary,
     breaks=25,
     main="",
     xlab="Annual Salary",
     col=colors()[25:40])
abline(v=muPop, 
       col="red", lwd=2)
title(main="Salaries Distribution",
      sub=bquote(paste(mu,"=$", .(muPop), " ",
                       sigma, "=$", .(sdPop))))

# Calculate the skew in salaries
skewSal <- skewness(Metro$AnnualSalary)

# Display the skew on the distribution plot
text(150000, 6000,
     paste("Skew in these salaries =", 
           round(skewSal, 1)))

# par(mai=c(1, 0.3, 0.3, 0.5))
boxplot(Metro$AnnualSalary,
        horizontal=TRUE,
        main="Salaries for the entire Metro",
        pch=16, cex=0.7,
        col="orange")

# Check for normality
# par(mai=c(1.5, 1.5, 1, 0.5))
qqPlot(Metro$AnnualSalary,
       ylab="",
       main="Q-Q Plot for Normality of Salaries",
       las=2,
       pch=16, col="navy", cex=0.7)


# Piece de resistance: A boxplot for 
# each department's salary spread!
par(mfrow=c(1, 1))
par(mai=c(1, 3, 0.3, 0.5))
par(cex.axis=0.75)
boxplot(AnnualSalary ~ Department,
        horizontal=TRUE,
        main="Salaries across Departments",
        las=2,
        range=1.5,
        pch=16, cex=0.7,
        col=colors()[20:40],
        data=Metro)

# Now for the review scores
# Let's depict the distribution of the reviews
muReview <- mean(Metro$ReviewScore)
sdReview <- sd(Metro$ReviewScore)

hist(Metro$ReviewScore,
     breaks=25,
     main="",
     xlab="Review Score",
     col=colors()[25:40])

abline(v=muReview, 
       col="red", lwd=2)
title(main="Review Scores Distribution",
      sub=bquote(paste(mu,"=$", .(muReview), " ",
                       sigma, "=$", .(sdReview))))

# Check for normality
# par(mai=c(1.5, 1.5, 1, 0.5))
qqPlot(Metro$ReviewScore,
       ylab="",
       main="Q-Q Plot for Normality of Scores",
       las=2,
       pch=16, col="navy", cex=0.7)

# Piece de resistance: A boxplot for 
# each department's review score spread!
par(mfrow=c(1, 1))
par(mai=c(1, 3, 0.3, 0.5))
par(cex.axis=0.75)
boxplot(ReviewScore ~ Department,
        horizontal=TRUE,
        main="Review Scores across Departments",
        las=2,
        range=1.5,
        pch=16, cex=0.7,
        col=colors()[20:40],
        data=Metro)

Metro$EmployeeID[which.max(Metro$ReviewScore)]
Metro$EmployeeID[which.max(Metro$AnnualSalary)]
Metro$Title[which.min(Metro$AnnualSalary)]

x = subset(Metro, Metro$ReviewScore >= 5 & Metro$ReviewScore <= 7)
