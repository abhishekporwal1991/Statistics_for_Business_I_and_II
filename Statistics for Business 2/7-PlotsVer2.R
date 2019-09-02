#################################################
# R Tutorial 
# Filename: 7-PlotsVer2.R
# Shankar Venkatagiri
#################################################

# Date variables can be hard, so we convert them
# into something more useful

# Install a package called lubridate via the
# Packages tab -  should take very little time
library(lubridate)

# Next, import the data from a file. 

# If you encounter an error message, 
# then you must set the Working Directory.
# You could use the Files tab to navigate 
# to the file MonthlyCloses.csv
# and select More > Set working directory

# Read the closing prices into a variable
MonthlyCloses <- read.csv("MonthlyClosesVer2.csv", 
                          header=TRUE)

# Dimensions of the dataset
numDays <- nrow(MonthlyCloses)
numStocks <- ncol(MonthlyCloses) - 1

# Because the data is in mixed format - the 
# first column is a Date and the rest are
# Prices, we make sure that R knows about
# their intended formats

# Explicitly declare the formats of the columns
# Month field is alpha
MonthlyCloses[ , 1] <- as.character(MonthlyCloses[ , 1])

# Prices are numeric
for(i in 2:numStocks + 1) {
  MonthlyCloses[ , i] <- as.numeric(MonthlyCloses[ , i])
}

# Prepare the Month field, so it can be converted
# to a Date field. You don't have to understand
# everything here - all that the command is 
# achieving is that the Date column is set to
# a format recognisable by R
# MonthlyCloses$Month <- sapply(MonthlyCloses$Month,
#                               function(month) 
#                                 paste("01-",month, sep=""))

MonthlyCloses$Month <- as.Date(MonthlyCloses$Month,
                               "%d/%m/%y")

# If you encounter an error message, 
# then you must set the Working Directory.
# You could use the Files tab to navigate 
# to the file MonthlyCloses.csv
# and select More > Set working directory

# Let's attach a TradingMonth column to our dataset
MonthlyCloses$TradingMonth <- months(MonthlyCloses$Month)

# Obtain month-wise averages by grouping closed
# across the years
monthlyAverage <- aggregate(Software1 ~ TradingMonth,
                            MonthlyCloses,
                            mean) 

monthlyAverage[which.max(monthlyAverage$Software1),1]
monthlyAverage[which.min(monthlyAverage$Software1),1]

# month-wise median 
monthlyMedian <- aggregate(Software1 ~ TradingMonth,
                            MonthlyCloses,
                            median) 

monthlyMedian[which.max(monthlyMedian$Software1),1]
monthlyMedian[which.min(monthlyMedian$Software1),1]

# You may change the mean to 
# some other centrality measure

# Place two plots side by side
par(mfrow=c(1,2))

# This gives us a raw barplot
barplot(monthlyAverage$Software1,
        names.arg=monthlyAverage$TradingMonth,
        main="Month-wise Average for Software1",
        col=colors()[30:41])

# Here's a pie chart for the same
pie(monthlyAverage$Software1,
    labels=monthlyAverage$TradingMonth,
    main="Month-wise Average for Software1",
    col=colors()[30:41])

# A task you could undertake is to 
# sort these values by the Month

# Scatter plot of two stocks in the
# Software sector
with(MonthlyCloses,
     plot(Software1, Software2,
          main="Tale of two Software stocks",
          xlab="Price of Software1 Stock", 
          ylab="Price of Software2 Stock", 
          pch=20, cex=0.7, col="darkgreen")
)

# Next, get a line chart for both
# Determine the range for the y axis
maxVal <- max(max(MonthlyCloses$Software1),
              max(MonthlyCloses$Software2))
minVal <- min(min(MonthlyCloses$Software1),
              min(MonthlyCloses$Software2))

plot(MonthlyCloses$Software1 ~ 
       MonthlyCloses$Month,
     type="l",
     lwd=2,
     col="navy",
     xlab="Time", 
     ylab="Monthly Close Price",
     main="Two Software Stocks",
     ylim=c(0, maxVal+100))

# This is how we orchestrate an
# overlap of the second plot
par(new=TRUE)

plot(MonthlyCloses$Software2 ~ 
       MonthlyCloses$Month,
     type="l",
     lwd=2,
     ann=FALSE,
     col="brown",
     ylim=c(0, maxVal+100))

# Box plot
par(mfrow=c(1,1))

boxplot(MonthlyCloses$Housing,
        horizontal=TRUE,
        main="Housing stock movement",
        col="orange")
        
# Uncomment these lines to get the
# full picture for every stock

# # Plot the time series for each stock
# for(symbol in 2:numStocks + 1) {
#   
#   # The ts function of R helps us to
#   # construct a time series
#   plot(ts(MonthlyCloses[ , symbol],
#           start=c(1995, 1), end=c(2014, 12),
#           frequency=12),
#        main=paste("Monthly Movements for:", 
#                   colnames(MonthlyCloses)[symbol]),
#        xlab="Year", ylab="Stock Price in INR",
#        col="navy")
#   
# }
# 
# 
# 
# 
