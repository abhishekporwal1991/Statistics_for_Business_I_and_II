#################################################
# R Tutorial 
# Filename: 6-Import.R
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
MonthlyCloses <- read.csv("MonthlyCloses.csv", 
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
MonthlyCloses$Month <- sapply(MonthlyCloses$Month,
                            function(month) 
                              paste("01-",month, sep=""))

MonthlyCloses$Month <- as.Date(MonthlyCloses$Month,
                             "%d-%b-%y")

View(MonthlyCloses)
