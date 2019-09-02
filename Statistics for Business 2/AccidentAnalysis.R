##############################################################
# Analysis of Poisson & Exponential variables
#
# Shankar Venkatagiri
##############################################################

# Libraries to be installed
library(lubridate)
library(moments)

###################################################
# A function to check if a supplied date is valid
# For instance 31st of Feb is not valid
isValid <- function(Day, Month, Year) {
  # cat("Day:", Day, "Month:", Month, "Year:", Year, "\n")
  
  valid <- TRUE
  
  Day <- as.numeric(Day)
  Month <- as.numeric(Month)
  Year <- as.numeric(Year)
  
  LeapYear <- (Year %% 4 == 0)
  
  if(Month == 2) {
    if(LeapYear) {
      if(Day > 29) valid <- FALSE
    } else if(Day > 28) valid <- FALSE
  }
  
  if(Month %in% c(4, 6, 9, 11)) {
    if(Day > 30) {
      valid <- FALSE
    }
  }
  return(valid)
}

# Read in the Accidents file just once
# The processing of this file takes a
# long time - we avoid doing this the 
# subsequent times by only performing
# these steps if the Accidents variable
# does NOT exist
if(!exists("Accidents")) {
  Accidents <- read.csv("Accidents0514.csv",
                        header=TRUE)
  
  # Remove columns we don't need now
  Accidents <- Accidents[, c(9, 10, 11, 12)]
  Accidents$Date <- as.Date(Accidents$Date,
                            "%d/%m/%Y")
  
  # Extract elements & create new columns
  Accidents$Year <- year(Accidents$Date)
  
  # Restrict attention to years <= 2013
  Accidents <- subset(Accidents, 
                      Year <= 2013)
  
  # Grab the different time components for 
  # an accident
  Accidents$Month <- month(Accidents$Date)
  Accidents$Day <- day(Accidents$Date)
  Accidents$Hour <- sapply(Accidents$Time,
                           function(x) {
                             as.numeric(strsplit(as.character(x), 
                                                 ":")[[1]][1])
                           })
  
  # Aggregate accidents by year
  AccidentsByYear <- table(Accidents$Year)
  AccidentsByYear <- as.data.frame(AccidentsByYear)
}

AccRatio = c()
for(i in 2:9){
  AccRatio[i-1] = 100*(AccidentsByYear[i,2] - 
    AccidentsByYear[i-1,2])/AccidentsByYear[i-1,2]
}


# Visualise accidents by year
barplot(AccidentsByYear$Freq,
        names.arg=c(2005:2013),
        col=colors()[25:35],
        ylim=c(0, 200000),
        main="Total Reported Accidents by Year")

# Driving patterns are fairly uniform
# in the interval 2011 - 2013
RecentAccidents <- subset(Accidents, Year >= 2011)
RecentAccidents <- RecentAccidents[order(RecentAccidents$Year,
                                         RecentAccidents$Month, 
                                         RecentAccidents$Day), ]

# Tabulate accidents by day
AccidentsByDay <- xtabs( ~ Day + Month + Year,
                     data=RecentAccidents)
AccidentsByDay <- as.data.frame(AccidentsByDay)

# Eliminate "illegal" days like 30th Feb
validDay <- c()
numAccidents <- nrow(AccidentsByDay)
for(i in 1:numAccidents) {
  validDay[i] = isValid(AccidentsByDay$Day[i],
                        AccidentsByDay$Month[i],
                        AccidentsByDay$Year[i])
}

AccidentsByDay$Valid <- validDay
View(AccidentsByDay)
AccidentsByDay <- subset(AccidentsByDay,
                         Valid == TRUE)

cat("Parameters of counts BEFORE filtering\n")
dailyAccidentCounts <- AccidentsByDay$Freq
aveDailyAccidents <- mean(dailyAccidentCounts)
varDailyAccidents <- var(dailyAccidentCounts)

cat("********************************************\n")
cat("Mean Daily Accidents:", aveDailyAccidents, 
    "\nVariance in Daily Accidents:", varDailyAccidents,
    "\n")
cat("********************************************\n")

# Pick a reasonably large number of bad accidents
# Like the 99.5th percentile
OutlierNumber <- quantile(RecentAccidents$Number_of_Casualties, 
                          0.995)
cat("Selecting only those accidents with an\n",
    "outlying", OutlierNumber, "casualties...\n")

# We only consider accidents with a 
# large number of casualties, as decided 
# by the percentile above
BadAccidents <- subset(RecentAccidents,
                       Number_of_Casualties >= OutlierNumber)

BadAccidents <- BadAccidents[order(BadAccidents$Year,
                                   BadAccidents$Month, 
                                   BadAccidents$Day, 
                                   BadAccidents$Hour), ]

# The companion to a Poisson variable is the
# corresponding Exponential variable, which 
# models waiting times (for the next bad
# accident, in this case)
# So we examine the time elapsed between
# successive bad accidents
hrsToNextAccident <- c()
numAccidents <- nrow(BadAccidents)

for(i in 2:numAccidents) {
  nextDay <- BadAccidents$Day[i]
  currDay <- BadAccidents$Day[i - 1]
  
  nextHr <- BadAccidents$Hour[i]  
  currHr <- BadAccidents$Hour[i - 1]
  
  elapsedHours <- (nextDay - currDay)*24 + nextHr - currHr
  if(elapsedHours < 0) {
    # Last day of the month!
    elapsedHours <- 24 + nextHr - currHr
  }
  hrsToNextAccident[i - 1] <- elapsedHours
}

hrsToNextAccident[numAccidents] <- mean(hrsToNextAccident)
BadAccidents$DaysToNextAccident <- hrsToNextAccident/24

BadAccidentsByDay <- xtabs( ~ Day + Month + Year,
                      data=BadAccidents)
BadAccidentsByDay <- as.data.frame(BadAccidentsByDay)

# Eliminate "illegal" days like 30th Feb
validDay <- c()
numAccidents <- nrow(BadAccidentsByDay)
for(i in 1:numAccidents) {
  validDay[i] = isValid(BadAccidentsByDay$Day[i],
                        BadAccidentsByDay$Month[i],
                        BadAccidentsByDay$Year[i])
}

BadAccidentsByDay$Valid <- validDay
View(BadAccidentsByDay)
BadAccidentsByDay <- subset(BadAccidentsByDay,
                            Valid == TRUE)


cat("Parameters of counts AFTER filtering\n")
dailyAccidentCounts <- BadAccidentsByDay$Freq
aveDailyAccidents <- mean(dailyAccidentCounts)
varDailyAccidents <- var(dailyAccidentCounts)

cat("Selected", nrow(BadAccidents), 
    "BAD accidents...\n")

cat("********************************************\n")
cat("Distribution of Daily Counts for Bad Accidents\n")
cat("Mean: ", round(aveDailyAccidents, 2), "\n")
cat("Variance: ", round(varDailyAccidents, 2), "\n")
cat("********************************************\n")

par(mfrow=c(1, 2))
observedCounts <- table(BadAccidentsByDay$Freq)
observedCounts <- as.data.frame(observedCounts)
observedCounts <- subset(observedCounts, Freq > 1)

barplot(observedCounts$Freq,
        main="Bad Accidents in a Day (Observed)",
        names.arg=c(0:10),
        col=colors()[30:40])

# dpois gives the Poisson probability
predictedCounts <- dpois(c(0:10),
                         lambda=aveDailyAccidents) * 
  nrow(BadAccidentsByDay)

barplot(predictedCounts,
        main="Bad Accidents in a Day (Predicted)",
        names.arg=c(0:10),
        col=colors()[30:40])

# We compare the counts of bad accidents
# predicted by the Poisson variable with
# what is observable from the data
par(mfrow=c(1,1))
hist(BadAccidents$DaysToNextAccident,
     main="Distribution of Days to Next Bad Accident",
     breaks=40,
     xlim=c(0,1.2),
     xlab="No. of Days",
     col=colors()[20:50])

xVals <- seq(0, 1.2, 0.01)
par(new=TRUE)
plot(xVals,
     dexp(xVals, aveDailyAccidents),
     type="l", lwd=2,
     xlab="No. of Days",
     col="red",
     xlim=c(0,1.2),
     ann=FALSE,
     axes=FALSE)

# Ideal distributions
par(mfrow=c(1, 2))

numBadAccidents <- nrow(BadAccidents)
predictedCounts <- dpois(c(0:10),
                         lambda=aveDailyAccidents) * 
  numBadAccidents
barplot(predictedCounts,
        main="Bad Accidents in a Day (Predicted)",
        names.arg=c(0:10),
        col=colors()[30:40])

hist(rexp(numBadAccidents, aveDailyAccidents),
     col=colors()[30:40],
     main="Waiting time for Next Bad Accident",
     xlab="No. of Days")
par(new=TRUE)

xVals <- seq(0, 1.2, 0.01)
expVals <- dexp(xVals, aveDailyAccidents)
plot(xVals,
     expVals,
     type="l", lwd=2,
     col="navy",
     ann=FALSE,
     axes=FALSE)

x = c(0.5,0.75)
pexp(x)
pexp(0.75, rate = 1/4)
pexp(0.50, rate = 1/4)
pexp(0.75, rate = 1/3.81) - pexp(0.50, rate = 1/3.81)
pexp(1, rate = 1/3.81) - pexp(0.75, rate = 1/3.81)
