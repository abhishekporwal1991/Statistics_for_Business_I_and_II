getwd()
Flights = read.csv("Dec2014Flights.csv", header = TRUE)
str(Flights)

# Declare variables as categorical
Flights$FL_NUM <- factor(Flights$FL_NUM)
Flights$CANCELLED <- factor(Flights$CANCELLED)
Flights$DIVERTED <- factor(Flights$DIVERTED)

# Declare each of these variables as numerical
Flights$CRS_DEP_TIME <- as.numeric(Flights$CRS_DEP_TIME)
Flights$DEP_DELAY <- as.numeric(Flights$DEP_DELAY)
Flights$CRS_ARR_TIME <- as.numeric(Flights$CRS_ARR_TIME)
Flights$ARR_DELAY <- as.numeric(Flights$ARR_DELAY)
Flights$DISTANCE <- Flights$DISTANCE


Cancelled = subset(Flights, CANCELLED == 1)
Diverted  = subset(Flights, DIVERTED  == 1)

hist(Diverted$DISTANCE, breaks = 20, col = colors()[30:50])
hist(Cancelled$DISTANCE, breaks = 20, col = colors()[30:50])

par(mfrow = c(2,1))
boxplot(Diverted$DISTANCE, col = "orange", horizontal = TRUE,
        xlab = "Distances in miles (Diverted filghts)")
title("Distribution of Diverted flights distance travelled")

boxplot(Cancelled$DISTANCE, col = "orange", horizontal = TRUE,
        xlab = "Distances in miles (Cancelled filghts)")
title("Distribution of Cancelled flights distance travelled")


summary(Diverted$DISTANCE)
summary(Cancelled$DISTANCE)

FlightsByDOW = table(Flights$DAY_OF_WEEK)
print(FlightsByDOW)

table(Flights$DAY_OF_WEEK, Flights$CANCELLED == 1)
table(Flights$DAY_OF_WEEK, Flights$DIVERTED == 1)

FlightsByCarrierByDay = xtabs(~CARRIER + DAY_OF_WEEK, data = Flights)

aggregate(Flights$DISTANCE, by = list(Flights$CARRIER), FUN = median)
