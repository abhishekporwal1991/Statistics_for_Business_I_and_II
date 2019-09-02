library(manipulate)
getwd()
stocks = read.csv("FourStocks.csv", header = TRUE)
str(stocks)
View(stocks)

# Column mean
print(colMeans(stocks[,2:5]))
print(apply(stocks[,2:5], MARGIN = 2, FUN = sd))
print(apply(stocks[,2:5], MARGIN = 2, FUN = median))

library(moments)
print(skewness(stocks[,2:5]))

library(lubridate)

stocks$Date = as.Date(stocks$Date, "%d-%b-%y")
stocks$Month = month(stocks$Date)
stocks$Year = year(stocks$Date)

write.csv(stocks,"NewStocks.csv", row.names = FALSE)
cat("Created NewStocks.csv in", getwd(), "\n",
    "you may open it as spreadsheet!\n")

# Creating pivot table in R using aggregate function
monthYearAvePharma = aggregate(Pharma ~ Month + Year, data = stocks,
                               FUN = mean)
monthYearAveFMCG = aggregate(FMCG ~ Month + Year, data = stocks,
                             FUN = mean)
monthYearAvePower = aggregate(Power ~ Month + Year, data = stocks,
                             FUN = mean)
monthYearAveSteel = aggregate(Steel ~ Month + Year, data = stocks,
                             FUN = mean)

summary(monthYearAveSteel)


# Plotting actual vs Aggregate time series plots
par(mfrow = c(1,2))
ts.Pharma = ts(stocks$Pharma)
plot(ts.Pharma,
     main = "Actual prices of Pharma Stock",
     col = "navy",
     ylab = "Pharma (actual)")

ts.PharmaAvg = ts(monthYearAvePharma$Pharma,
                  start = c(1995,1),
                  end = c(2014,12),
                  frequency = 12)
plot(ts.PharmaAvg,
     main = "Aggregate prices of Pharma Stock",
     col = "darkred",
     ylab = "Pharma (Aggregate)")
#----------------------------------------------
ts.FMCG = ts(stocks$FMCG)
plot(ts.FMCG,
     main = "Actual prices of FMCG Stock",
     col = "navy",
     ylab = "FMCG (actual)")

ts.FMCGAvg = ts(monthYearAveFMCG$FMCG,
                  start = c(1995,1),
                  end = c(2014,12),
                  frequency = 12)
plot(ts.FMCGAvg,
     main = "Aggregate prices of FMCG Stock",
     col = "darkred",
     ylab = "FMCG (Aggregate)")
#----------------------------------------------
ts.Power = ts(stocks$Power)
plot(ts.Power,
     main = "Actual prices of Power Stock",
     col = "navy",
     ylab = "Power (actual)")

ts.PowerAvg = ts(monthYearAvePower$Power,
                start = c(1995,1),
                end = c(2014,12),
                frequency = 12)
plot(ts.PowerAvg,
     main = "Aggregate prices of Power Stock",
     col = "darkred",
     ylab = "Power (Aggregate)")
#----------------------------------------------
ts.Steel = ts(stocks$Steel)
plot(ts.Steel,
     main = "Actual prices of Steel Stock",
     col = "navy",
     ylab = "Steel (actual)")

ts.SteelAvg = ts(monthYearAveSteel$Steel,
                 start = c(1995,1),
                 end = c(2014,12),
                 frequency = 12)
plot(ts.SteelAvg,
     main = "Aggregate prices of Steel Stock",
     col = "darkred",
     ylab = "Steel (Aggregate)")
#----------------------------------------------