getwd()

Census = read.csv("CensusIncomeForR.csv", header = TRUE)

Sample100 = Census[1:100,]
Sample400 = Census[1:400,]

par(mfrow = c(1,3))

lbls = c("Female", "Male")
pie(table(Sample100$Sex), labels = lbls )
pie(table(Sample400$Sex), labels = lbls )
pie(table(Census$Sex), labels = lbls )

title(main = "Comparison b/w Datasets" )

sample(100,10)
runif(10,1,100)

par(mfrow = c(1,3))
hist(runif(100,1,10), breaks = 20, col = colors()[30:50], xlab = "Number")
hist(runif(1000,1,10), breaks = 20, col = colors()[30:50], xlab = "Number")
hist(runif(10000,1,10), breaks = 20, col = colors()[30:50], xlab = "Number")
hist(runif(100000,1,10), breaks = 20, col = colors()[30:50], xlab = "Number")
