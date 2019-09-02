economy = mtcars$mpg
cylinders = mtcars$cyl

meanEco = mean(economy)
sdEco   = sd(economy)

plot(economy,cylinders)

print(median(cylinders))
cat(median(cylinders))

boxplot(mtcars$wt, horizontal = TRUE)

sumWights = summary(mtcars$wt)

IQR = sumWights[5] - sumWights[2]
sumWights[2]-1.5*IQR
sumWights[5]+1.5*IQR
table(mtcars$wt > (sumWights[5]+1.5*IQR))
table(mtcars$wt < (sumWights[2]-1.5*IQR))

directory = dirname(file.choose())
setwd(directory)
getwd()


sleep
summary(sleep)
View(sleep)
setwd("D:/Study/Data Science/Statistics for Business 2")
dir.create("SinkEx")
sink("SinkEx/NumPrimes", append = TRUE)
pdf("SinkEx/Graphs.pdf")
pdf("sinkEx/Graphs.pdf")
pdf("sinkEx/Graph.pdf")
source("1-Data.R")
sink()
dev.off

library(e1071)
kurt = kurtosis(mtcars$mpg)
print(kurt)
View(mtcars)
plot(mtcars$mpg)
hist(mtcars$mpg, col = colors()[100:130])
mean(mtcars$wt)
hist(mtcars$wt)
skewness(mtcars$wt)

par(mfrow =c(1,1))
faithful


lmfit = lm(eruptions ~ waiting, data = faithful)
plot(faithful$waiting, faithful$eruptions)
abline(lmfit, col = "red")
summary(lmfit)



#Generating Boxplots for a set of random numbers
#Week 2; CL 2.2
##############################
par(bg="wheat")
n <- 15
g <- gl(n, 100, n*100)
x <- rnorm(n*100) + sqrt(as.numeric(g))
boxplot(split(x,g), col="orange", notch = TRUE)
title(main="Notched Boxplots", xlab="Group", font.main=4, font.lab=1)
plot(g)
plot(x)

library(e1071)
par(mfrow = c(1,2))
boxplot(faithful$eruptions, horizontal = TRUE, col = "red")
boxplot(faithful$waiting, horizontal = TRUE, col = "orange")
skewness(faithful$eruptions)
skewness(faithful$waiting)
