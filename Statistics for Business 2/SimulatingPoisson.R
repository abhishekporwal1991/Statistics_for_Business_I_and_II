lamda = 16
numGen = 400

set.seed(234)
returnCounts = rpois(numGen,lamda)
print(returnCounts)
x = as.data.frame(returnCounts)

table(x)
y = subset(returnCounts, returnCounts > 22)
mean(returnCounts)
var(returnCounts)
