NUmbers = seq(1,10,1)
LogNum = log10(NUmbers)
plot(LogNum)

Pincodes = read.csv("PinCodes.csv", header = TRUE)
str(Pincodes)
Pincodes = Pincodes[,1:2]

which(Pincodes[,1] == "Mannady S.O (Chennai)")
Pincodes[113944,2]

Pincodes[which(Pincodes[,2] == 855108),1]

Leading.digit = substring(Pincodes$pincode,1,1)
Frequency = table(Leading.digit)
sum(Frequency)

proportion = table(Leading.digit)/154797
print(proportion)

barplot(Frequency)
