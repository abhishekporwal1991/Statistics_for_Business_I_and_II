########################################################
# Monte Carlo Simulation of Risks in a software setting
# 
# The list of risks and their associated probabilities 
# are contained in RiskList.csv
#
# Shankar Venkatagiri
########################################################

# Read in the list of risks
RiskList <- read.csv("RiskList.csv", header=TRUE)

# Number of risks in the list
numRisks <- nrow(RiskList)

# No. of Rounds for the Monte Carlo simulation 
numRounds <- 1000

# Append columns for risk exposures on schedule and cost
RiskList$ScheduleRE <- RiskList$ScheduleImpact * RiskList$Probability
RiskList$CostRE <- RiskList$CostImpact * RiskList$Probability

# Plot the relative losses versus probability
# for Schedule impact & Cost impact
par(mfrow=c(1,2))

# Square-shaped plots
par(pty="s")

with(RiskList, {
  
  # Calculate three settings for risk exposures
  # along Schedule and Cost factors
  SchedREQuartiles <- quantile(ScheduleRE, 
                               c(0.25, 0.5, 0.75))
  CostREQuartiles <- quantile(CostRE, 
                              c(0.25, 0.5, 0.75))
  
  # Risk Probability versus Schedule RE
  plot(ScheduleImpact, jitter(Probability, amount=0.03),
       ylim=c(min(Probability), max(Probability)),
       pch=18, 
       col="blue",
       xlab="Impact in Calendar Months",
       ylab="Probability (with jitter)",
       main="Risks by Schedule Impact")
    identify(ScheduleImpact, Probability,
             labels=RiskList$RiskCondition,
             cex=0.8)
  
  # Range of x-values for isoquant plot
  SchedX <- seq(min(ScheduleImpact), max(ScheduleImpact),
                length=100)
  
  # Draw the isoquants against the points
  for(n in 1:3) {
    par(new=TRUE)
    plot(SchedX, SchedREQuartiles[n]/SchedX,
         type="l",
         col="navy",
         ylim=c(min(Probability), max(Probability)),
         xaxt="n", yaxt="n",
         ann=FALSE)
  }
  
  par(new=FALSE)
  plot(CostImpact, jitter(Probability, amount=0.03),
       pch=18,
       ylim=c(min(Probability), max(Probability)),
       col="red",
       xlab="Impact in USD '000s",
       ylab="Probability (with jitter)",
       main="Risk Exposure by Cost")
    identify(CostImpact, Probability,
             labels=RiskList$RiskCondition,
             cex=0.8)
  
  CostX <- seq(min(CostImpact), max(CostImpact),
               length=100)
  for(n in 1:3) {
    par(new=TRUE)
    plot(CostX, CostREQuartiles[n]/CostX,
         type="l",
         col="brown",
         ylim=c(min(Probability), max(Probability)),
         xaxt="n", yaxt="n",
         ann=FALSE)
  }
})

# Profile the risk exposures
hist(RiskList$ScheduleRE,
     breaks=10,
     col=colors()[30:40],
     main="Schedule Risk Exposures",
     xlab="Calendar Months")

hist(RiskList$CostRE,
     breaks=10,
     col=colors()[50:60],
     main="Cost Risk Exposures",
     xlab="USD '000s")



# Impact on schedule and cost for numRounds iterations
schedImpact <- c()
costImpact <- c()
for(iter in 1:numRounds) {
  
  # Generate uniform random numbers
  randNums <- runif(numRisks, min=0, max=1)
  
  # Contrast with probabilities to figure out which risks
  # become issues
  issueOrNot <- (randNums < RiskList$Probability)
  
  # With this, calcuate the hit on schedule and cost
  scheduleBuf <- 0
  costBuf <- 0
  for(var in 1:numRisks) {
    
    # Compare the random number with the probability
    if(issueOrNot[var]) {
      scheduleBuf <- scheduleBuf + RiskList[var,]$ScheduleImpact
      costBuf <- costBuf + RiskList[var,]$CostImpact
    }
    schedImpact[iter] <- scheduleBuf
    costImpact[iter] <- costBuf
}
}

hist(schedImpact,
     breaks=20,
     col=colors()[60:80],
     xlab="Stretch in Calendar Months",
     main="Schedule Impact for Simulation")
     
hist(costImpact,
     breaks=20,
     col=colors()[80:100],
     xlab="Stretch in USD '000s",
     main="Cost Impact for Simulation")