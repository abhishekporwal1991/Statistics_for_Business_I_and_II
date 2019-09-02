#################################################
# Filename: BinomialSamples.R
# Shankar Venkatagiri
#################################################

# Read in the population
Population <- read.csv("CensusIncome.csv",
                       header=TRUE)

Managers <- subset(Population, 
                   Occupation == "Exec-managerial")

# Sample parameters
sampleSize <- 234           # using the goal seek tool in excel
numSamples <- 100

# We will just choose the EduLevel column 
# which reflects the level of education
Samples <- data.frame(matrix(nrow=sampleSize,
                             ncol=numSamples))

# Label the columns as Sample 1, Sample 2,...
colnames(Samples) <- paste("Sample",
                           c(1:numSamples))

# What to sample from
EduLevel <- Managers$EduLevel

# For each sample, figure out how many 
# have a College education level
counts <- vector(length=numSamples)

# Build the samples and populate the
# Samples data frame
for(i in 1:numSamples) {
  mySample <- EduLevel[sample(length(EduLevel),
                                   sampleSize)]
  counts[i] <- length(subset(mySample, 
                             mySample == "College"))
  Samples[ , i] <- mySample
}

# Print the number of qualifying samples
numMgrsCollege <- length(subset(counts, 
                                counts >= 100))

cat("# of samples with > 100 managers
    with college degrees =",
    numMgrsCollege, "\n")

