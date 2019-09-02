#################################################
# R Tutorial 
# Filename: 4-SourceSink.R
# Shankar Venkatagiri
#################################################

# First, set the proper working directory
# You know how to do it!

# Next, create a sub-directory for the output
dir.create("SinkEx")

# Start recording the output
sink("SinkEx/NumPrimes", append=TRUE)
pdf("SinkEx/Graphs.pdf")

source("1-Data.R")

# Stop recording the output
sink()
dev.off()
print("We are done!")
