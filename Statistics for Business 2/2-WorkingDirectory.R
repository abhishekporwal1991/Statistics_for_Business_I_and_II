#################################################
# R Tutorial 
# Filename: 2-WorkingDirectory.R
# Shankar Venkatagiri
#################################################

# Print the current working directory
cat("The working directory is currently",
    getwd(), "\n")

# Forcefully set the working directory to 
# another location
setwd("..")
cat("Changing the working directory to",
    getwd(), "\n")

# Scan for the file in your system
cat("Please select the file MonthlyCloses.csv by ",
    "navigating to its location: \n")
directory <- dirname(file.choose())

setwd(directory)

# Try to access a file that's not within the 
# directory mentioned here
MonthlyCloses <- read.csv("MonthlyCloses.csv",
                        header=TRUE)

cat("SUCCESSfully read MonthlyCloses.csv\n")
View(MonthlyCloses)
#
# To remedy this problem, you need to set the
# working directory (WD) to where the program
# can find the file. Three ways to do it:
#    1. As an RStudio option
#    2. Using setwd()
#    3. Use the Files tab to browse to where
#       you want, and click on More -> Set WD
#



