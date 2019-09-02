###############################################
#SalaryExploration.R
# Souravi Banerjee
#
# GOAL:
# We are going to read salary information of
# faculty members at over 1100 US colleges 
# and learn how to manipulate it.
#
# We have amply COMMENTED the code so that
# beginners will be comfortable with the 
# operations being carried out...
###############################################

# This is the library to compute skew in 
# a supplied dataset. If you have not already 
# done it, please install the e1071 package
library(e1071)

# We have two options to read the data
# Option 1: Read the data directly from a file
# Make sure that UnivSalaries.csv is accessible
# otherwise, the script will not run
#
fac.sal <- read.csv("UnivSalaries.csv",
                    header=FALSE)

# Here's Option 2, which you may uncomment and
# try out if the URL is accessible from your location

# To uncomment/comment many lines at once
# use Ctrl-Shift-C or Cmd-Shift-C

# Read the data from the Internet site
 salaryURL <- url("http://lib.stat.cmu.edu/datasets/colleges/aaup.data")
 fac.sal <- read.csv(salaryURL,
                     header = FALSE)

######################################
# Function to identify whether the
# supplied row contains a *
missingDataInRow <- function(row) {
  
  # This is a flag that is set to
  # FALSE by default
  contains <- FALSE
  
  # Set the flag to TRUE if any cell
  # in the entered row contains a "*"
  # indicating missing information
  for(i in 1:length(row)) {
    if (row[i] == "*") contains <- TRUE
  }
  return(contains)
}

# Missing value treatment: our approach
# is to delete rows with data entry ='*'
fac.sal$missing <- apply(fac.sal,
                         MARGIN=1, # row-wise
                         FUN=missingDataInRow)
  
# This is the CLEANED dataset which 
# we shall work with for the rest of 
# this program
fac.sal.new <- subset(fac.sal, 
                      missing == FALSE)
                      
# Renaming headers for better understanding
# Sal = Salary, Comp = Total Compensation
colnames(fac.sal.new)<-c("Fed_ID No.",
                         "University/College",
                         "State",
                         "Type",
                         "AveSal_Prof",
                         "AveSal_Asc.Prof",
                         "AveSal_Ast.Prof",
                         "AveSal_All",
                         "AveComp_Prof",
                         "AveComp_Asc.Prof",
                         "AveComp_Ast.Prof",
                         "AveComp_All",
                         "Num_Prof",
                         "Num_Asc.Prof",
                         "Num_Ast.Prof",
                         "Num_Instructors",
                         "Num_All",
                         "MissingData")

# Check out the newly cleaned dataset
# by displaying it on a tab
View(fac.sal.new)

# Function to onverte the input data 
# from factor (categorical) to numeric
convertToNum <- function(x) {
  return(as.numeric(as.character(x)))
}

# Convert currency data represented as categorical 
# into numerical
fac.sal.new$AveSal_Prof <- convertToNum(fac.sal.new$AveSal_Prof)
fac.sal.new$AveSal_Asc.Prof <- convertToNum(fac.sal.new$AveSal_Asc.Prof)
fac.sal.new$AveSal_Ast.Prof <- convertToNum(fac.sal.new$AveSal_Ast.Prof)
fac.sal.new$AveComp_Prof <- convertToNum(fac.sal.new$AveComp_Prof)
fac.sal.new$AveComp_Asc.Prof <- convertToNum(fac.sal.new$AveComp_Asc.Prof)
fac.sal.new$AveComp_Ast.Prof <- convertToNum(fac.sal.new$AveComp_Ast.Prof)

# Plotting histograms of salaries/compensation 
# for each faculty position

# Display the plots in 2 rows & 3 columns
# Row 1: Salary Row 2: Compensation
par(mfrow=c(2,3))
hist(fac.sal.new$AveSal_Asc.Prof)
hist(fac.sal.new$AveSal_Prof)
hist(fac.sal.new$AveSal_Ast.Prof)
hist(fac.sal.new$AveComp_Prof)
hist(fac.sal.new$AveComp_Ast.Prof)
hist(fac.sal.new$AveComp_Asc.Prof)

# Display a few summary statistics
cat("Quick summary for Average Compensation of Profs\n")
print(summary(fac.sal.new$AveComp_Prof))
cat("Quick summary for Average Compensation of Associate Profs\n")
print(summary(fac.sal.new$AveComp_Asc.Prof))
cat("\nSkew in the numbers of Assistant Profs: ",
    skewness(fac.sal.new$Num_Ast.Prof), "\n\n")

# Visually check for outliers within 
# the average compendation variable. 
# You may cycle through the remaining 
# variables for practice
par(mfrow=c(1,1))
boxplot(fac.sal.new$AveComp_Prof, 
        horizontal = TRUE)
title(main="Average compensation of Profs across Colleges")

# Calculating which college pays the 
# maximum salary to profs
# The summary function returns a 
# 7-number summary for the dataset
salaryInfo <- summary(fac.sal.new$AveSal_Prof)
maxSal <- salaryInfo[6]

# Figure out which row(s) in the dataset
# carry this maximum salary figure - there 
# could potentially be multiple rows...
index <- which(fac.sal.new$AveSal_Prof == maxSal,
               arr.ind=TRUE)

# The second column in the dataset contains
# the name of the college
maxCollege <- as.character(fac.sal.new[index, 2])

# Output a message
cat(maxCollege, "pays its professors the most\n\n")

# Calculating which college has the 
# least number of Assitant Profs
numAstProfInfo <- summary(fac.sal.new$Num_Ast.Prof)
minProfs <- numAstProfInfo[1]
  
# Figure out which row(s) in the dataset
# carry this minimum number - there 
# could potentially be multiple rows..
index <- which(fac.sal.new$Num_Ast.Prof == minProfs, 
               arr.ind=TRUE)

minColleges <- as.character(fac.sal.new[index, 2])
cat("\nColleges with the minimum number of Asst Profs:\n\n")
print(minColleges)

# Checking whether total number of faculty 
# positions in Purdue Univ.-Main Campus 
# is represented by the column of Faculty-All ranks 
PUIndex <- which(fac.sal.new[ , 2] =="Purdue Univ.-Main Campus",
                 arr.ind=TRUE)

x <- fac.sal.new$Num_All[PUIndex]
y <- fac.sal.new$Num_Prof[PUIndex] +
  fac.sal.new$Num_Asc.Prof[PUIndex] +
  fac.sal.new$Num_Ast.Prof[PUIndex]+
  fac.sal.new$Num_Instructors[PUIndex]

# No.of colleges where AveSal of a Professor 
# rounds up to 80% of the AveComp
fac.sal.new$Percent <- fac.sal.new$AveSal_Prof/fac.sal.new$AveComp_Prof*100
percentComp <- round(fac.sal.new$Percent)

# Which colleges 
index <- which(percentComp == 80, arr.ind=TRUE)
collegesWith80 <- as.character(fac.sal.new[index, 2])
cat("\nColleges with Salary as 80% of Compensation:\n\n")
print(collegesWith80)
