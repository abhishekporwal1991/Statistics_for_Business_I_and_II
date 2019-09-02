# Make sure you have the lubridate
# package installed. Otherwise, do so
# using the Packages tab
library(lubridate)

# Read in the file
if(!exists("Complaints")) {
  
  # Read in the data
  Complaints <- read.csv("Consumer_Complaints.csv",
                         header=TRUE)
  
  # Focus only on these columns
  Complaints <- Complaints[ , c(1, 2, 4, 8, 9, 11, 13, 14)]
  
  # Better column names
  colnames(Complaints) <- c("Date", "Product", "Issue", "Company",
                            "State", "SubmittedVia", 
                            "CompanyResponse", "Timely")
  
  # Eliminate entries with empty issues
  Complaints <- subset(Complaints, Issue != "")
  
  # Eliminate entries with empty state information
  Complaints <- subset(Complaints, State != "")
    
  # Capture the Day, Month and Year
  Complaints$Date <- as.Date(Complaints$Date, "%m/%d/%Y")
  
  Complaints$Day <- day(Complaints$Date)
  Complaints$Month <- month(Complaints$Date)
  Complaints$Year <- sapply(year(Complaints$Date),
                            function(x) 
                              if(x < 100) x + 2000 else x)
  
  # Narrow the focus only to full years
  Complaints <- subset(Complaints, 
                       Year > 2011 & Year < 2015)
}

# Figure out how many complaints by the Year
ComplaintsByYear <- table(Complaints$Year)
barplot(ComplaintsByYear, 
        main="Complaint Counts by Year",
        col=c("darkred", "navy","darkgreen"))

# Figure out complaints by Product category
ComplaintsByProducts <- table(Complaints$Product)

# Arrange them in descending order
ComplaintsByProducts <- 
  ComplaintsByProducts[order(ComplaintsByProducts)]

# Direct view of counts
View(ComplaintsByProducts)

# Now visualise these counts with a 
# horizontal bar plot, with labels

# Make enough margin for the labels
par(mar=c(3,11,3,2))

bp <- barplot(ComplaintsByProducts,
              main="Complaints by Product Category",
              las=1,
              xlim=c(0, 150000),
              horiz=TRUE,
              col=colors()[30:40])

text(x=ComplaintsByProducts + 10000, 
     y=bp, 
     label=ComplaintsByProducts, 
     col = "blue")

prob_Mortgage = 129448/329288
n  = 250

mean_Mortgage = n*prob_Mortgage
variance_MOrtgage = n*prob_Mortgage*(1-prob_Mortgage)
sigma = sqrt(variance_MOrtgage)
LowerLimit = mean_Mortgage-3*sigma
Upperlimit = mean_Mortgage+3*sigma

LowProb = pbinom(75,250,0.393, lower.tail = TRUE)
HigherProb = pbinom(122,250,0.393, lower.tail = TRUE)
quartile = qbinom(0.95,250,0.393)

pbinom(99,250,0.393, lower.tail = FALSE)
