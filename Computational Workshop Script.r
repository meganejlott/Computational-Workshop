
#Step 2: Load Packages
library(ggplot2)
library(lubridate)

#Step 3: Declare Functions 

#Step 4: Load Data 
mers <- read.csv('cases.csv')

#Step 5: Analysis

mers$onset2 <- ymd(mers$onset)
mers$hospitalized2 <- ymd(mers$hospitalized)
class(mers$onset2)

day0 <- min(na.omit(mers$onset2))

mers$epi.day <- as.numeric(mers$onset2 - day0)

ggplot(data=mers) + 
  geom_bar(mapping=aes(x=epi.day, fill=country)) + 
  labs(x = 'Epidemic Day', y = 'Case Count', title = 'Global Count of MERS Cases by Date of Symptom Onset', 
       caption = "Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv") 

###################################################################################################################
#########################################EXERCISES#################################################################
###################################################################################################################

#Exercise. Copy the MERS data file cases.csv and paste it into your working directory.

#Exercise. Create a new script following the prototype we introduced. Your script should load
#the MERS data and make a plot.

#Exercise. To commit a change, navigate to the “Git” tab in the top right explorer window.
#You will see a list of files in your work directory. Select the files that need to be pushed to
#Github and click on “Commit”. A dialog box will open. In the top right there is an editing
#window where you can register a comment describing the nature of the commit.

