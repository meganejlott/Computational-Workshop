#ECOL Computational Workshop 2019

#Set work directory. Can use the "sessions" option above. 
setwd("C:/Users/mel13212/Desktop/mers")

#Set name of the data set and call into R the data file.
mers <- read.csv('cases.csv')

#inspect the data using head
head(mers)

#edit errors in the data, first by updating date format, then removing line 471
mers$hospitalized[890] <- c('2015-02-20')
mers <- mers [-471,]

#download lubridate package
library(lubridate)

#ymd function takes date time variable and puts it into the form we want 
mers$onset2 <- ymd(mers$onset)
mers$hospitalized2 <- ymd(mers$hospitalized)
class(mers$onset2)

#5 failed to parse - this data set is spotty and may have data that is missing. These dates were not re-formatted. 

day0 <- min(na.omit(mers$onset2))

#Why do we use na.omit? This command is helpful for dealing with NA in the dataframe. Remove objects. Will get an error without this. 

#Make a new data point for "epidemic day" 
mers$epi.day <- as.numeric(mers$onset2 - day0)

#What purpose does as.numeric serve? interpret as numbers 

#Install ggplot package
library(ggplot2)
ggplot(data=mers) + 
  geom_bar(mapping=aes(x=epi.day, fill=country)) + 
  labs(x = 'Epidemic Day', y = 'Case Count', title = 'Global Count of MERS Cases by Date of Symptom Onset', 
         caption = "Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv") +
  coord_polar()

#What if you don't use the "+" function? The "+" will ensure that the next line runs with the one above it. It adds layers to the code.

#calculate the raw infectious period
mers$infectious.period <- mers$hospitalized2-mers$onset2
class(mers$infectious.period)
mers$infectious.period <- as.numeric(mers$infectious.period, units = "days")

ggplot(data=mers) + 
  geom_histogram(aes(x=infectious.period)) + 
  labs(x = 'Infectious Period', y = 'Frequency', title = 'Distribution of Calculated MERS Infectious Period', 
       caption = "Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

mers$infectious.period2 <- ifelse(mers$infectious.period<0, 0, mers$infectious.period)
ggplot(data=mers) + 
  geom_histogram(aes(x=infectious.period2)) + 
  labs(x = 'Infectious Period', y = 'Frequency', title = 'Distribution of Calculated MERS Infectious Period (Positive Values Only)', 
       caption = "Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

ggplot(data=mers) + 
  geom_density(mapping = aes(x=infectious.period2)) + 
  labs(x = 'Infectious Period', y = 'Frequency', title = 'Probability Density of MERS Infectious Period (Positive Values Only)', 
       caption = "Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

ggplot(data=mers) + 
  geom_area(stat = 'bin', mapping = aes(x=infectious.period2)) + 
  labs(x = 'Infectious Period', y = 'Frequency', title = 'Area Plot for MERS Infectious Period (Positive Values Only)', 
       caption = "Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

ggplot(data=mers) + 
  geom_dotplot( mapping = aes(x=infectious.period2)) + 
  labs(x = 'Infectious Period', y = 'Frequency', title = 'Area Plot for MERS Infectious Period (Positive Values Only)', 
       caption = "Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

ggplot(data=mers) + 
  geom_bar(stat = 'bin', mapping = aes(x=infectious.period2)) + 
  labs(x = 'Infectious Period', y = 'Frequency', title = 'Area Plot for MERS Infectious Period (Positive Values Only)', 
       caption = "Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

###BIVARIATE PLOTS###

#Study the change in the infectious period over the course of the MERS epidemic. 
ggplot(data=mers, mapping=aes(x=epi.day, y=infectious.period2))+
  geom_point()

#Add curved fit. Is there evidence of societal learning?
ggplot(data=mers, mapping=aes(x=epi.day, y=infectious.period2))+
  geom_point()+geom_smooth()

#Add curved fit for each country.
ggplot(data=mers, mapping=aes(x=epi.day, y=infectious.period2, fill=country, color=country)) +
  geom_point() + geom_smooth(method='loess')

###FACETING###

#Here, we will vary other features of aethetics. Multi-panel plots will add additional variables. 

#Using facet_wrap(), we will look at subsets of data simultaniously.Looking at data by country.
ggplot(data=mers, mapping=aes(x=epi.day, y=infectious.period2))+
  geom_point(mapping = aes(color=country)) + 
  facet_wrap(~ country) + 
  scale_y_continuous(limits = c(0,50)) +
  labs(x='Epidemic Day', y = 'Infectious Period', title = 'MERS Infectious Period (Positive Values Only) Over Time', caption = "Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#Looking at data by gender.
ggplot(data=subset(mers, gender %in% c('M', 'F') & country %in% c('France', 'Iran', 'Italy', 'Jordan', 'KSA', 'Kuwait', 'Lebanon', 'Oman', 'Qatar', 'South Korea', 'Tunisia', 'UAE', 'UK', 'Yemen')), 
       mapping = aes(x=epi.day, y=infectious.period2)) +
  geom_point(mapping = aes(color=country)) + 
  facet_grid(gender ~ country) + 
  scale_y_continuous(limits = c(0,50)) +
  labs(x='Epidemic Day', y = 'Infectious Period', title = 'MERS Infectious Period by Gender and by Country (Positive Values Only) Over Time', caption = "Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#Study variation in Case Fatality Rate Overtime and Across Countries. 
#The fraction of cases that end in death. Use aggregate function. 

#Exercise. Download and install the ggplot extension. Modify your plot with one or more new
#geoms.

library(plotly)
epi.curve <- ggplot(data=mers) +
  geom_bar(mapping=aes(x=epi.day)) +
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
ggplotly(epi.curve)
