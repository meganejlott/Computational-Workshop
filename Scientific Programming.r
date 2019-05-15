###SCIENTIFIC PROGRAMMING FOR DATA ANALYSIS IN R###
#Megan Lott, megan.lott@uga.edu, Written May 13, 2019


#Step 2: Load Packages
library(ggplot2)

#Step 3: Declare Functions 

#Step 4: Load Data 
setwd("C:/Users/mel13212/Desktop/wnv")
wnv <- read.csv('wnv.csv')

#Step 5: Analysis



#Write a script to load the West Nile virus data and use ggplot to create a histogram 
#for the total number of cases in each state in each year. Follow the format of the prototypical
#script advocated in the presentation: Header, Load Packages, Declare Functions, Load Data,
#Perform Analysis.


#stat = "identity" - go and grab the data frame from the

ggplot(data = wnv, aes(x = reorder(wnv$Year, wnv$Total), y = wnv$Total)) +
  geom_histogram(stat = "identity") +
  facet_wrap(~wnv$State) +
  labs(y = "Total Number of Cases", x = "Year")

#Exercise. The state-level and case burden is evidently highly skewed. Plot a histogram for the
#logarithm of the number of cases. Do this two different ways.

ggplot(data = wnv, aes(x = reorder(wnv$Year, wnv$Total), y = log(wnv$Total))) +
  geom_histogram(stat = "identity") +
  facet_wrap(~wnv$State) +
  labs(y = "Total Number of Cases", x = "Year")

log_Total <- log(wnv$Total)

ggplot(data = wnv, aes(x = reorder(wnv$Year, wnv$Total), y = log_Total)) +
  geom_histogram(stat = "identity") +
  facet_wrap(~wnv$State) +
  labs(y = "Total Number of Cases", x = "Year")


#Exercise. Use arithmetic operators to calculate the raw case fatality rate (CFR) in each state
#in each year. Plot a histogram of the calcated CFRs.

CFR = wnv$Fatal/wnv$Total
ggplot(data = wnv, aes(x = reorder(wnv$Year, CFR), y = CFR)) +
  geom_histogram(stat = "identity") +
  facet_wrap(~wnv$State) +
  labs(y = "Total Number of Cases", x = "Year")

#Exercise. Use arithmetic operators, logical operators, and the function sum to verify that the
#variable Total is simply the sum of the number of febrile cases, neuroinvasive cases, and other
#cases.

sum(wnv$Total != (wnv$EncephMen+wnv$Fever+wnv$Other))


#Exercise. Use modular arithmetic to provide an annual case count for each state rounded
#(down) to the nearest dozen. 

wnv$Rounded <- wnv$Total - wnv$Total%%12

#Use modular arithmetic to extract the rounding errors associated
#with this calculate, then add the errors to obtain the total error.

wnv$Error <- (wnv$Total - wnv$Rounded)
wnv$Percent_Error <- (wnv$Total - wnv$Rounded)/(wnv$Total)*100
sum(wnv$Error)

###FUNCTIONS###

#Exercise. Write a function to calculate the mean and standard error (standard deviation
#divided by the square root of the sample size) of the neuroinvasive disease rate for all the states in a given list and given set of years. 
#Follow the Google R style and remember to place the function near the top of your script. Use your function to calculate the average severe
#disease rate in California, Colorado, and New York.

calculate_ndr <- function(state='Colorado', years=1999:2007){
  
  #Copmutes mean and standard error of neuroinvasive disease rare by state 
  #if the user doesn't specify, the default is 'Colorado' and '1999:2007'
  
  x <- wnv[wnv$State %in% state & wnv$Year %in% years,] #Is the state you typed within the list provided? #Is the year you typed in the list of years? 
  y <- data.frame(state = x$State, ndr = x$EncephMen / x$Total)
  m <- aggregate(y$ndr, by = list(y$state), FUN = mean)
  se <- aggregate(y$ndr, by=list(y$state), FUN = function(x) sd(x)/sqrt(length(x)) )
  out <- merge(m, se, by = 'Group.1')
  names(out) <- c('state', 'mean.ndr', 'se.ndr')
  return(out)
}

disease <- calculate_ndr(state = c('California', 'Colorado', 'New York'))

ggplot(disease, aes(x=state, y=mean.ndr, fill=state)) + 
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin=mean.ndr-se.ndr, ymax = mean.ndr+se.ndr)) + 
  labs(x = 'State', y = 'Neuroinvasive Disease Rate', title = 'Neuroinvasive Disease Rate, 1999-2007')


vState = state.name
alldisease <- calculate_ndr(vState)

ggplot(alldisease, aes(x=state, y=mean.ndr, fill=state)) + 
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin=mean.ndr-se.ndr, ymax = mean.ndr+se.ndr)) + 
  labs(x = 'State', y = 'Neuroinvasive Disease Rate', title = 'Neuroinvasive Disease Rate, 1999-2007')

###PIPES###

install.packages("magrittr")
library(magrittr)


