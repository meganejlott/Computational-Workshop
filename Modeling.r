###MODELING IN R###
#Megan Lott, megan.lott@uga.edu


###MODELING ASSIGNMENT###

library(tidyverse)
library(magrittr)
library(GGally)
library(modelr)

#Task 1: Using either read_csv (note: the underscore version, not read.csv) or load, import the data set
#you put together in the module on 'Data wrangling in R'.

setwd("C:/Users/mel13212/Desktop/Lyme Case Study")
lymedisease <- read_csv("LymeData.csv")


#Task 2: Use the ggpairs function to obtain a 4x4 summary plot of precipitation (prcp), average temperature
#(avtemp), population size (size), number of Lyme disease cases (cases). Note: it may take several seconds for
#this plot to appear as there are nearly 50,000 data points.

#For a data frame df where you're interested in numeric data columns x, y,
#and z, we load the GGally library and issue the command ggpairs(df,columns=c("x","y","z")).

ggpairs(lymedisease,columns=c("prcp","avtemp","size", "Cases"))

#Task 3: Create two new columns for log10(size) and log10(cases+1) and substitute these for the original size
#and cases supplied when you recreate the ggpairs plot. Why do we add 1 to the number of cases?


lymedisease %<>% mutate(log10size = log10(size))
lymedisease %<>% mutate(log10cases = log10(Cases+1))
ggpairs(ld,columns=c("prcp","avtemp","log10size", "log10cases"))

#Log(0) DNE


#Task 4: Using set.seed(222) for reproducibility, create a new data frame to be a random sample (n=100
#rows) of the full data frame and plot precipitation (x-axis) vs average temperature (y-axis).
#You can make use of the dplyr function sample_n.
#Name your ggplot (myPlot <- ggplot...) then call it (plot it) on a separate line (this will make it
#easy to add a subsequent layer to the plot)


set.seed(222)
randomsample <- lymedisease %>% sample_n(100)
myPlot <- ggplot(data = randomsample) +
  geom_point(aes(x = prcp, y = avtemp)) + geom_smooth(aes(x = prcp, y = avtemp), method = 'lm')


#Task 5: Add the best straight line to the plot using geom_smooth.


#Task 6: Create a linear model (lm) object with a call like myModel <- lm(y ~ x, data = myData) for the
#subsetted data, where y=avtemp and x=prcp. In addition, view the summary with a call along the lines of
#summary(myModel)

myModel <- lm(avtemp~prcp, data = randomsample)
summary(myModel) 

#Task 7: What is the slope of the line you plotted in Task 5, and is the slope significantly different from 0
#(p<0.05)?

#slope is 0.00672, p-value: 3.19e-06


#Task 8: Write a single line of code to generate a ggplot of total population size by year.

task8plot <- lymedisease %>% group_by(year) %>% summarize(TotalPop = sum(size)) %>% ggplot(.) + geom_point(aes (year, TotalPop))

#Task 9: Create a data frame called "by_state" from the main data frame, that groups by state, and inspect it.

by_state <- lymedisease %>% group_by(state)

#Task 10: Next, update this new data frame so that it is nested (simply pass it to nest). Again, inspect the
#data frame by typing its name in the console so see how things changed.

by_state %<>% nest

#Task 11: Display the Georgia data in the console window.
by_state$data[[10]]


#Task 12: Write a function that takes a data frame as its argument and returns a linear model object that
#predicts size by year.

linGrowth_model <- function(df){
  predicted_model <- lm(size~year, data = df)
  return(predicted_model)
} 

linGrowth_model(by_state$data[[10]])


#Task 13: Add a column to the by_state dataframe, where each row (state) has its own model object.

install.packages("purrr")
library(purrr)
purrr::map


by_state %<>% mutate(model = map(data, linGrowth_model))


#Task 14: Run these commands and inspect "resids". What is the structure of "resids"?
by_state %<>% mutate(resids = map2(data, model, add_residuals))
by_state$resids[[10]]

#Task 15: Write a function that accepts an object of the type in the resids list, and returns a sum of the
#absolute values, i.e. ignoring sign: abs(3)+abs(-2)=5. Use the function to add a column called totalResid
#to by_state that provides the total size of residuals summed over counties and years.

Sum_Resids <- function(object) { 
  sum(abs(object$resid))  
} 

##MAP is connect column list to a function

by_state %<>% mutate(total_resids = map(resids,Sum_Resids))

#Task 16: Write a function that accepts a linear model and returns the slope (model M has slope
#M$coefficients[2]) and then use this function to create a new column called slope in the by_state data frame, that is the slope for each state.

Slope_FXN <- function(model_input){
  return(model_input$coefficients[2])}

by_state %<>% mutate(slope = map(model,Slope_FXN))


#Task 17: Plot the growth rate (slope value) for all states.
unnest <- by_state %<>% unnest(total_resids)

task17plot <- ggplot(unnest) + geom_point(aes(state, slope)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Task 18: Plot the total resisduals for all states.

task18plot <- ggplot(unnest) + geom_point(aes(state, total_resids)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Task 19: Repeat Tasks 9 and 10 using a different data frame name, by_state2.

by_state2 <- lymedisease %>% group_by(state)
by_state2 %<>% nest


#Task 20: Write a function that accepts an element of the by_state2$data list-column and returns the
#spearman correlation coefficient between Lyme disease cases and precipitation

SpearmanCorrelation <- function(DataElement) { 
 Result <- cor.test(DataElement$Cases, DataElement$prcp, data = DataElement) 
  return(Result)}


###NOTES###

library(GGally)
library(magrittr)
data(cars)
cars %>% ggpairs(columns = c('speed', 'dist'))
#if this function doesn't work, you'll need some code to bring it up to speed.

library(dplyr)
mutate(log10speed = log10(speed))
cars %>% ggpairs(columns = c('speed', 'dist'))

#Randomization and Reproducibility 
x <- tibble(rnorm(10)) %>% print
x %>% sample_n(5) #gives you a subsample of 5 numbers from the sample

#take a number, set.seed, and this will make the randomization reproducable
set.seed(123);x %>% sample_n(5) 

#Linear Modeling
library(ggplot2)
ggplot(cars)+geom_point(aes(speed, dist)) +


summary(lm(speed~dist, data = cars))

#Lists 
y <- list(3.14, "eggs", lm(speed~dist, data = cars)) %>% print
#can mix up different data 

#Correlation Test
cor.test(cars$speed, cars$dist, method = "spearman")


#modelr: create models from data, store modeling information
#group and nest data for analysis, un-nest for visualization 
#store models within the data frame 

