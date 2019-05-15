###DATA WRANGLING###
#Megan Lott, megan.lott@uga.edu
#May 15, 2019 

###DATA WRANGLING ASSIGNMENT###

install.packages("tidyverse")
install.packages("dplyr")
install.packages("stringr")
install.packages("GGally")
install.packages("maptools")
install.packages("ggmap")
install.packages("maps")


library(tidyverse)
library(magrittr)
library(dplyr)
library(stringr)
library(GGally)
library(maptools)
library(ggmap)
library(maps)


setwd("C:/Users/mel13212/Desktop/Lyme Case Study")
ld <- read_csv("lyme.csv")
pop <- read_csv('pop.csv')
prism <- read_csv('climate.csv')

#Task 2: By inspecting the 'pop' data, and talking with your neighbors and instructors, articulate in which
#way(s) these data fail to conform to the tidy data format?
#The pop data summarizes the population data by FIPS

#Task 3: Before running the code, read it to see if you can work out or guess what each line is doing. Before
#running a line of code, remind yourself of the current format of pop by typing it's name in the console window
#and hitting return. Then run each line, one by one, and after each line has been executed, look again at pop
#to see what it did. Once you're clear about what each line is doing, add a comment line above each code line
#to remind you, in your own words, what the code does.

#View Pop in the Console Window

pop %<>% select(fips,starts_with("pop2")) #selects the colums that includes fips and those that are pop2000 and beyond

pop %<>% mutate(year=str_replace_all(str_year,"pop","")) 
#x is where R should look to perform these tasks, y is the patten to be replaced, and z is the replacing pattern


pop %<>% mutate(year=as.integer(year)) #writes all years as integers


pop %<>% mutate(fips=str_replace_all(fips,"^0","")) #removes the placeholder 0 from the beginning of the FIPS


pop %<>% mutate(fips=as.integer(fips)) #makes the FIPS into integers 


#Task 4: Write a code chunk to convert the Lyme disease data to tidy data format.

#You are going to want a column that has the full FIPS codes for each county, as explained above. You
#should write a function that takes two arguments: state code and county code. 

#The CITYCODE will need to be padded with zeros. If there is a one digit city code, add 00. If there is a two digit city code, add 0. If there is a 3 digit city code, add no zeros.


fips_builder <- function(st, ct){ 
    if (str_length(ct) == 3) 
      {paste(as.character(st), as.character(ct), sep="")} 
    else if (str_length(ct) == 2) 
      {paste(as.character(st),"0", as.character(ct), sep="")} 
    else 
    {paste(as.character(st), "00", as.character(ct), sep="")}}


ld%<>% rowwise()%>% mutate(fips= as.integer(fips_builder(STCODE, CTYCODE))) 

ld$fips <- ld %>% rowwise %>% fips_builder(ld$STCODE, ld$CTYCODE)

ld %<>% mutate(fips=as.integer(fips))

#Use 'rename' to rename the columns "STNAME" and "CTYNAME" with "state" and "county",
#respectively (useful for a later join-operation for mapping)
ld%<>%rename(state = STNAME,  county = CTYNAME)

#Task 5: Join the Lyme disease data frame and PRISM data frame together to form a new data frame, and in
#such a way that it retains county-year combinations for which we have both disease and clime data. Use join function.

ld2 <- ld %<>% select(fips,starts_with("Cases"), state, county) 
ld2 %<>% gather(starts_with("Cases"),key="str_year",value="Cases") %>% na.omit
ld2 %<>% mutate(year=str_replace_all(str_year,"Cases",""))
ld2 %<>% mutate(year=as.integer(year))

#Task 6: Write a line of code to additionally combine the demographic data with the Lyme disease and climate
#data.


combined_data <- inner_join(ld2, prism, by = c("fips", "year"))
all_data <- inner_join(combined_data, pop, by = c("fips", "year"))
all_data %<>% select(-c(str_year.x,str_year.y))

#Task 7: Write two lines of code that create two new data frames: (1) to determine how many cases of Lyme
#disease were reported each year. #(2) the average number of cases in each state - averaged across county and
#year. What was the worst year? Which three states have been most impacted on average?

total_cases_by_year <- all_data %>% group_by(year) %>% summarize(SumCases = sum(Cases)) %>% arrange(desc(SumCases))
avg_cases_by_state <- all_data %>% group_by(state) %>% summarize(avgCases = mean(Cases)) %>% arrange(desc(avgCases))

#The worst year was 2009. The three states most impacted on average were CT, MA, DE. 


#How would you show the number of cases in Connecticut in 2009

###geom_tile(state, year color by cases)

#Task 8: use save to create an Rda file of the data frame and use write_csv to create a csv file of the same
#(remember to try ?save and ?write_csv in the console if you're not sure how these functions work).

save(all_data, file = 'LymeData.Rda')
write_csv(LymeData.Rda, file = 'LymeData')

write_csv(all_data, path='LymeData.csv')

#Task 9: Add annotations to the following lines of code with comment lines to explain what they are achieving.
#Note: in the code line with "group_by(ld.prism.pop)" you need to replace the data frame in parentheses with
#the name of your data frame in which you combined Lyme disease, climate and demography data (Task 6)

county_map <- map_data("county") #calling upon data that is already in R
state_map <- map_data("state")
ag.fips <- group_by(all_data,fips) #grouping data from combined data according to fips
ld.16y<-summarize(ag.fips,all.cases=sum(Cases))
ld.16y<-left_join(select(all_data,c(state,county,fips)),ld.16y)
ld.16y<-distinct(ld.16y)
ld.16y %<>% rename(region=state,subregion=county)
ld.16y$subregion<-str_replace_all(ld.16y$subregion," County","")
ld.16y$region<-tolower(ld.16y$region)
ld.16y$subregion<-tolower(ld.16y$subregion)
ld.16y$subregion<-str_replace_all(ld.16y$subregion," parish","")
ld.16y %<>% mutate(log10cases=log10(1+all.cases))
map.ld.16y<-left_join(county_map,ld.16y)
ggplot(map.ld.16y)+geom_polygon(aes(long,lat,group=group,fill=log10cases),color="gray",lwd=0.2) +
  scale_fill_gradientn(colours=rev(heat.colors(10)))



###NOTES###
#You can use read_csv instead of read.csv for tidy R, the data frame is known as a tibble
#To save a data fram save(df, file = 'get_df.Rda') <- this will save the data and title it 
#To recover the file, load('get_df.Rda)
#The file extension .Rda is short for Rdata 
#The TIDY DATA format: 
  #Each variable has its own column 
  #Each observation has its own row
  #Each value has its own cell 
#dplyr package provides these tools: 
  #gather - takes info that is not stored in tidy format and puts it that way
  #select - take particular columns
  #mutate - change select columns and do what you want with them 
#String manipulation: used to clean data sets with different formats: 
  #str_replace_all
  #paste (base R)
#FIPS Codes: numerical code to each county in the US 
  #GA = 13
  #ACC = 59 
  #FIPS = 13059
#Piping is used to a way to take verbs 

#library(magrittr)
#x <- 2 
#x <- x %>% sin %>% sqrt #overwrite data frame using pipes 

#full_join() command is an option to combine data sets, even if they are not complete
#inner_join() command is an option to combine data sets for which the data set is complete

#Grouping data and analyzing aggregate data - can use aggregate command, or summarize command 

