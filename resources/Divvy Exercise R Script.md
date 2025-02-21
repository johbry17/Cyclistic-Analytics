library(tidyverse)  \#helps wrangle data  
\# Use the conflicted package to manage conflicts  
library(conflicted)

\# Set dplyr::filter and dplyr::lag as the default choices  
conflict\_prefer("filter", "dplyr")  
conflict\_prefer("lag", "dplyr")

\#=====================  
\# STEP 1: COLLECT DATA  
\#=====================  
\# \# Upload Divvy datasets (csv files) here  
q1\_2019 \<- read\_csv("Divvy\_Trips\_2019\_Q1.csv")  
q1\_2020 \<- read\_csv("Divvy\_Trips\_2020\_Q1.csv")

\#====================================================  
\# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE  
\#====================================================  
\# Compare column names each of the files  
\# While the names don't have to be in the same order, they DO need to match perfectly before we can use a command to join them into one file  
colnames(q1\_2019)  
colnames(q1\_2020)

\# Rename columns  to make them consistent with q1\_2020 (as this will be the supposed going-forward table design for Divvy)

(q1\_2019 \<- rename(q1\_2019  
                   ,ride\_id \= trip\_id  
                   ,rideable\_type \= bikeid  
                   ,started\_at \= start\_time  
                   ,ended\_at \= end\_time  
                   ,start\_station\_name \= from\_station\_name  
                   ,start\_station\_id \= from\_station\_id  
                   ,end\_station\_name \= to\_station\_name  
                   ,end\_station\_id \= to\_station\_id  
                   ,member\_casual \= usertype  
                   ))

\# Inspect the dataframes and look for incongruencies  
str(q1\_2019)  
str(q1\_2020)

\# Convert ride\_id and rideable\_type to character so that they can stack correctly  
q1\_2019 \<-  mutate(q1\_2019, ride\_id \= as.character(ride\_id)  
                   ,rideable\_type \= as.character(rideable\_type)) 

\# Stack individual quarter's data frames into one big data frame  
all\_trips \<- bind\_rows(q1\_2019, q1\_2020)\#, q3\_2019)\#, q4\_2019, q1\_2020)

\# Remove lat, long, birthyear, and gender fields as this data was dropped beginning in 2020  
all\_trips \<- all\_trips %\>%    
  select(-c(start\_lat, start\_lng, end\_lat, end\_lng, birthyear, gender,  "tripduration"))

\#======================================================  
\# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS  
\#======================================================  
\# Inspect the new table that has been created  
colnames(all\_trips)  \#List of column names  
nrow(all\_trips)  \#How many rows are in data frame?  
dim(all\_trips)  \#Dimensions of the data frame?  
head(all\_trips)  \#See the first 6 rows of data frame.  Also tail(all\_trips)  
str(all\_trips)  \#See list of columns and data types (numeric, character, etc)  
summary(all\_trips)  \#Statistical summary of data. Mainly for numerics

\# There are a few problems we will need to fix:  
\# (1) In the "member\_casual" column, there are two names for members ("member" and "Subscriber") and two names for casual riders ("Customer" and "casual"). We will need to consolidate that from four to two labels.  
\# (2) The data can only be aggregated at the ride-level, which is too granular. We will want to add some additional columns of data \-- such as day, month, year \-- that provide additional opportunities to aggregate the data.  
\# (3) We will want to add a calculated field for length of ride since the 2020Q1 data did not have the "tripduration" column. We will add "ride\_length" to the entire dataframe for consistency.  
\# (4) There are some rides where tripduration shows up as negative, including several hundred rides where Divvy took bikes out of circulation for Quality Control reasons. We will want to delete these rides.

\# In the "member\_casual" column, replace "Subscriber" with "member" and "Customer" with "casual"  
\# Before 2020, Divvy used different labels for these two types of riders ... we will want to make our dataframe consistent with their current nomenclature  
\# N.B.: "Level" is a special property of a column that is retained even if a subset does not contain any values from a specific level  
\# Begin by seeing how many observations fall under each usertype  
table(all\_trips$member\_casual)

\# Reassign to the desired values (we will go with the current 2020 labels)  
all\_trips \<-  all\_trips %\>%   
  mutate(member\_casual \= recode(member\_casual  
                                ,"Subscriber" \= "member"  
                                ,"Customer" \= "casual"))

\# Check to make sure the proper number of observations were reassigned  
table(all\_trips$member\_casual)

\# Add columns that list the date, month, day, and year of each ride  
\# This will allow us to aggregate ride data for each month, day, or year ... before completing these operations we could only aggregate at the ride level  
\# https://www.statmethods.net/input/dates.html more on date formats in R found at that link  
all\_trips$date \<- as.Date(all\_trips$started\_at) \#The default format is yyyy-mm-dd  
all\_trips$month \<- format(as.Date(all\_trips$date), "%m")  
all\_trips$day \<- format(as.Date(all\_trips$date), "%d")  
all\_trips$year \<- format(as.Date(all\_trips$date), "%Y")  
all\_trips$day\_of\_week \<- format(as.Date(all\_trips$date), "%A")

\# Add a "ride\_length" calculation to all\_trips (in seconds)  
\# https://stat.ethz.ch/R-manual/R-devel/library/base/html/difftime.html  
all\_trips$ride\_length \<- difftime(all\_trips$ended\_at,all\_trips$started\_at)

\# Inspect the structure of the columns  
str(all\_trips)

\# Convert "ride\_length" from Factor to numeric so we can run calculations on the data  
is.factor(all\_trips$ride\_length)  
all\_trips$ride\_length \<- as.numeric(as.character(all\_trips$ride\_length))  
is.numeric(all\_trips$ride\_length)

\# Remove "bad" data  
\# The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride\_length was negative  
\# We will create a new version of the dataframe (v2) since data is being removed  
\# https://www.datasciencemadesimple.com/delete-or-drop-rows-in-r-with-conditions-2/  
all\_trips\_v2 \<- all\_trips\[\!(all\_trips$start\_station\_name \== "HQ QR" | all\_trips$ride\_length\<0),\]

\#=====================================  
\# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS  
\#=====================================  
\# Descriptive analysis on ride\_length (all figures in seconds)  
mean(all\_trips\_v2$ride\_length) \#straight average (total ride length / rides)  
median(all\_trips\_v2$ride\_length) \#midpoint number in the ascending array of ride lengths  
max(all\_trips\_v2$ride\_length) \#longest ride  
min(all\_trips\_v2$ride\_length) \#shortest ride

\# You can condense the four lines above to one line using summary() on the specific attribute  
summary(all\_trips\_v2$ride\_length)

\# Compare members and casual users  
aggregate(all\_trips\_v2$ride\_length \~ all\_trips\_v2$member\_casual, FUN \= mean)  
aggregate(all\_trips\_v2$ride\_length \~ all\_trips\_v2$member\_casual, FUN \= median)  
aggregate(all\_trips\_v2$ride\_length \~ all\_trips\_v2$member\_casual, FUN \= max)  
aggregate(all\_trips\_v2$ride\_length \~ all\_trips\_v2$member\_casual, FUN \= min)

\# See the average ride time by each day for members vs casual users  
aggregate(all\_trips\_v2$ride\_length \~ all\_trips\_v2$member\_casual \+ all\_trips\_v2$day\_of\_week, FUN \= mean)

\# Notice that the days of the week are out of order. Let's fix that.  
all\_trips\_v2$day\_of\_week \<- ordered(all\_trips\_v2$day\_of\_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

\# Now, let's run the average ride time by each day for members vs casual users  
aggregate(all\_trips\_v2$ride\_length \~ all\_trips\_v2$member\_casual \+ all\_trips\_v2$day\_of\_week, FUN \= mean)

\# analyze ridership data by type and weekday  
all\_trips\_v2 %\>%   
  mutate(weekday \= wday(started\_at, label \= TRUE)) %\>%  \#creates weekday field using wday()  
  group\_by(member\_casual, weekday) %\>%  \#groups by usertype and weekday  
  summarise(number\_of\_rides \= n()							\#calculates the number of rides and average duration   
            ,average\_duration \= mean(ride\_length)) %\>% 		\# calculates the average duration  
  arrange(member\_casual, weekday)								\# sorts

\# Let's visualize the number of rides by rider type  
all\_trips\_v2 %\>%   
  mutate(weekday \= wday(started\_at, label \= TRUE)) %\>%   
  group\_by(member\_casual, weekday) %\>%   
  summarise(number\_of\_rides \= n()  
            ,average\_duration \= mean(ride\_length)) %\>%   
  arrange(member\_casual, weekday)  %\>%   
  ggplot(aes(x \= weekday, y \= number\_of\_rides, fill \= member\_casual)) \+  
  geom\_col(position \= "dodge")

\# Let's create a visualization for average duration  
all\_trips\_v2 %\>%   
  mutate(weekday \= wday(started\_at, label \= TRUE)) %\>%   
  group\_by(member\_casual, weekday) %\>%   
  summarise(number\_of\_rides \= n()  
            ,average\_duration \= mean(ride\_length)) %\>%   
  arrange(member\_casual, weekday)  %\>%   
  ggplot(aes(x \= weekday, y \= average\_duration, fill \= member\_casual)) \+  
  geom\_col(position \= "dodge")

\#=================================================  
\# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS  
\#=================================================  
\# Create a csv file that we will visualize in Excel, Tableau, or my presentation software  
\# N.B.: This file location is for a Mac. If you are working on a PC, change the file location accordingly (most likely "C:\\Users\\YOUR\_USERNAME\\Desktop\\...") to export the data. You can read more here: https://datatofish.com/export-dataframe-to-csv-in-r/  
counts \<- aggregate(all\_trips\_v2$ride\_length \~ all\_trips\_v2$member\_casual \+ all\_trips\_v2$day\_of\_week, FUN \= mean)  
write.csv(counts, file \= 'avg\_ride\_length.csv')

