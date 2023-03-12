#Case Study 1: How does a bike-share navigate speedy success?

#initially we need to add some libearies
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(readr)
library(janitor)
library(data.table)

getwd() #by this we can get the path of our file
setwd("C:/Swasthik pr/New folder") #Using this code we can set or working directory

#Once the WD is created we need to import the file
q1_2016 <- read_csv("q1_2016.csv")
q2_2016 <- read_csv("q2_2016.csv")
q3_2016 <- read_csv("q3_2016.csv")
q4_2016 <- read_csv("q4_2016.csv")
q5_2016 <- read_csv("q5_2016.csv")
q6_2016 <- read_csv("q6_2016.csv")


#Once the files are imported we need to check column names and all the column names should be uniform
colnames(q1_2016)
colnames(q2_2016)
colnames(q3_2016)
colnames(q4_2016)
colnames(q5_2016)
colnames(q6_2016)

#using this code we can check the structure of the data
str(q1_2016)
str(q2_2016)
str(q3_2016)
str(q4_2016)
str(q5_2016)
str(q6_2016)


#We need to mutate our columns

q1_2016 = mutate(q1_2016, trip_id = as.character(trip_id)
                 ,usertype = as.character(usertype))
q2_2016 = mutate(q2_2016, trip_id = as.character(trip_id)
                 ,usertype = as.character(usertype))
q3_2016 = mutate(q3_2016, trip_id = as.character(trip_id)
                 ,usertype = as.character(usertype))
q4_2016 = mutate(q4_2016, trip_id = as.character(trip_id)
                 ,usertype = as.character(usertype))
q5_2016 = mutate(q5_2016, trip_id = as.character(trip_id)
                 ,usertype = as.character(usertype))
q6_2016 = mutate(q6_2016, trip_id = as.character(trip_id)
                 ,usertype = as.character(usertype))

#Once all the files are in a good format we can merge our file into a single working file

all_trips <- bind_rows(q1_2016, q2_2016, q3_2016, q4_2016, q5_2016, q6_2016)

table(all_trips$usertype)

#If there is any unappropriate charecter in the column we need to remove it
all_trips = filter(all_trips, usertype == "Customer" & usertype == "Subscriber")

#Once the file is clean the we need to format our date and time column
all_trips$starttime <- as.POSIXct(all_trips$starttime, format="%m/%d/%Y %H: %M")
all_trips$stoptime <- as.POSIXct(all_trips$stoptime, format="%m/%d/%Y %H: %M")

#Using this code we can get the ride_lenght of the users
all_trips$ride_length <- (as.double(difftime(all_trips$stoptime, all_trips$starttime)))/60

all_trips$ride_length <- as.numeric(all_trips$ride_length)

#Using the below code we can seggreagte the date as weekdays, Months, and shft of the day
all_trips$day <- weekdays(all_trips$starttime)
all_trips$month <- months(all_trips$starttime)
all_trips$year <- year(all_trips$starttime)

table(all_trips$day)
table(all_trips$year)
table(all_trips$month)

all_trips <- filter(all_trips, all_trips$ride_length > 0)

table(all_trips$usertype)


##CONDUCT DESCRIPTIVE ANALYSIS

summary(all_trips$ride_length)



avg_trip = mean(all_trips$tripduration) #straight average (total ride length / rides)
median_trip = median(all_trips$tripduration) #midpoint number in the ascending array of ride lengths
max_trip = max(all_trips$tripduration) #longest ride
min_trip = min(all_trips$tripduration) #shortest ride

#then we need to group our data by the usertype
mean_member_casual = aggregate(all_trips$ride_length ~ all_trips$usertype, FUN =  mean)
median_member_casual = aggregate(all_trips$ride_length ~ all_trips$usertype, FUN =  median)
max_member_casual = aggregate(all_trips$ride_length ~ all_trips$usertype, FUN =  max)
min_member_casual = aggregate(all_trips$ride_length ~ all_trips$usertype, FUN =  min)

#then we need to group our data by the shift
mean_shifts = aggregate(all_trips$ride_length ~ all_trips$shift, FUN =  mean)
median_shifts = aggregate(all_trips$ride_length ~ all_trips$shift, FUN =  median)
max_shifts = aggregate(all_trips$ride_length ~ all_trips$shift, FUN =  max)
min_shifts = aggregate(all_trips$ride_length ~ all_trips$shift, FUN =  min)

#then we need to group our data by the day
mean_day = aggregate(all_trips$ride_length ~ all_trips$day, FUN =  mean)
median_day = aggregate(all_trips$ride_length ~ all_trips$day, FUN =  median)
max_day = aggregate(all_trips$ride_length ~ all_trips$day, FUN =  max)
min_day = aggregate(all_trips$ride_length ~ all_trips$day, FUN =  min)

#then we need to group our data by the month
mean_month = aggregate(all_trips$ride_length ~ all_trips$month, FUN =  mean)
median_month = aggregate(all_trips$ride_length ~ all_trips$month, FUN =  median)
max_month = aggregate(all_trips$ride_length ~ all_trips$month, FUN =  max)
min_month = aggregate(all_trips$ride_length ~ all_trips$month, FUN =  min)


str(mean_shifts)
str(median_shifts)
str(max_shifts)
str(min_shifts)



# Prior to plotting the data, we need to order the week.
all_trips$day <- ordered(all_trips$day, levels = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))

#Prior to plotting the data, we need to order the Month of the data in the order that we want it.
table(all_trips$month)
all_trips$month <- ordered(all_trips$month,levels = c("January", "February", "March", "April", "May", "June", "July",
                                                           "August", "September", "October", "November", "December"))

#Prior to plotting the data, we need to order the shift of the day of the data in the order that we want it.
all_trips$shift <- ordered(all_trips$shift,levels = c("MORNING", "AFTERNOON", "EVENING", "April", "NIGHT"))
table(all_trips$shift)

#Total trips by customer type Vs.Shift
all_trips %>%  
  group_by(usertype, shift) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(usertype, shift)  %>% 
  ggplot(aes(x = shift, y = number_of_rides, fill = usertype)) +
  labs(title ="Total trips by customer type Vs.Shift") +
  theme(axis.text.x = element_text(angle = 30)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

#Total trips by customer type Vs. Month
all_trips %>%  
  group_by(usertype, month) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(usertype, month)  %>% 
  ggplot(aes(x = month, y = number_of_rides, fill = usertype)) +
  labs(title ="Total trips by customer type Vs. Month") +
  theme(axis.text.x = element_text(angle = 30)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))



#Total trips by customer type Vs. Week
all_trips %>%  
  group_by(usertype, day) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(usertype, day)  %>% 
  ggplot(aes(x = day, y = number_of_rides, fill = usertype)) +
  labs(title ="Total trips by customer type Vs. Week") +
  theme(axis.text.x = element_text(angle = 30)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))



